library(dplyr)
library(udpipe)
library(tidytext)
library(quanteda)
library(textreuse)

load_manifesto <- function(path, party){
  tibble::tibble(
    text = readLines(path), 
    party = party
  ) %>%
    mutate(sentence_id = paste0(party, "_", row_number()))
}

ku_df <- load_manifesto("2020/krestanska-unia.md", "KÚ") %>%
  filter(text != "") 
olano_df <- load_manifesto("2020/olano.txt", "OĽaNO") %>%
  filter(text != "")
sas_df <- load_manifesto("2020/sas.txt", "SaS") %>% 
  filter(text != "")
sme_df <- load_manifesto("2020/sme_rodina.txt", "Sme Rodina") %>% 
  filter(text != "")
zl_df <- load_manifesto("2020/za-ludi.txt", "Za ľudí") %>%
  filter(text != "")
ps_df <- load_manifesto("2020/ps-spolu.txt", "PS-Spolu") %>%
  filter(text != "")

# prg_2020 <- pdftools::pdf_text("2020/svk-prg-2020.pdf")
# writeLines(prg_2020, "2020/svk-prg-2020.txt")
prg_2020_txt <- load_manifesto("2020/svk-prg-2020.txt", "GOV")

ud_model <- udpipe_download_model(language = "slovak")
ud_model <- udpipe_load_model(ud_model$file_model)
prg_lemmas <- udpipe_annotate(ud_model, x = prg_2020_txt$text, 
                              doc_id = prg_2020_txt$sentence_id) %>% 
  as.data.frame

party_df <- bind_rows(ku_df, olano_df, sas_df, sme_df, zl_df, ps_df)
party_lemmas <- udpipe_annotate(ud_model, 
                                x = party_df$text, 
                                doc_id = party_df$sentence_id) %>% 
  as.data.frame

## find keywords
keywords_stats <- keywords_rake(x = prg_lemmas, term = "lemma", group = "doc_id", 
                                relevant = prg_lemmas$upos %in% c("NOUN", "ADJ"))
party_lemmas %>%
  mutate(party_id = stringr::str_extract("[A-Ža-ž ]+", doc_id)) %>%
  keywords_rake(x = ., term = "lemma", group = "party_id", 
                relevant = .$upos %in% c("NOUN", "ADJ")) %>%
  filter(keyword %in% keywords_stats$keyword) -> keyword_parties

keywords <- keyword_parties %>% 
  filter(freq <= 10) %>%
  pull(keyword)

keywords <- c("goldplating", "rodinný dom", "konšpirácia", "marginalizovaný komunita", 
              "zdravotnícky asistent", "medzinárodný organizácia", "domáci uhľ", "kritický infraštruktúra", 
              "európsky komisia", "aktívny občan", "štátny les", "hodnota za peniaze", "predškolský vek", 
              "Pisa", "krajský súd", "protipovodňový opatrenie", "prvý pilier", "platobný mechanizmus", 
              "celoživotný vzdelávanie", "mimovládny organizácia", "Rtvs", "Ústavný súd", "profesionálny vojak", "násilia", 
              "asignácia", "Policajný zbor", "rusínský jazyk", "nájomný byt", "územný plán", "úrad pre slovák", 
              "Matica slovenský", "kultúrny priemysel", "knižnica", "národný kultúrny pamiatka", 
              "podkladový materiál")

party_lemmas %>%
  group_by(doc_id, paragraph_id, sentence_id, sentence) %>%
  summarise(lemmatized_sentence = paste0(lemma, collapse = " ")) -> party_sentences

purrr::map(party_sentences$lemmatized_sentence, function(x) 
  purrr::map_lgl(keywords, function(y) grepl(y, x))) %>%
  purrr::map(., which) %>% 
  purrr::map(., function(x) keywords[x]) -> which_keywords
party_sentences$keyword <- which_keywords
party_sentences$n_keywords <- purrr::map_int(which_keywords, length)

prg_lemmas %>%
  group_by(doc_id) %>%
  summarise(lemmatized_sentence = paste0(lemma, collapse = " ")) %>%
  left_join(prg_2020_txt, ., by = c("sentence_id"="doc_id")) -> prg_sentences

purrr::map(prg_sentences$lemmatized_sentence, function(x) 
  purrr::map_lgl(keywords, function(y) grepl(y, x))) %>%
  purrr::map(., which) %>% 
  purrr::map(., function(x) keywords[x]) -> which_keyword
prg_sentences$keyword <- which_keyword
prg_sentences$n_keywords <- purrr::map_int(which_keyword, length)
purrr::map2_int(prg_sentences$keyword, prg_sentences$lemmatized_sentence, 
                function(x, y) 
  if(length(x) > 0){
    kword <- textreuse::tokenize_words(x[1], lowercase = FALSE)[1]
    sentence_words <- textreuse::tokenize_words(y, lowercase = FALSE)
    which(grepl(kword, sentence_words))[1]
  }else{
    NA_integer_
  }) -> prg_sentences$keyword_start
prg_sentences$found_keyword <- purrr::map_chr(1:nrow(prg_sentences), function(x) {
  start <- prg_sentences$keyword_start[x]
  text <- prg_sentences$text[x]
  keyword <- prg_sentences$keyword[x][[1]][1]
  if(!is.na(start)){
    n_words <- length(textreuse::tokenize_words(keyword, lowercase = FALSE))
    words <- textreuse::tokenize_words(text, lowercase = FALSE)
    paste0(words[start:(start + n_words - 1)], collapse = " ")  
  }else{
    NA_character_
  }
})

party_keyword_occurrences <- party_sentences[rep(1:nrow(party_sentences), times = party_sentences$n_keywords), ] %>%
  group_by(doc_id, sentence) %>%
  mutate(n_row = row_number()) %>%
  mutate(found_keyword = purrr::map2_chr(keyword, n_row, function(x, y) x[y])) %>% 
  ungroup %>% 
  mutate(party_id = gsub("_", "", stringr::str_extract(doc_id, "[A-Ža-ž ]+"))) %>% 
  mutate(related_text = paste0("__", party_id, "__: ", sentence)) %>% 
  group_by(found_keyword) %>%
  summarise(related_text = paste0(related_text, collapse = "\n"))
  
# saveRDS(prg_sentences, "prg_sentences_2020.RData")

prg_sentences %>%
  filter(text != "") %>%
  select(sentence_id, text, lemmatized_sentence) -> prg_sentences_clean

party_sentences %>%
  ungroup %>%
  filter(!grepl("#", sentence)) %>%
  select(doc_id, sentence_id, manifesto_text = sentence, manifesto_text_lemma = lemmatized_sentence) %>%
  mutate(doc_id = paste0(doc_id, "_", sentence_id)) %>% 
  select(-sentence_id) -> party_sentences_clean

olano_sentences <- party_sentences_clean %>%
  filter(grepl("OĽaNO", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)

ku_sentences <- party_sentences_clean %>%
  filter(grepl("KÚ", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)

sas_sentences <- party_sentences_clean %>%
  filter(grepl("SaS", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)

sme_sentences <- party_sentences_clean %>%
  filter(grepl("Sme", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)

zl_sentences <- party_sentences_clean %>%
  filter(grepl("Za", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)

ps_sentences <- party_sentences_clean %>%
  filter(grepl("PS", doc_id)) %>%
  mutate(n_char = purrr::map_int(manifesto_text, nchar)) %>%
  filter(n_char > 10)


# calculate_local_align <- function(a, b){
#   align_local(a, b)
# }
# 
# possibly_calculate_local_align <- purrr::possibly(calculate_local_align, 
#                                                   list(a_edits = NA, b_edits = NA, score = 0))
# 
# calc_overlap <- function(x, y){
#   length(intersect(textreuse::tokenize_words(x), textreuse::tokenize_words(y)))
# }

calc_max_similarity <- function(party_sentences, prg_sentences){
  party_dfm <- dfm(corpus(party_sentences$manifesto_text_lemma))
  prg_dfm <- dfm(corpus(prg_sentences$lemmatized_sentence))
  party_prg_simil <- textstat_simil(party_dfm, prg_dfm, margin = "documents", method = "jaccard")
  party_prg_matrix <- as.matrix(party_prg_simil)
  purrr::map_df(1:nrow(party_prg_matrix), function(x) {
    which_max <- which.max(party_prg_matrix[x, ])
    tibble::tibble(
      party_sentence_id = party_sentences$doc_id[x], 
      gov_sentence_id = prg_sentences$sentence_id[which_max], 
      max_simil = party_prg_matrix[x, which_max]  
    )
  })
}

# TODO: napsat to ne jako prase
ku_simil <- calc_max_similarity(ku_sentences, prg_sentences_clean)

ol_simil1 <- calc_max_similarity(olano_sentences[1:300, ], prg_sentences_clean)
ol_simil2 <- calc_max_similarity(olano_sentences[301:600, ], prg_sentences_clean)
ol_simil3 <- calc_max_similarity(olano_sentences[601:900, ], prg_sentences_clean)
ol_simil4 <- calc_max_similarity(olano_sentences[901:1200, ], prg_sentences_clean)
ol_simil5 <- calc_max_similarity(olano_sentences[1201:1500, ], prg_sentences_clean)
ol_simil6 <- calc_max_similarity(olano_sentences[1501:1800, ], prg_sentences_clean)
ol_simil7 <- calc_max_similarity(olano_sentences[1801:2100, ], prg_sentences_clean)
ol_simil8 <- calc_max_similarity(olano_sentences[2101:2400, ], prg_sentences_clean)
ol_simil9 <- calc_max_similarity(olano_sentences[2401:nrow(olano_sentences), ], prg_sentences_clean)

sme_simil1 <- calc_max_similarity(sme_sentences[1:300, ], prg_sentences_clean)
sme_simil2 <- calc_max_similarity(sme_sentences[301:600, ], prg_sentences_clean)
sme_simil3 <- calc_max_similarity(sme_sentences[601:900, ], prg_sentences_clean)
sme_simil4 <- calc_max_similarity(sme_sentences[901:1200, ], prg_sentences_clean)
sme_simil5 <- calc_max_similarity(sme_sentences[1201:1500, ], prg_sentences_clean)
sme_simil6 <- calc_max_similarity(sme_sentences[1501:1800, ], prg_sentences_clean)
sme_simil7 <- calc_max_similarity(sme_sentences[1801:2100, ], prg_sentences_clean)
sme_simil8 <- calc_max_similarity(sme_sentences[2101:nrow(sme_sentences), ], prg_sentences_clean)

zl_simil1 <- calc_max_similarity(zl_sentences[1:300, ], prg_sentences_clean)
zl_simil2 <- calc_max_similarity(zl_sentences[301:600, ], prg_sentences_clean)
zl_simil3 <- calc_max_similarity(zl_sentences[601:900, ], prg_sentences_clean)
zl_simil4 <- calc_max_similarity(zl_sentences[901:1200, ], prg_sentences_clean)
zl_simil5 <- calc_max_similarity(zl_sentences[1201:1500, ], prg_sentences_clean)
zl_simil6 <- calc_max_similarity(zl_sentences[1501:1800, ], prg_sentences_clean)
zl_simil7 <- calc_max_similarity(zl_sentences[1801:2100, ], prg_sentences_clean)
zl_simil8 <- calc_max_similarity(zl_sentences[2101:nrow(sme_sentences), ], prg_sentences_clean)

sas_simil1 <- calc_max_similarity(sas_sentences[1:300, ], prg_sentences_clean)
sas_simil2 <- calc_max_similarity(sas_sentences[301:600, ], prg_sentences_clean)
sas_simil3 <- calc_max_similarity(sas_sentences[601:900, ], prg_sentences_clean)
sas_simil4 <- calc_max_similarity(sas_sentences[901:1200, ], prg_sentences_clean)
sas_simil5 <- calc_max_similarity(sas_sentences[1201:1500, ], prg_sentences_clean)
sas_simil6 <- calc_max_similarity(sas_sentences[1501:1800, ], prg_sentences_clean)
sas_simil7 <- calc_max_similarity(sas_sentences[1801:2100, ], prg_sentences_clean)
sas_simil8 <- calc_max_similarity(sas_sentences[2101:2400, ], prg_sentences_clean)
sas_simil9 <- calc_max_similarity(sas_sentences[2401:2700, ], prg_sentences_clean)
sas_simil10 <- calc_max_similarity(sas_sentences[2701:3000, ], prg_sentences_clean)
sas_simil11 <- calc_max_similarity(sas_sentences[3001:3300, ], prg_sentences_clean)
sas_simil12 <- calc_max_similarity(sas_sentences[3301:3600, ], prg_sentences_clean)
sas_simil13 <- calc_max_similarity(sas_sentences[3901:4200, ], prg_sentences_clean)
sas_simil14 <- calc_max_similarity(sas_sentences[4201:4500, ], prg_sentences_clean)
sas_simil15 <- calc_max_similarity(sas_sentences[4501:4800, ], prg_sentences_clean)
sas_simil16 <- calc_max_similarity(sas_sentences[4801:5100, ], prg_sentences_clean)
sas_simil17 <- calc_max_similarity(sas_sentences[5101:5400, ], prg_sentences_clean)
sas_simil18 <- calc_max_similarity(sas_sentences[5401:5700, ], prg_sentences_clean)
sas_simil19 <- calc_max_similarity(sas_sentences[5701:6000, ], prg_sentences_clean)
sas_simil20 <- calc_max_similarity(sas_sentences[6001:6300, ], prg_sentences_clean)
sas_simil21 <- calc_max_similarity(sas_sentences[6301:6600, ], prg_sentences_clean)
sas_simil22 <- calc_max_similarity(sas_sentences[6601:6900, ], prg_sentences_clean)
sas_simil23 <- calc_max_similarity(sas_sentences[6901:7200, ], prg_sentences_clean)
sas_simil24 <- calc_max_similarity(sas_sentences[7201:7500, ], prg_sentences_clean)
sas_simil25 <- calc_max_similarity(sas_sentences[7501:7800, ], prg_sentences_clean)
sas_simil26 <- calc_max_similarity(sas_sentences[7801:nrow(sas_sentences), ], prg_sentences_clean)

ps_simil1 <- calc_max_similarity(ps_sentences[1:300, ], prg_sentences_clean)
ps_simil2 <- calc_max_similarity(ps_sentences[301:600, ], prg_sentences_clean)
ps_simil3 <- calc_max_similarity(ps_sentences[601:900, ], prg_sentences_clean)
ps_simil4 <- calc_max_similarity(ps_sentences[901:1200, ], prg_sentences_clean)
ps_simil5 <- calc_max_similarity(ps_sentences[1201:1500, ], prg_sentences_clean)
ps_simil6 <- calc_max_similarity(ps_sentences[1501:1800, ], prg_sentences_clean)
ps_simil7 <- calc_max_similarity(ps_sentences[1801:2100, ], prg_sentences_clean)
ps_simil8 <- calc_max_similarity(ps_sentences[2101:2400, ], prg_sentences_clean)
ps_simil9 <- calc_max_similarity(ps_sentences[2401:2700, ], prg_sentences_clean)

olano_simil <- bind_rows(ol_simil1, ol_simil2, ol_simil3, ol_simil4, ol_simil5, ol_simil6, 
                       ol_simil7, ol_simil8, ol_simil9)
sme_simil <- bind_rows(sme_simil1, sme_simil2, sme_simil3, sme_simil4, sme_simil5, 
                     sme_simil6, sme_simil7, sme_simil8)
zl_simil <- bind_rows(zl_simil1, zl_simil2, zl_simil3, zl_simil4, 
                    zl_simil5, zl_simil6, zl_simil7, zl_simil8)
sas_simil <- bind_rows(sas_simil1, sas_simil2, sas_simil3, sas_simil4, sas_simil5, sas_simil6,
                     sas_simil7, sas_simil8, sas_simil9, sas_simil10, sas_simil11, sas_simil12, 
                     sas_simil13, sas_simil14, sas_simil15, sas_simil16, sas_simil17, sas_simil18, 
                     sas_simil19, sas_simil20, sas_simil21, sas_simil22, sas_simil23, sas_simil24, 
                     sas_simil25, sas_simil26)
ps_simil <- bind_rows(ps_simil1, ps_simil2, ps_simil3, ps_simil4, ps_simil5, 
                      ps_simil6, ps_simil7, ps_simil8, ps_simil9)

all_simil <- bind_rows(ku_simil, olano_simil, sme_simil, zl_simil, sas_simil, ps_simil)
all_simil %>% 
  group_by(gov_sentence_id) %>%
  arrange(desc(max_simil)) %>%
  filter(row_number() == 1) %>%
  filter(max_simil >= 0.5) -> max_prg_sentence

prg_sentences %>% 
  left_join(., max_prg_sentence, by = c("sentence_id"="gov_sentence_id")) %>% 
  left_join(., party_sentences_clean, by = c("party_sentence_id"="doc_id")) %>%
  mutate(party_id = gsub("_", "", stringr::str_extract(party_sentence_id, "[A-Ža-ž ]+"))) %>%
  mutate(found_keyword2 = purrr::map_chr(keyword, function(x) x[1])) %>%
  left_join(., party_keyword_occurrences, by = c("found_keyword2"="found_keyword")) %>%
  mutate(found_keyword = gsub("\\(", "", found_keyword)) -> prg_sentence_all

saveRDS(prg_sentence_all, "prg_sentences_all_2020.RData")
