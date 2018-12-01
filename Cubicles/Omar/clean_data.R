library(gsheet)

theoffice_url <- 'docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0'
theoffice_df <- gsheet2tbl(theoffice_url)

library(dplyr)
library(tokenizers)
library(stopwords)
library(stringr)
library(tidytext)
library(tidyr)

#### FUNCTIONS ####

clean_text <- function(text, ct_list, sw_list){
  cleaned_text <- text %>% 
    tolower %>% 
    remove_apostrophes %>% 
    underscore_common_terms(ct_list) %>% 
    remove_stopwords(sw_list)
  return(cleaned_text)
}

remove_apostrophes <- function(text) gsub("'", '', text)

underscore_common_terms <- function(text, ct_list) {
  for (ct in ct_list){
    text <- underscore_common_term(text, ct)
  }
  return(text)
}

underscore_common_term <- function(text, common_term) {
  underscored_ct <- common_term %>% 
    strsplit(split = " ") %>% unlist %>% 
    paste_words(., "_")
  underscored_text <- gsub(common_term, underscored_ct, text)
  return(underscored_text)
}

paste_words <- function(words_list, sep) {
  return(do.call(paste, c(as.list(words_list), sep = sep)))
}

remove_stopwords <- function(text, sw_list) {
  text_wo_sw <- lapply(unlist(text), remove_stopwords_by_line, sw_list = sw_list)
  return(unlist(text_wo_sw))
}

remove_stopwords_by_line <- function(line, sw_list) {
  words <- unlist(strsplit(line, " "))
  non_stop_words <- words[!words %in% sw_list]
  if (length(non_stop_words) == 0) {
    non_stop_words <- " "
  }
  non_stop_line <- paste_words(non_stop_words, " ")
  return(non_stop_line)
}

get_wrylies <- function(line) {
  wrylies <- line %>% 
    str_extract_all("\\[(.*?)\\]") %>% 
    gsub("\\[|\\]", "", .)
  wrylies <- ifelse(wrylies == "character(0)", NA, wrylies) 
  return(wrylies)
}

remove_wrylies <- function(line) {
  return(gsub("\\[.*?\\]", "", line))
}

#### SCRIPT ####

ct_list = list(
  "dunder mifflin",
  "thats what she said"
)

sw_list <- stopwords("en")

cleaned_df <- theoffice_df %>% 
  mutate(
    cleaned_text = clean_text(line_text, ct_list, sw_list),
    wrylies = get_wrylies(cleaned_text),
    dialogue = remove_wrylies(cleaned_text)
  )

# tok_line_text = tokenize_words(dialogue, stopwords = stopwords("en"))

office_bigrams <- cleaned_df %>%
  unnest_tokens(bigram, dialogue, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

office_trigrams <- cleaned_df %>%
  unnest_tokens(trigram, dialogue, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE)
