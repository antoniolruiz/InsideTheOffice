library(readr)

theoffice_df <- read_csv('the-office-lines - scripts.csv')

library(dplyr)
library(tokenizers)
library(stopwords)
library(stringr)
library(tidytext)
library(tidyr)

#### FUNCTIONS ####

clean_text <- function(text, ct_list){
  cleaned_text <- text %>% 
    tolower %>% 
    remove_apostrophes %>% 
    underscore_common_terms(ct_list)
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

paste_words <- function(words_list, sepa) {
  return(do.call(paste, c(as.list(words_list), sep = sepa)))
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

cleaned_df <- theoffice_df %>% 
  mutate(
    cleaned_text = clean_text(line_text, ct_list),
    wrylies = get_wrylies(cleaned_text),
    dialogue = remove_wrylies(cleaned_text)
  )

# tok_line_text = tokenize_words(dialogue, stopwords = stopwords("en"))

office_bigrams <- cleaned_df %>%
  unnest_tokens(bigram, dialogue, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  count(word1, word2, sort = TRUE)

office_trigrams <- cleaned_df %>%
  unnest_tokens(trigram, dialogue, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  filter(!word3 %in% stopwords("en")) %>% 
  count(word1, word2, word3, sort = TRUE)

office_tetragrams <- cleaned_df %>%
  unnest_tokens(tetragram, dialogue, token = "ngrams", n = 4) %>%
  separate(tetragram, c("word1", "word2", "word3","word4"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  filter(!word3 %in% stopwords("en")) %>% 
  filter(!word4 %in% stopwords("en")) %>% 
  count(word1, word2, word3,word4, sort = TRUE)

office_fivegrams <- cleaned_df %>%
  unnest_tokens(fivegram, dialogue, token = "ngrams", n = 5) %>%
  separate(fivegram, c("word1", "word2", "word3","word4","word5"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  filter(!word3 %in% stopwords("en")) %>% 
  filter(!word4 %in% stopwords("en")) %>% 
  filter(!word5 %in% stopwords("en")) %>% 
  count(word1, word2, word3,word4,word5, sort = TRUE)

