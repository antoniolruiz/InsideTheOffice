library(gsheet)

theoffice_url <- 'docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0'
theoffice_df <- gsheet2tbl(theoffice_url)

library(dplyr)
library(tokenizers)
library(stopwords)
library(stringr)
library(tidytext)
library(tidyr)

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

cleaned_df <- theoffice_df %>% 
  mutate(
    wrylies = get_wrylies(line_text),
    pure_text = remove_wrylies(line_text)
  )

# tok_line_text = tokenize_words(pure_text, stopwords = stopwords("en"))

office_bigrams <- cleaned_df %>%
  unnest_tokens(bigram, pure_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  count(word1, word2, sort = TRUE)

office_trigrams <- cleaned_df %>%
  unnest_tokens(trigram, pure_text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stopwords("en")) %>%
  filter(!word2 %in% stopwords("en")) %>% 
  filter(!word3 %in% stopwords("en")) %>% 
  count(word1, word2, word3, sort = TRUE)


