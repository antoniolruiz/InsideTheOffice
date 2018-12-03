
library(readr)

theoffice_df <- read_csv('the-office-lines - scripts.csv')

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
    remove_replacement_character %>% 
    remove_apostrophes %>% 
    remove_punctuation %>% 
    underscore_common_terms(ct_list) %>% 
    remove_stopwords(sw_list)
  return(cleaned_text)
}

remove_replacement_character <- function(text) gsub('\uFFFD', '', text)
remove_apostrophes <- function(text) gsub("'", '', text)
remove_punctuation <- function(text) gsub('[[:punct:] ]+', ' ', text)

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

count_ngrams <- function(df, var, n) {
  df$var <- df[[var]]
  ngrams <- df %>%
    unnest_tokens(ngram, var, token = "ngrams", n = n) %>%
    count(ngram, sort = TRUE)
  return(ngrams)
}

#### SCRIPT ####

ct_list = list(ct_list = list(
  "dunder mifflin",
  "thats what she said",
  "bob vance vance refrigeration",
  "just dont",
  "david wallace",
  "michael scott",
  "oh god",
  "new york",
  "conference room",
  "dwight schrute",
  "dont even",
  "whats going on",
  "regional manager",
  "jim halpert",
  "andy bernard",
  "mr scott",
  "please dont",
  "high school",
  "valentines day",
  "parking lot",
  "paper company",
  "ice cream",
  "number two",
  "wow thats",
  "good news",
  "sounds good",
  "best friend",
  "costumer service",
  "planning committee",
  "cell phone",
  "cant even",
  "number one",
  "bob vance",
  "robert california",
  "nard dog",
  "hey andy",
  "schrute farms",
  "please stop",
  "hot dogs",
  "scranton branch",
  "just kidding",
  "michael scarn",
  "stanley hudson",
  "god thats",
  "ryan howard",
  "todd packer",
  "attention everyone",
  "michael please",
  "people think",
  "michael michael",
  "jim jim",
  "hey pam",
  "dwight dwight",
  "pam pam",
  "oh yeah",
  "oh thank god",
  "beep beep beep",
  "yes yes yes yes yes",
  "la la la la la",
  "na na na na na",
  "beep beep beep beep beep",
  "go go go go go",
  "blah blah blah blah blah",
  "whoa whoa whoa whoa whoa",
  "stop stop stop stop stop",
  "im pretty sure",
  "guys guys guys",
  "okay okay okay",
  "dont know",
  "oh god",
  "lets get",
  "hey hey hey",
  "whoa whoa whoa",
  "go go go go",
  "dont think",
  "right now",
  "im sorry",
  "im gonna",
  "im going",
  "lets go",
  "nope nope nope nope nope"
  
)

)

sw_list <- stopwords("en") %>% remove_apostrophes()

wrylies_dialogue_df <- theoffice_df %>% 
  mutate(
    wrylies = get_wrylies(line_text),
    dialogue = remove_wrylies(line_text)
  )

dialogues_df <- wrylies_dialogue_df %>% 
  filter(!deleted) %>% 
  select(id, season, episode, scene, speaker, dialogue) %>% 
  mutate(filt_dialogue = clean_text(dialogue, ct_list, sw_list))

# tok_line_text = tokenize_words(dialogue, stopwords = stopwords("en"))

dialogues_2grams <- count_ngrams(dialogues_df, "filt_dialogue", 2)
dialogues_3grams <- count_ngrams(dialogues_df, "filt_dialogue", 3)
dialogues_4grams <- count_ngrams(dialogues_df, "filt_dialogue", 4)
dialogues_5grams <- count_ngrams(dialogues_df, "filt_dialogue", 5)
