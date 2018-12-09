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
clean_names <- function(text) {
  text <- gsub("David Wallace", "David", text)
  text <- gsub("Deangelo", "DeAngelo", text)
  text <- gsub("Todd Packer", "Todd", text)
  text <- gsub("Packer", "Todd", text)
  text <- gsub("Daryl", "Darryl", text)
  text <- gsub("Robert California", "Robert", text)
  text <- tolower(text)
  return(text)
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

clean_text <- function(text, ct_list){
  cleaned_text <- text %>% 
    tolower %>% 
    remove_replacement_character %>% 
    remove_apostrophes %>% 
    remove_punctuation %>% 
    underscore_common_terms(ct_list)
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

count_ngrams <- function(df, var, n) {
  df$var <- df[[var]]
  ngrams <- df %>%
    unnest_tokens(ngram, var, token = "ngrams", n = n) %>%
    count(ngram, sort = TRUE)
  return(ngrams)
}

#### SCRIPT ####

ct_list = list(
  "scranton strangler",
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
  "regional manager",
  "jim halpert",
  "andy bernard",
  "mr scott",
  "high school",
  "valentines day",
  "parking lot",
  "paper company",
  "ice cream",
  "number two",
  "good news",
  "sounds good",
  "best friend",
  "costumer service",
  "planning committee",
  "cell phone",
  "number one",
  "bob vance",
  "robert california",
  "nard dog",
  "schrute farms",
  "hot dogs",
  "scranton branch",
  "michael scarn",
  "stanley hudson",
  "ryan howard",
  "todd packer",
  "hey pam",
  "hey hey hey",
  "dwight dwight",
  "oh yeah",
  "im sorry"
)

wrylies_dialogue_df <- theoffice_df %>% 
  mutate(
    speaker = clean_names(speaker),
    wrylies = get_wrylies(line_text),
    dialogue = remove_wrylies(line_text)
  )

dialogues_df <- wrylies_dialogue_df %>% 
  filter(!deleted) %>% 
  select(id, season, episode, scene, speaker, dialogue) %>% 
  mutate(filt_dialogue = clean_text(dialogue, ct_list))

# dialogues_2grams <- count_ngrams(dialogues_df, "filt_dialogue", 2)
# dialogues_3grams <- count_ngrams(dialogues_df, "filt_dialogue", 3)
# dialogues_4grams <- count_ngrams(dialogues_df, "filt_dialogue", 4)
# dialogues_5grams <- count_ngrams(dialogues_df, "filt_dialogue", 5)

words_df <- dialogues_df %>% 
  separate_rows(., filt_dialogue, sep = " ", convert = TRUE) %>% 
  rename(word = filt_dialogue) %>% 
  filter(word != "")

mydir <- "C:/Users/ganto/OneDrive/Documents/GitHub/InsideTheOffice/Cubicles/Antonio's Office"
setwd(mydir)
viewers <- read.csv('The_office_viewers.csv')

5) Who uses the most crutch words?
  Antonio
 
6) Who got cut out? Who lost the most scenes due to the editors?
  Antonio

10) As the show progressed, how did ratings change?
  Antonio

