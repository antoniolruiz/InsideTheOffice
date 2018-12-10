
library(gsheet)

theoffice_url <- 'docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0'
theoffice_df <- gsheet2tbl(theoffice_url)

library(dplyr)
library(tokenizers)
library(stopwords)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(forcats)

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



# Antonio
# 2,7


MyPalette <- c(andy = '#e6194b', angela = '#fabebe', darryl = '#ffe119', dwight =  '#3cb44b', erin = '#f58231', jan =  '#911eb4',
               jim = '#46f0f0', kelly = '#f032e6', kevin = '#bcf60c', michael = '#4363d8', oscar = '#008080', pam = '#e6beff',
               phyllis ='#9a6324',roy = '#fffac8',ryan = '#ffd8b1', stanley= '#808080', toby = '#000000')


# 2) Characters that are mentioned the most

replace_characters <- function(text) {
  text <- gsub("michael_scott", "michael", text)
  text <- gsub("dwight_schrute", "dwight", text)
  text <- gsub("jim_halpert", "jim", text)
  text <- gsub("andy_bernard", "andy", text)
  text <- gsub("david_wallace", "david", text)
  text <- gsub("robert_california", "robert", text)
  text <- gsub("ryan_howard", "ryan", text)
  text <- gsub("todd_packer", "todd", text)
  text <- gsub("stanley_hudson", "dwight", text)
  text <- gsub("bob_vance", "bob", text)
  text <- gsub("mr_scott", "michael", text)
  return(text)
}

main_and_sec_chars <- dialogues_df %>% 
  group_by(speaker) %>% 
  summarise(Freq = n()) %>% 
  arrange(-Freq) %>% 
  top_n(30) %>% 
  .$speaker

interact_df <- words_df %>% 
  mutate(word = replace_characters(word)) %>% 
  filter(word %in% main_and_sec_chars)

get_interactions_by_char <- function(char, interact_df) {
  interact_char <- interact_df %>% 
    filter(speaker == char)
  interactions <- table(interact_char$word) %>% 
    as.data.frame() %>% 
    arrange(-Freq)
  return(interactions)
}


interactions <- get_interactions_by_char("kelly", interact_df)
aux_i <- interactions[order(-interactions$Freq),] 
top_i <- head(aux_i,10)
upper_lim <- as.numeric(top_i$Freq[1]) +10


get_graph <- function(character)
  {
  title <- paste("Interactions of",character)
  
  interactions <- get_interactions_by_char(character, interact_df)
  aux_i <- interactions[order(-interactions$Freq),] 
  top_i <- head(aux_i,10)
  upper_lim <- as.numeric(top_i$Freq[1]) +10
  
  ggplot(data = top_i, aes(x = fct_reorder(Var1,Freq,.desc = TRUE), y = Freq, fill= Var1)) +
    geom_bar(stat = 'identity',colour='black') + 
    scale_colour_manual(values = MyPalette, aesthetics = "fill") +
    labs(title=title) +
    labs(x="Character", y="Count") + 
    ylim(c(0,upper_lim)) 
}

get_graph("kelly")
# 5) Who uses the most crutch words?


crutches <- words_df
crutches$crutch <- crutches$word %in% stopwords()
crutches <- crutches %>% 
  mutate(crutch = ifelse(crutch == TRUE,1,0))

count_c <- crutches %>%
  select(speaker,crutch) %>% 
  group_by(speaker) %>% 
  summarize(freq = n())

#View(count_c)
#colnames(count_c)

aux_c <- count_c[order(-count_c$freq),] 
top_c <- head(aux_c,15)
top_c$freq <- top_c$freq/1000


ggplot(data = top_c, aes(x = fct_reorder(speaker,freq,.desc = TRUE), y = freq, fill= speaker)) +
  geom_bar(stat = 'identity',colour='black') + 
  scale_colour_manual(values = MyPalette, aesthetics = "fill") +
  labs(title="Most crutches by character (in thousands)") +
  labs(x="Character", y="Count") + 
  ylim(c(0,160)) 

#colnames(top_c)
#View(top_c)

  
# 6) Who got cut out? Who lost the most scenes due to the editors?
deleted_scenes <- theoffice_df %>% 
  filter(deleted == TRUE) %>% 
  select(speaker,season) %>% 
  group_by(speaker) %>% #,season
  summarize(freq = n())

deleted_scenes$speaker <- tolower(deleted_scenes$speaker)
aux_ds <- deleted_scenes[order(-deleted_scenes$freq),] 
top_deleted <- head(aux_ds,15)

#ratings$Season <- as.factor(ratings$Season)
ggplot(data = top_deleted, aes(x = fct_reorder(speaker,freq,.desc = TRUE), y = freq,fill= speaker)) +
  geom_bar(stat = 'identity',colour = 'black') + 
  scale_colour_manual(values = MyPalette,aesthetics = "fill") +
  labs(title="Most scenes deleted by character") +
  labs(x="Character", y="Count") + 
  ylim(c(0,600)) 

#colnames(top_deleted)
#View(top_deleted)


# 10) As the show progressed, how did ratings change?

mydir <- "C:/Users/ganto/OneDrive/Documents/GitHub/InsideTheOffice/Cubicles/Antonio's Office"
surfdir <- "C:/Users/Antonio/Documents/GitHub/InsideTheOffice/Cubicles/Antonio's Office"

setwd(mydir)
viewers <- read.csv('The_office_viewers.csv')

ratings <- viewers %>%  
  select(colnames(viewers)[1],Episode.overall,Episode,U.S..viewers..millions.) 

colnames(ratings) <- c('Season','Episode_overall','Episode','Viewers')

ratings$Season <- as.factor(ratings$Season)
ggplot(data = ratings, aes(x = Episode_overall, y = Viewers, fill = Season)) +
  geom_bar(stat = 'identity',colour = "black") + 
#  facet_grid(~Season) +
  labs(title="US viewers per episode (in millions)") +
  labs(x="Episode", y="Viewers") + 
  xlim(c(0,205)) +
  ylim(c(0,25)) 


  

