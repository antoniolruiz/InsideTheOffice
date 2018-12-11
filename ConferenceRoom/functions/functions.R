get_data <- function(){
  
  theoffice_url <- 'https://docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0/edit#gid=747974534'
  theoffice_df <- gsheet2tbl(theoffice_url)
  
  return (theoffice_df)
}

get_libraries <- function(){
  
  library(forcats)
  library(gsheet)
  library(tidyverse)
  library(stopwords)
  library(gridExtra)
  library(dplyr)
  library(tokenizers)
  library(stringr)
  library(tidytext)
  library(tidyr)
  library(ggplot2)

  
}

get_palette <- function(){
  
  MyPalette <- c(andy = '#e6194b', 
                 angela = '#fabebe', 
                 darryl = '#ffe119', 
                 dwight =  '#3cb44b', 
                 erin = '#f58231', 
                 jan =  '#911eb4',
                 jim = '#46f0f0', 
                 kelly = '#f032e6', 
                 kevin = '#bcf60c', 
                 michael = '#4363d8', 
                 oscar = '#008080', 
                 pam = '#e6beff',
                 phyllis ='#9a6324',
                 roy = '#fffac8',
                 ryan = '#ffd8b1', 
                 stanley= '#808080', 
                 toby = '#000000')
  
  return (MyPalette)
}

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

remove_replacement_character <- function(text) gsub('\uFFFD', '', text)
remove_apostrophes <- function(text) gsub("'", '', text)
remove_punctuation <- function(text) gsub('[[:punct:] ]+', ' ', text)



get_vars <-  function(theoffice_df){
  

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
    mutate(filt_dialogue = clean_text(dialogue, ct_list), dialogue_length = nchar(filt_dialogue))
  
  # dialogues_2grams <- count_ngrams(dialogues_df, "filt_dialogue", 2)
  # dialogues_3grams <- count_ngrams(dialogues_df, "filt_dialogue", 3)
  # dialogues_4grams <- count_ngrams(dialogues_df, "filt_dialogue", 4)
  # dialogues_5grams <- count_ngrams(dialogues_df, "filt_dialogue", 5)
  
  words_df <- dialogues_df %>% 
    separate_rows(., filt_dialogue, sep = " ", convert = TRUE) %>% 
    rename(word = filt_dialogue) %>% 
    filter(word != "")%>%
    mutate(word_length = nchar(word))
  
  words_df2 <- words_df %>% 
    filter(!grepl("_",word))
  return (list(wrylies_dialogue_df,dialogues_df,words_df,words_df2))
  
}

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

most_lines_graph <- function(dialogues_df){
  
  
  dialogues_by_char <- dialogues_df %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()/1000) %>% 
    arrange(Freq) %>%
    top_n(15)
  
  dialogues_by_char$speaker <- as.factor(dialogues_by_char$speaker)
  
  ggplot(dialogues_by_char, aes(x = reorder(speaker, -Freq), y = Freq, fill= speaker)) + 
         geom_bar(stat = 'identity', colour='black') +
         labs(title = "Michael, Dwight and Jim have the highest number of dialogues in the series", 
              x = "Character", 
              y="Number of lines (in thousands)") + 
         scale_colour_manual(values = MyPalette, aesthetics = "fill") + 
    theme(legend.position="none")
}

most_words_graph <- function(words_df){
  
  words_by_char <- words_df %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()/1000) %>% 
    arrange(-Freq) %>%
    top_n(15)
  
  words_by_char$speaker <- as.factor(words_by_char$speaker)
  
  ggplot(words_by_char, aes(x = reorder(speaker, -Freq), y = Freq, fill= speaker)) + 
    geom_col(color = 'black') + 
    labs(title = "Michael, Dwight and Jim also have the highest amount of words said in the show", 
         x = "Character", 
         y="Number of words (in thousands)") + 
    scale_colour_manual(values = MyPalette, aesthetics = "fill") + 
    theme(legend.position="none")
}


screen_distribution_graph <-function(dialogues_df){
  
  dialogue_length_df <- dialogues_df %>%
    select (speaker, dialogue_length) %>%
    group_by(speaker) %>% 
    summarize (avg_d_length = sum(dialogue_length)/n(), max_dialogue = max(dialogue_length)) %>%
    top_n(15)
  
  filtered_dial <- gather(dialogue_length_df, attribute, value, -speaker)
  
  ggplot(filtered_dial, 
         aes(x = value, 
             y = fct_reorder2(speaker, 
                              attribute == 'max_dialogue', 
                              value, 
                              .desc = FALSE))) +
    geom_point(aes(color = attribute)) + 
    labs(title = "Does talking more also mean that you say the most important things?", 
         x = "Number of letters", 
         y="Character") + 
    theme(legend.position="top", 
          legend.title=element_blank() ) + 
    scale_colour_discrete(labels=c("Average dialogue", "Longest dialogue"))
  
}

length_words_graph <- function(words_df){
  
  
  word_length_df <- words_df %>%
    select (speaker, word_length) %>%
    group_by(speaker) %>% 
    summarize (avg_length = sum(word_length)/n(), max_word = max(word_length)) %>%
    top_n(15)
  
  filtered_word <- gather(word_length_df, attribute, value, -speaker)
  
  ggplot(filtered_word, 
         aes(x = value, 
             y = fct_reorder2(speaker, 
                              attribute == 'max_word', 
                              value, 
                              .desc = FALSE))) +
    geom_point(aes(color = attribute)) + 
    labs(title = "Average length of words vs longest word used by each character.", 
         x = "Number of letters", 
         y = "Character") + 
    theme(legend.position="top", legend.title=element_blank() ) + 
    scale_colour_discrete(labels=c("Average word", "Longest word")) + 
    geom_vline(xintercept=4.5)
}

crutches_graph <- function(words_df){
  
  crutches <- words_df
  crutches$crutch <- crutches$word %in% stopwords()
  crutches <- crutches %>% 
    mutate(crutch = ifelse(crutch == TRUE,1,0))
  
  crutches <- crutches %>% 
    filter(!grepl("0",crutch))
  
  count_c <- crutches %>%
    select(speaker,crutch) %>% 
    group_by(speaker) %>% 
    summarize(freq = n())
  
  aux_c <- count_c[order(-count_c$freq),] 
  top_c <- head(aux_c,15)
  top_c$freq <- top_c$freq/1000
  
  
  ggplot(data = top_c, aes(x = fct_reorder(speaker,freq,.desc = TRUE), y = freq, fill= speaker)) +
    geom_bar(stat = 'identity',colour='black') + 
    scale_colour_manual(values = MyPalette, aesthetics = "fill") +
    labs(title="Most crutch words by character (in thousands)") +
    labs(x="Character", y="Words") + 
    ylim(c(0,80)) +
    theme(legend.position="none")
  
}

deleted_scenes_graph <- function(theoffice_df){
  
  deleted_scenes <- theoffice_df %>% 
    filter(deleted == TRUE) %>% 
    select(speaker,season) %>% 
    group_by(speaker) %>% #,season
    summarize(freq = n())
  
  deleted_scenes$speaker <- tolower(deleted_scenes$speaker)
  aux_ds <- deleted_scenes[order(-deleted_scenes$freq),] 
  top_deleted <- head(aux_ds,15)
  
  #ratings$Season <- as.factor(ratings$Season)
  ggplot(data = top_deleted, aes(x = fct_reorder(speaker,freq,.desc = FALSE), y = freq,fill= speaker)) +
    geom_bar(stat = 'identity',colour = 'black') + 
    coord_flip() +
    scale_colour_manual(values = MyPalette,aesthetics = "fill") +
    labs(title="The main characters lost more scenes to editors") +
    labs(x="", y="Scenes") + 
    ylim(c(0,600)) +
    theme(legend.position="none")
  
}

interact_data <- function(dialogues_df)
{
  main_and_sec_chars <- dialogues_df %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()) %>% 
    arrange(-Freq) %>% 
    top_n(30) %>% 
    .$speaker
  
  interact_df <- words_df %>% 
    mutate(word = replace_characters(word)) %>% 
    filter(word %in% main_and_sec_chars)
  
  return(interact_df)
}

get_interactions_by_char <- function(char, interact_df) {
  interact_char <- interact_df %>% 
    filter(speaker == char)
  interactions <- table(interact_char$word) %>% 
    as.data.frame() %>% 
    arrange(-Freq)
  return(interactions)
}

get_graph <- function(character)
{
  title <- paste("How many times did",character, "say each name?")
  
  interact_df <- interact_data(dialogues_df)
  interactions <- get_interactions_by_char(character, interact_df)
  aux_i <- interactions[order(-interactions$Freq),] 
  top_i <- head(aux_i,10)
  upper_lim <- as.numeric(top_i$Freq[1]) +10
  colnames(top_i) <- c('speaker','Freq')
  
  ggplot(data = top_i, aes(x = fct_reorder(speaker,Freq,.desc = TRUE), y = Freq, fill= speaker)) +
    geom_bar(stat = 'identity',colour='black') + 
    scale_colour_manual(values = MyPalette, aesthetics = "fill") +
    labs(title=title) +
    labs(x="Character", y="Mentions") + 
    ylim(c(0,upper_lim)) +
    theme(legend.position="none")
}

get_chars_involved <- function(speaker) {
  speaker %>% 
    unique %>% 
    sort %>% 
    paste_words("_")
}



screen_time_graph <- function(words_df){
  chars_per_scene <- words_df %>% 
    group_by(season, episode, scene) %>% 
    summarise(chars = get_chars_involved(speaker)) %>% 
    ungroup %>% 
    group_by(chars) %>% 
    summarise(Freq = n()) %>% 
    arrange(-Freq) 
  
  chars_per_scene$chars <- tolower(chars_per_scene$chars)
  aux_cps <- chars_per_scene[order(-chars_per_scene$Freq),] 
  top_cps <- head(aux_cps,25)
  colnames(top_cps) <- c('speaker','Freq')
  
  ggplot(data = top_cps, aes(x = fct_reorder(speaker,Freq,.desc = TRUE), y = Freq,fill= speaker)) +
    geom_bar(stat = 'identity',colour = 'black') + 
    #scale_colour_manual(values = MyPalette,aesthetics = "fill") +
    labs(title="Most screen time (includes time with camera man)") +
    labs(x="Characters", y="Count") + 
    theme(axis.text.x=element_blank())+
    ylim(c(0,780))
  
}


character_gender_graph <-  function(){
  gen <- read.csv(file="data/main_cast_gender.csv", header=TRUE, sep=",")
  
  ggplot(gen, aes(x=fct_infreq(gen$gender))) + 
    geom_bar(aes(y=(..count..), fill = gender)) + 
    labs(title = "Over 2/3 of the show's main cast are male", 
         x = "Gender", 
         y = "Number of characters") +
    theme(legend.position="none")
}


dialogues_gender_graph <- function(words_df,dialogues_df){
  ## Merge for number of dialogues
  
  words_by_char <- words_df %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()/1000) %>% 
    arrange(-Freq) %>%
    top_n(15)
  
  dialogues_by_char <- dialogues_df %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()/1000) %>% 
    arrange(Freq) %>%
    top_n(15)
  
  gen <- read.csv(file="data/main_cast_gender.csv", header=TRUE, sep=",")
  
  gen_dialogues <- merge(dialogues_by_char, gen, all.x = TRUE) %>%
    filter(gender %in% c('male', 'female')) %>%
    group_by(gender) %>% 
    summarize(Freq = sum(Freq)) %>% 
    arrange(-Freq)
  
  plot1 <- ggplot(gen_dialogues, aes(x = reorder(gender, -Freq), y = Freq, fill = gender)) + 
    geom_col() + 
    labs(title = "Do females have more dialogues?", x = "Gender", y="Number of lines (in thousands)") + 
    theme(legend.position="none")
  
  ## Merge for number of words
  gen_words <- merge(words_by_char, gen, all.x = TRUE) %>%
    filter(gender %in% c('male', 'female')) %>%
    group_by(gender) %>% 
    summarize(Freq = sum(Freq)) %>% 
    arrange(-Freq)
  
  plot2 <- ggplot(gen_words, aes(x = reorder(gender, -Freq), y = Freq, fill = gender)) + 
    geom_col() + 
    labs(title = "Who says more words?", 
         x = "Gender", 
         y = "Number of words (in thousands)") + 
    theme(legend.position="none")
  
  grid.arrange(plot1, plot2, ncol=2)
  
}

dialogues_by_gen_graph <- function(dialogues_df){
  
  gen <- read.csv(file="data/main_cast_gender.csv", header=TRUE, sep=",")
  
  dialogues_by_gen <- merge(dialogues_df, gen, all.x = TRUE) %>%
    filter(gender %in% c('male', 'female')) %>%
    group_by(season, gender) %>% 
    summarize(Freq = n())
  
  dialogues_by_gen$season <- as.factor(dialogues_by_gen$season)
  
  ggplot(dialogues_by_gen, aes(x = season, y = Freq, fill = gender)) + 
    geom_bar(position = "fill", stat = "identity") + 
    labs(title = "The proportion of dialogues by gender remains constant through the show", 
         x = "Season", 
         y = "Percentage of lines") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 
}


viewers_graph <-  function(){
  viewers <- read.csv('data/The_office_viewers.csv')
  
  ratings <- viewers %>%  
    select(colnames(viewers)[1],Episode.overall,Episode,U.S..viewers..millions.) 
  
  colnames(ratings) <- c('Season','Episode_overall','Episode','Viewers')
  
  ratings$Season <- as.factor(ratings$Season)
  ggplot(data = ratings, aes(x = Episode_overall, y = Viewers, fill = Season)) +
    geom_bar(stat = 'identity') + 
    #  facet_grid(~Season) +
    labs(title="Did The Office maintain its viewers hooked from start to finish?") +
    labs(x="Episode", y="Viewers (in millions)") + 
    xlim(c(0,205)) +
    ylim(c(0,25)) 
}

get_most_shared_scenes <- function(words_df, char, seasons) {
  char_scenes_df <- get_char_scenes(words_df, char, seasons)
  if(nrow(char_scenes_df) > 0) {
    shared_scenes_df <- get_shared_scenes(words_df, char, char_scenes_df)
    most_shared_scenes_plot <- get_most_shared_scenes_plot(shared_scenes_df)
    return(most_shared_scenes_plot)
  } else {
    return()
  }
}

get_char_scenes <- function(words_df, char, seasons){
  char_scenes_df <- words_df %>% 
    filter_words_df(char, seasons) %>% 
    select(season, episode, scene) %>% 
    unique() %>% 
    mutate(scene_id = 1:nrow(.))
  return(char_scenes_df)
}

get_shared_scenes <- function(words_df, char, char_scenes_df){
  shared_scenes_df <- words_df %>% 
    filter(speaker != char) %>% 
    inner_join(char_scenes_df, by = c("season", "episode", "scene")) %>% 
    group_by(speaker) %>% 
    summarise(shared_scenes = length(unique(scene_id))) %>% 
    top_n(10, shared_scenes) %>% 
    mutate(speaker = fct_reorder(factor(str_to_title(speaker)), -shared_scenes))
  return(shared_scenes_df)
}

get_most_shared_scenes_plot <- function(shared_scenes_df) {
  shared_scenes_df$speaker <- tolower(shared_scenes_df$speaker)
  most_shared_scenes_plot <- ggplot(
    data = shared_scenes_df,
    aes(x = reorder(speaker, -shared_scenes), y = shared_scenes, fill= speaker)
  ) +
    geom_bar(stat = "identity", color = 'black') +
    labs(title="Jim's most common character interactions in season six.") +
    labs(x="Character", y="Shared scenes") + 
    theme(legend.position="none") +
    scale_colour_manual(values = MyPalette, aesthetics = "fill") 
  return(most_shared_scenes_plot)
 

}

phrase_graph <- function(words_df){
  thats_what_she_said <- words_df %>% 
    filter(word == "thats_what_she_said") %>% 
    group_by(speaker) %>% 
    summarise(Freq = n()) %>% 
    arrange(-Freq)
  
  characters <- unique(thats_what_she_said$speaker)
  palette_updated <- update_palette(characters, MyPalette)
  ggplot(thats_what_she_said, aes(x=reorder(speaker, Freq), y=Freq, fill = speaker)) +
    geom_col(color = 'black') + 
    coord_flip() +
    labs(title = "That's what she said.", 
         x = "", 
         y="Total number of gags") +
    scale_colour_manual(values = palette_updated, aesthetics = "fill") +
    theme(legend.position="none")
}

filter_words_df <- function(words_df, character, seasons){
  if(!is.null(seasons)) {
    filtered_words_data <- words_df %>% 
      filter(speaker == character, season %in% seasons)
    return(filtered_words_data)
  } else {
    return()
  }
}

update_palette <- function(characters, MyPalette) {
  sec_characters <- characters[!characters %in% names(MyPalette)]
  MyPalette_sec <- rep('#686868', length(sec_characters))
  names(MyPalette_sec) <- sec_characters
  UpdatedPalette <- append(MyPalette, MyPalette_sec)
  return(UpdatedPalette)
}