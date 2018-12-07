#### Analyze interactions ####
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

get_interactions_by_char("kelly", interact_df)