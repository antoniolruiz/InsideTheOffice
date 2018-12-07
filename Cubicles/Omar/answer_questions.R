get_chars_involved <- function(speaker) {
  speaker %>% 
    unique %>% 
    sort %>% 
    paste_words("_")
}
  
chars_per_scene <- words_df %>% 
  group_by(season, episode, scene) %>% 
  summarise(chars = get_chars_involved(speaker)) %>% 
  ungroup %>% 
  group_by(chars) %>% 
  summarise(Freq = n()) %>% 
  arrange(-Freq) 

thats_what_she_said <- words_df %>% 
  filter(word == "thats_what_she_said") %>% 
  group_by(speaker) %>% 
  summarise(Freq = n()) %>% 
  arrange(-Freq)

dialogues_by_char <- dialogues_df %>% 
  group_by(speaker) %>% 
  summarise(Freq = n()) %>% 
  arrange(-Freq)

words_by_char <- words_df %>% 
  group_by(speaker) %>% 
  summarise(Freq = n()) %>% 
  arrange(-Freq)
