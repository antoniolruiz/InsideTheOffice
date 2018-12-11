get_most_shared_scenes <- function(words_df, char, seasons) {
  char_scenes_df <- get_char_scenes(words_df, char, seasons)
  if(nrow(char_scenes_df) > 0) {
    char_scenes_df <- char_scenes_df %>% mutate(scene_id = 1:nrow(.))
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
    unique() 
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
  UpdatedPalette <- update_palette(shared_scenes_df$speaker, MyPalette)
  most_shared_scenes_plot <- ggplot(
    data = shared_scenes_df,
    aes(x = speaker, y = shared_scenes, fill = speaker)
  ) +
    geom_bar(stat = "identity", colour='black') +
    xlab("") +
    ylab("") +
    theme_base(base_size = 18) +
    scale_colour_manual(values = UpdatedPalette, aesthetics = "fill") +
    theme(legend.position="none")
  return(most_shared_scenes_plot)
}
