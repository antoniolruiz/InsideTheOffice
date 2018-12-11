get_avg_dialogues_length <- function(words_df, char) {
  dl_df <- get_dialogues_length(words_df)
  avg_dl_df <- get_avg_dialogues_length_df(dl_df)
  avg_dl_plot <- get_avg_dialogues_length_plot(avg_dl_df, char)
  return(avg_dl_plot)
}

get_dialogues_length <- function(words_df) {
  dl_df <- words_df %>% 
    group_by(season, episode, scene, id) %>% 
    summarise(dl = length(word)) %>% 
    ungroup
  return(dl_df)
}

get_avg_dialogues_length_df <- function(dl_df) {
  avg_dl_df_init <- dl_df %>% 
    group_by(season) %>% 
    summarise(avg_dl = mean(dl))
  seasons_df <- data_frame(season = 1:9)
  avg_dl_df <- seasons_df %>% 
    left_join(avg_dl_df_init, by = "season") 
  return(avg_dl_df)
}

get_avg_dialogues_length_plot <- function(avg_dl_df, char) {
  int_breaks <- seq(min(avg_dl_df$season), max(avg_dl_df$season))
  char_color <- MyPalette[[str_to_title(char)]]
  avg_dl_plot <- ggplot(
    avg_dl_df,
    aes(x = season, y = avg_dl)
  ) +
    geom_line(size = 4, color = char_color) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = int_breaks) +
    xlab("season") +
    ylab("words") +
    theme_base(base_size = 18)
  return(avg_dl_plot)
}
