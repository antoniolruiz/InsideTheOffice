get_lines_perc <- function(dialogues_df, char, seasons) {
  char_lines_df <- get_char_lines(dialogues_df, char, seasons)
  lines_perc_df <- get_lines_perc_df(dialogues_df, char_lines_df)
  lines_perc_plot <- get_lines_perc_plot(lines_perc_df, char)
  return(lines_perc_plot)
}

get_char_lines <- function(dialogues_df, char, seasons) {
  char_lines_df <- dialogues_df %>% 
    filter_words_df(char, seasons) %>% 
    group_by(season) %>% 
    summarise(char_lines = n())
  return(char_lines_df)
}

get_lines_perc_df <- function(dialogues_df, char_lines_df) {
  season_lines_df <- dialogues_df %>% 
    group_by(season) %>% 
    summarise(total_lines = n())
  lines_perc_df_init <- season_lines_df %>% 
    left_join(char_lines_df, by = "season") %>% 
    mutate(lines_perc = char_lines / total_lines)
  seasons_df <- data_frame(season = 1:9)
  lines_perc_df <- seasons_df %>% 
    left_join(lines_perc_df_init, by = "season") %>% 
    mutate(lines_perc = ifelse(lines_perc == 0, NA, lines_perc))
  return(lines_perc_df)
}

get_lines_perc_plot <- function(lines_perc_df, char) {
  int_breaks <- seq(min(lines_perc_df$season), max(lines_perc_df$season))
  char_color <- MyPalette[[str_to_title(char)]]
  lines_perc_plot <- ggplot(
    lines_perc_df,
    aes(x = season, y = lines_perc)
  ) +
    geom_line(size = 4, color = char_color) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = int_breaks) +
    scale_y_continuous(labels = percent) +
    xlab("season") +
    ylab("dialogues") +
    theme_base(base_size = 18) 
  return(lines_perc_plot)
}
