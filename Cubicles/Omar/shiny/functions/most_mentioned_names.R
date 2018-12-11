get_most_mentioned_names <- function(words_df, chars){
  if(nrow(words_df) > 0) {
    names_df <- get_mentioned_names(words_df, chars)
    if(nrow(names_df) > 0) {
      freq_names_df <- get_freq_names(names_df)
      most_mentioned_names_plot <- get_most_mentioned_names_plot(freq_names_df)
      return(most_mentioned_names_plot)
    } else {
      return()
    }
  } else {
    return()
  }
}

get_mentioned_names <- function(words_df, chars) {
  names_df <- words_df %>% 
    mutate(word = replace_characters(word)) %>% 
    filter(word %in% chars)
  return(names_df)
}

get_freq_names <- function(names_df) {
  freq_names <- table(names_df$word) %>% as.data.frame() %>% 
    top_n(10, Freq) %>% 
    rename(character = Var1) %>% 
    mutate(character = fct_reorder(factor(str_to_title(character)), -Freq))
  return(freq_names)
}

get_most_mentioned_names_plot <- function(freq_names_df) {
  UpdatedPalette <- update_palette(freq_names_df$character, MyPalette)
  mentioned_names_plot <- ggplot(
    data = freq_names_df,
    aes(x = character, y = Freq, fill = character)
  ) +
    geom_bar(stat = "identity", colour = "black") +
    xlab("") +
    ylab("") +
    theme_base(base_size = 18) +
    scale_colour_manual(values = UpdatedPalette, aesthetics = "fill") +
    theme(legend.position="none")
  return(mentioned_names_plot)
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