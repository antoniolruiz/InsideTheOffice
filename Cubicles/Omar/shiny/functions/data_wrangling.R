filter_words_df <- function(words_df, character, seasons){
  if(!is.null(seasons)) {
    filtered_words_data <- words_df %>% 
      filter(speaker == character, season %in% seasons)
    return(filtered_words_data)
  } else {
    return()
  }
}
