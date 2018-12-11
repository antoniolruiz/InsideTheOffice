
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(R.utils)
library(forcats)
library(stringr)
library(scales)
library(ggthemes)

function_files <- sourceDirectory("functions")
for (file in function_files){
  source(file, local = TRUE)
}

shinyServer(function(input, output) {
  
  observe({
    speaker1 <- input$speaker1
    speaker2 <- input$speaker2
    seasons <- input$seasons
    
    words_path <- "data/theoffice_words.csv"
    dialogues_path <- "data/theoffice_words.csv"
    words_df <- read_csv(words_path)
    dialogues_df <- read_csv(dialogues_path)
    
    chars <- words_df %>% 
      group_by(speaker) %>% 
      summarise(Freq = n()) %>% 
      top_n(100, Freq) %>%
      .$speaker
    chars <- chars[
      !chars %in% c(
        "all", "man", "someone", "everyone", "everybody", "office", "guy",
        "manager", "phone", "woman", "together", "girl"
      )
    ]
    
    filtered_words_df1 <- filter_words_df(words_df, speaker1, seasons)
    filtered_words_df2 <- filter_words_df(words_df, speaker2, 1:9)
    
    # MENTIONED NAMES
    output$mentioned_names_title <- renderText({
      return(paste0("Which are the names that ", toupper(speaker1), " mentions the most?"))
    })
    
    output$most_mentioned_names <- renderPlot({
      shiny::validate(need(input$seasons,"Check at least one Season!"))
      return(get_most_mentioned_names(filtered_words_df1, chars))
    }, height = 300, width = 750)
    
    # SHARED SCENES
    output$shared_scenes_title <- renderText({
      return(paste0("With whom ", toupper(speaker1), " shares more scenes?"))
    })
    
    output$most_shared_scenes <- renderPlot({
      shiny::validate(need(input$seasons,"Check at least one Season!"))
      return(get_most_shared_scenes(words_df, speaker1, seasons))
    }, height = 300, width = 750)
    
    # DIALOGUES LENGTH
    output$dialogues_length_title <- renderText({
      return(paste0("On average, how long are the dialogues of ", toupper(speaker2), "?"))
    })
    
    output$avg_dialogues_length <- renderPlot({
      return(get_avg_dialogues_length(filtered_words_df2, speaker2))
    }, height = 300, width = 750)
    
    # LINES PERCENTAGE
    output$lines_perc_title <- renderText({
      return(paste0("What percentage of all the dialogues belong to ", toupper(speaker2), "?"))
    })
    
    output$lines_perc <- renderPlot({
      shiny::validate(need(input$seasons,"Check at least one Season!"))
      return(get_lines_perc(dialogues_df, speaker2, 1:9))
    }, height = 300, width = 750)
    
  })
})
