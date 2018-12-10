
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(forecast)
library(tibble)
library(lubridate)
library(dplyr)
library(strucchange)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(ggrepel)
library(R.utils)
library(dlm)
library(DT)
library(gridExtra)
library(magrittr)
library(purrrlyr)
library(forcats)
library(stringr)

function_files <- sourceDirectory("functions")
for (file in function_files){
  source(file, local = TRUE)
}

shinyServer(function(input, output) {

  output$kpi <- renderUI({
    
    file_name <- "data/brand1.csv"
    respondent_data <- import_respondent_data(file_name)
    kpis <- colnames(respondent_data)[startsWith(colnames(respondent_data),"KPI")]

    choices <- c(kpis)
    names(choices) <- delete_kpi_from_names(kpis)
      
    return(selectInput(
      "test_kpi",
      label = h3("Select KPI"),
      choices = choices,
      selected = choices[1],
      width = "100%"
    ))
  })
  
  observe({
    
    if (!is.null(input$test_kpi)) {
      kpi <- input$test_kpi
    } else {
      file_name <- "data/brand1.csv"
      respondent_data <- import_respondent_data(file_name)
      kpis <- colnames(respondent_data)[startsWith(colnames(respondent_data),"KPI")]
      kpi <- kpis[1]
    }
    
    data_periods <- "months"
    frequency <- get_frequency(data_periods)
    breakpoints_criteria <- "kalman_series"
    
    brand <- "brand1"
    summary_compare_versus <- input$summary_compare_versus
    grouping_variables <- input$grouping_variables
    noise_filter <- input$noise_filter
    compare_versus <- input$compare_versus
    age <- input$age
    gender <- input$gender
    region <- input$region
    speaker <- input$speaker
    seasons <- input$seasons
    
    comparison_type <- "differences"
    file_name <- paste("data/",brand,".csv", sep="")
    respondent_data <- import_respondent_data(file_name)
    
    file_name_theoffice <- "theoffice_words"
    file_path_theoffice <- paste0("data/", file_name_theoffice,".csv")
    words_df <- read_csv(file_path_theoffice)
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
    
    filtered_words_df <- filter_words_df(words_df, speaker, seasons)
    
    filtered_data  <- filter_respondent_data(respondent_data,age,gender,region)

    period_data  <- group_filtered_data(filtered_data)

    time_series <- get_kpi_data(period_data,kpi)
    
    # SUMMARY

    output$summary_plot <- renderDataTable({
      aux <- plot_summary_data(respondent_data, grouping_variables, summary_compare_versus)
      print(aux)
      return(aux)
    })
    
    # output$doing_title <- renderText({
    #   return(paste("How is ",toupper(brand),"doing?"))
    # })
    
    # MENTIONED NAMES
    output$mentioned_names <- renderText({
      return(paste0("Which are the names that ", toupper(speaker), " mentions the most?"))
    })
    
    output$most_mentioned_names <- renderPlot({
      shiny::validate(need(input$seasons,"Check at least one Season!"))
      return(get_most_mentioned_names(filtered_words_df, chars))
    }, height = 300, width = 750)
    
    # HEALTH MAP
    
    output$brand_health <- renderPlot({
      shiny::validate(need(input$age,"Check at least one Age!"))
      shiny::validate(need(input$gender,"Check at least one Gender!"))
      shiny::validate(need(input$region,"Check at least one Region!"))
      return(get_brand_health_map(period_data, frequency, breakpoints_criteria))
    })
    
    # LATEST CHANGE
    
    output$brand_latest_change_distribution <- renderPlot({
      shiny::validate(need(input$age,"Check at least one Age!"))
      shiny::validate(need(input$gender,"Check at least one Gender!"))
      shiny::validate(need(input$region,"Check at least one Region!"))
      return(get_brand_latest_change(period_data, compare_versus))
    }, height = 300, width = 750)
    
    
    # TREND

    output$trend_title <- renderText({
      return(paste("How is ",toupper(kpi)," for ",toupper(brand), "trending?"))
    })
    
    output$latest_change <- renderPlot({
      return(plot_latest_change(time_series,comparison_type,compare_versus))
    }, height = 90, width = 250)
    
    output$mean_base <- renderText({
      return(ceiling(mean(time_series$base)))
    })
    
    output$time_series <- renderPlot({
      shiny::validate(need(input$age,"Check at least one Age!"))
      shiny::validate(need(input$gender,"Check at least one Gender!"))
      shiny::validate(need(input$region,"Check at least one Region!"))
      return(get_time_series_plot(
        time_series,
        frequency,
        breakpoints_criteria,
        noise_filter
      ))
    })

  })
})
