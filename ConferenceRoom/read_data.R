library(gsheet)
library(dplyr)
library(ggplot2)

theoffice_url <- 'docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0'
theoffice_df <- gsheet2tbl(theoffice_url)