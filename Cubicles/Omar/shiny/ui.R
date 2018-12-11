
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(R.utils)

function_files <- sourceDirectory("functions")
for (file in function_files){
  source(file, local = TRUE)
}

ui <- navbarPage(
  title = h4("Inside The Office"),
  theme = shinytheme("superhero"),
  tabPanel(
    title = h4("Interactions"),
    sidebarPanel(
      radioButtons(
        "speaker1",
        label = h2("Character"),
        choices = list(
          "Michael" = "michael",
          "Dwight" = "dwight",
          "Jim" = "jim",
          "Pam" = "pam",
          "Andy" = "andy",
          "Angela" = "angela",
          "Kevin" = "kevin",
          "Erin" = "erin",
          "Ryan" = "ryan",
          "Oscar" = "oscar",
          "Darryl" = "darryl",
          "Kelly" = "kelly",
          "Jan" = "jan",
          "Toby" = "toby",
          "Phyllis" = "phyllis"
        ),
        selected = "michael"
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Most Mentioned Names",
          h2(textOutput("mentioned_names_title")),
          column(
            10,
            align = "center",
            div(
              style="display:inline-block",
              hr(),
              plotOutput("most_mentioned_names", height = 300),
              h5("Note: If the graph is not shown, it is because the chosen character does not appear in the selected seasons.")
            )
          )
        ),
        tabPanel(
          "Most Shared Scenes",
          h2(textOutput("shared_scenes_title")),
          column(
            10,
            align = "center",
            div(
              style="display:inline-block",
              hr(),
              plotOutput("most_shared_scenes", height = 300),
              h5("Note: If the graph is not shown, it is because the chosen character does not appear in the selected seasons.")
            )
          )
        )
      ),
      width = 8
    ),
    sidebarPanel(
      h2("Filter"),
      dropdownButton(
        label = "Season", status = "default", width = 30,
        checkboxGroupInput(
          "seasons",
          label = h5("Select Season"),
          choices = list(
            "One" = 1,
            "Two" = 2,
            "Three" = 3,
            "Four" = 4,
            "Five" = 5,
            "Six" = 6,
            "Seven" = 7,
            "Eight" = 8,
            "Nine" = 9
          ),
          selected= c(1:9)
        )
      ),
      hr(),
      width = 2
    )
  ),
  tabPanel(
    title = h4("Evolution"),
    sidebarPanel(
      radioButtons(
        "speaker2",
        label = h2("Character"),
        choices = list(
          "Michael" = "michael",
          "Dwight" = "dwight",
          "Jim" = "jim",
          "Pam" = "pam",
          "Andy" = "andy",
          "Angela" = "angela",
          "Kevin" = "kevin",
          "Erin" = "erin",
          "Ryan" = "ryan",
          "Oscar" = "oscar",
          "Darryl" = "darryl",
          "Kelly" = "kelly",
          "Jan" = "jan",
          "Toby" = "toby",
          "Phyllis" = "phyllis"
        ),
        selected = "michael"
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Words by Dialogue",
          h2(textOutput("dialogues_length_title")),
          column(
            10,
            align = "center",
            div(
              style="display:inline-block",
              hr(),
              plotOutput("avg_dialogues_length", height = 300)
            )
          )
        ),
        tabPanel(
          "Lines Percentage",
          h2(textOutput("lines_perc_title")),
          column(
            10,
            align = "center",
            div(
              style="display:inline-block",
              hr(),
              plotOutput("lines_perc", height = 300)
            )
          )
        )
      ),
      width = 10
    )
  )
)
