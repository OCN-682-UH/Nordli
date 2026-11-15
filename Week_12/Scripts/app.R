### This is practice to create an app from scratch with a sample R Script 
### Start with our blank app
### Created by: Linnea Nordli 
### Created on: 2026-11-14
### Week 12 - Lecture Practice
####################################

## load libs
library(shiny)
library(tidyverse)

## make slider

ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number here", # Label for the input
              value = 23, min = 1, max = 100 # values for the slider: start at 25 and range from 1-100
  ),
  plotOutput("hist") # creates space (visualized in the order you code in), so if this was before sliderInput it would go above
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)

### add histogram

## add text input
ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  textInput(inputId = "title", # new Id is title
            label = "Write a title",
            value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist") # creates space for a plot called hist
)

## change title of plot
server <- function(input, output){
  
  output$hist <- renderPlot({
    
    # {} allows us to put all our R code in one nice chunk
    data <- tibble(x = rnorm(input$num)) # number of points set by the slider
    
    ggplot(data, aes(x = x)) + # make a histogram
      geom_histogram() +
      labs(title = input$title) # add a new title
  })
}

## place to put the table
ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
            label = "Choose a number", # Label above the input
            value = 25, min = 1, max = 100 # values for the slider
            ),
  textInput(inputId = "title", # new Id is title
          label = "Write a title",
          value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist"), # creates space for a plot called hist
  verbatimTextOutput("stats") # create a space for stats
)

## renderPrint
server <- function(input, output){
  
  output$hist <- renderPlot({
    
    # {} allows us to put all our R code in one nice chunk
    data <- tibble(x = rnorm(input$num)) # number of points set by the slider
    
    ggplot(data, aes(x = x)) + # make a histogram
      geom_histogram() +
      labs(title = input$title) # add a new title
  })
  
  output$stats <- renderPrint({
    summary(rnorm(input$num)) # calculate summary stats based on the numbers
  })
}

shinyApp(ui = ui, server = server)

## USE stars data 
stars <- read_csv("Week_12/NordliApp/Data/stars.csv")

glimpse(stars)

##Add a selectInput to Set a Minimum Count
ui <- fluidPage(
  selectInput(inputId = "min_stars", # ID name for the input
              label = "Show sites with at least this many sea stars:",
              choices = c(1, 5, 10, 20), # threshold options
              selected = 5 # the threshold that shows first
  ),
  plotOutput("star_map") # creates space for the map
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)


## filter the data in the server
server <- function(input, output){
  
  kept_sites <- reactive({
    stars |>
      # selectInput returns choices as STRINGS, so convert back to numeric!
      filter(star_no >= as.numeric(input$min_stars)) # sites above threshold
  })
  
  output$star_map <- renderPlot({
    ggplot(kept_sites(), aes(x = long, y = lat)) +
      geom_point(aes(size = star_no), color = "red") + # points sized by count
      labs(size = "Sea stars counted")
  })
}

# render app
shinyApp(ui = ui, server = server)
