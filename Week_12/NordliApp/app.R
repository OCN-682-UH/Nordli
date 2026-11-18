######## HW Assignment: Creating a Shiny App ########
### Created by: Linnea Nordli
### Created on: 2026-11-14
### Week 12
####################################

## Load Libraries ##
library(shiny)
library(tidyverse)
library(palmerpenguins) # includes penguin data
library(shinythemes) # shiny theme package

## Define UI ##
ui <- fluidPage(
  theme = shinytheme("sandstone"), # add theme to app
  titlePanel(tags$b("Make a Plot using PalmerPenguin Data Package")), # add bold (tags$b) title to App page
    sidebarLayout( # select variables to plot in a sidebar panel
      sidebarPanel( # select y-axis variable
        selectInput(inputId = "y", # this ID goes in output later
                    label = "Y-axis:",
                    choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"), # select drop-down variables
                    selected = "bill_length_mm"), # select default drop-down variable
        selectInput(inputId = "x", # select x-axis 
                    label = "X-axis:",
                    choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                    selected = "body_mass_g"),
        selectInput(inputId = "color", # add option to color points by a variable
                    label = "Color by:",
                    choices = c("species", "island", "sex"),
                    selected = "species"),
        sliderInput(inputId = "year", # add slider to choose year
                label = "Choose a year", # Label above the input
                value = 2007, min = 2007, max = 2009, # values for the slider
                sep = ""), # remove the comma in year values (e.g., 2,007)
      ), 
      mainPanel( # add plot as main panel, on the side of app page
        plotOutput(outputId = "scatterplot") # creates space for scatterplot, and name for output
      )
    ),
  textInput(inputId = "title", # # add option to customize title of plot
            label = "Write a title for your plot:", 
            value = "Scatterplot of Body Mass and Bill Length of Three Penguin Species"), # starting title based on starting variables
  textInput(inputId = "xlab", # add options to modify axes titles
            label = "Write an X-axis title:",
            value = "Body Mass (g)"), # starting title based on selected starting variables 
  textInput(inputId = "ylab",
            label = "Write a Y-axis title:",
            value = "Bill Length (mm)"),
  textInput(inputId = "legend", # add option to customize legend title
            label = "Write a legend title:",
            value = "Species")
  
)

## Define server ##
server <- function(input, output) {
  
  kept_penguins <- reactive({ # add reactive slider for year in dataframe
    penguins |> 
      filter(year == input$year) 
  })
  
  output$scatterplot <- renderPlot({ 
    ggplot(data = kept_penguins(),
           aes_string(x=input$x, y=input$y, color=input$color)) +
      geom_point()+
      labs(title = input$title, x=input$xlab, y=input$ylab, color=input$legend)+ # connect input to output
      theme_bw()+ # add plot theme
      theme(plot.title = element_text(face="bold", size = 20), # bold plot title and increase title size
            axis.title = element_text(size = 14, face = "bold"), # bold axes title and change size
            legend.title = element_text(face = "bold", size = 14)) # bold legend title and change size
    })
}

## Run the application ##
shinyApp(ui = ui, server = server)

## References ##
# Week 12 Lecture Slides from Dr. Nyssa Silbiger
# Mastering Shiny Book
# Intro to Shiny - GitHub Repository
# Shiny Themes - GitHub Repository
# Shiny HTML Tags Glossary - shiny.posit.co 
