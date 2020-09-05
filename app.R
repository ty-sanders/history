library(shiny)
library(tidyverse)
library(lubridate)
library(png)
library(imager)


setwd("C:/Users/tyman/Desktop/R_Files/shiny/history")
data <- read_csv("data/states.csv")


options(shiny.sanitize.errors = TRUE)


data <- data %>%
    mutate(photo_path = "us_history.png")

data <- data %>%
    mutate(source_path = State)

data[23,]$photo_path <- "Flag_of_Maine.png"
data[27,]$photo_path <- "Florida.png"

months <- month.name
this_month <- "March"   
this_day <- 15

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                #   tags$style('.container-fluid {
                #                background-color: #ffe6e6;
                # }'),
                tags$style("#sentence {font-size:25px;
               color:blue;
               display:block; }"),
                
                # Application title
                titlePanel(windowTitle = "Today in U.S. History",  h1("Today in U.S. History", align = "left")),
                tags$h1(tags$style(".titlePanel{ 
                         color: red;
                         font-size: 20px;
                         font-style: italic;
                         }")),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        selectInput("month", "Select Month of the Year", months, selected = this_month),
                        sliderInput("day", "Select Day of the Month", min = 1, max = 31, value = this_day)),



                    mainPanel(align="center",
                              uiOutput(outputId = "image", src = "us_history.png"),
                              textOutput("sentence"),
                              uiOutput("link"),
                    )
                )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    selected <-
        reactive({
            data %>%
                filter(month %in% input$month & day %in% input$day) %>%
                slice_sample(1)
        })
    
    no_data <- 
        reactive({
            if (nrow(selected() < 1)) {
               tribble(~"Sentence", 
                       "Sentence")
            } else {
                selected()}
            })




    output$image <- renderUI({

        if ((nrow(selected()) == 1)) {
            tags$img(height = 500,
                     width  = 650,
                     src    = selected()$photo_path)
            
        } else {
            tags$img(height = 500,
                     width  = 650,
                     src    = "us_history.png")
        
        }
    })

    output$sentence <- renderText({
        if ((nrow(selected()) == 1)) {
            selected()$sentence
        } else {
            
          print("Oops, no history happened on this day!")}

    })

    url <-  reactive({
        selected()$source_path
    })

    output$link <- renderUI({

        shiny::a(h4("Click Here to Learn More", class = "btn btn-default action-button" ,
                    style = "fontweight:600"), target = "_blank",
                 href = paste0("https://en.wikipedia.org/wiki/", url()))

    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

