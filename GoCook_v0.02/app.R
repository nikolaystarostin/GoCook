library(shiny)
library(tidyverse)
library(rvest)

recipe <- read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/recipe.csv",
                   sep = ";", encoding = "UTF-8")
colnames(recipe)[1] <- "id"
rating <- read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/rating.csv",
                   sep = ";")
colnames(rating)[1] <- "user_id"

ui <- fluidPage(
    titlePanel("GoCook v0.02"),
    sidebarLayout(
        sidebarPanel(
            numericInput("id",
                         "Введите свой ID:",
                         min = 1,
                         max = 50,
                         step = 1, 
                         value = 0),
            selectInput("include", 
                        "Выберите ингридиенты, которые вы хотите использовать:",
                        multiple = TRUE, 
                        choices = c('картофель', 'макароны')),
            selectInput("exclude", 
                        "Выберите ингридиенты, которые вы хотите исключить:",
                        multiple = TRUE, 
                        choices = c('картофель', 'макароны'))
            
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

        
}

shinyApp(ui = ui, server = server)
