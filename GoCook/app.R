library(shiny)
library(tidyverse)


recipe <- read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/recipe.csv",
                        sep = ";", encoding = "UTF-8")
colnames(recipe)[1] <- "id"
rating <- read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/rating.csv",
                   sep = ";")
colnames(rating)[1] <- "user_id"

ui <- fluidPage(

    titlePanel("GoCook"),

    sidebarLayout(
        sidebarPanel(
            numericInput("id",
                        "Input your user id:",
                        min = 1,
                        max = 50,
                        step = 1, 
                        value = 0),
            selectInput("ing1", "Select first ingridient:",
                        choices = c("Potato, kg", "Chicken, kg",
                                    "Sausage, pieces", "Oil, ml", 
                                    "Panirovka, kg")),
            numericInput("ing1_n",
                         "Choose the amount of first ingridient?",
                         step = 0.01, 
                         value = 3),
            selectInput("ing2", "Select second ingridient:",
                        choices = c("Potato, kg", "Chicken, kg",
                                    "Sausage, pieces", "Oil, ml", 
                                    "Panirovka, kg")),
            numericInput("ing2_n",
                         "Choose the amount of second ingridient?",
                         step = 0.01, 
                         value = 2),
            selectInput("ing3", "Select third ingridient:",
                        choices = c("Potato, kg", "Chicken, kg",
                                    "Sausage, pieces", "Oil, ml", 
                                    "Panirovka, kg")),
            numericInput("ing3_n",
                         "Choose the amount of third ingridient?",
                         step = 0.01, 
                         value = 0)
        ),

        mainPanel(
            textOutput("name"),
            textOutput("text"),
            tableOutput("table")
        )
    )
)

server <- function(input, output) {
        ingridients <- reactive({read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/ingridients.csv",
                            sep = ";")})
        filter1 <- reactive({
            switch(input$ing1, 
                   "Potato, kg" = filter(ingridients(), potato_kg <= input$ing1_n), 
                   "Chicken, kg" = filter(ingridients(), chicken_kg <= input$ing1_n),
                   "Sausage, pieces" = filter(ingridients(), sausage_p <= input$ing1_n), 
                   "Oil, ml" = filter(ingridients(), oil_ml <= input$ing1_n), 
                   "Panirovka, kg" = filter(ingridients(), panirovka_kg <= input$ing1_n)) 
        })
        filter2 <- reactive({
            switch(input$ing2, 
                   "Potato, kg" = filter(filter1(), potato_kg <= input$ing2_n), 
                   "Chicken, kg" = filter(filter1(), chicken_kg <= input$ing2_n),
                   "Sausage, pieces" = filter(filter1(), sausage_p <= input$ing2_n), 
                   "Oil, ml" = filter(filter1(), oil_ml <= input$ing2_n), 
                   "Panirovka, kg" = filter(filter1(), panirovka_kg <= input$ing2_n)) 
        })
        filter3 <- reactive({
            switch(input$ing3, 
                   "Potato, kg" = filter(filter2(), potato_kg <= input$ing3_n), 
                   "Chicken, kg" = filter(filter2(), chicken_kg <= input$ing3_n),
                   "Sausage, pieces" = filter(filter2(), sausage_p <= input$ing3_n), 
                   "Oil, ml" = filter(filter2(), oil_ml <= input$ing3_n), 
                   "Panirovka, kg" = filter(filter2(), panirovka_kg <= input$ing3_n)) 
        })
        recipes_filter <- reactive({
            filter(recipe, id %in% filter3()[,1])
        })
        output$name <- renderText({as.character(recipes_filter()$name[1])})
        output$text <- renderText({as.character(recipes_filter()$text[1])})
        output$table <- renderTable(recipes_filter())
    
}

shinyApp(ui = ui, server = server)
