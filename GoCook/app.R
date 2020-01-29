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
    theme = "bootstrap.css",
    titlePanel("GoCook"),
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
                         "Choose the amount of first ingridient:",
                         step = 0.01, 
                         value = 3),
            selectInput("ing2", "Select second ingridient:",
                        choices = c("Potato, kg", "Chicken, kg",
                                    "Sausage, pieces", "Oil, ml", 
                                    "Panirovka, kg"),
                        selected = "Sausage, pieces"),
            numericInput("ing2_n",
                         "Choose the amount of second ingridient:",
                         step = 0.01, 
                         value = 2),
            selectInput("ing3", "Select third ingridient:",
                        choices = c("Potato, kg", "Chicken, kg",
                                    "Sausage, pieces", "Oil, ml", 
                                    "Panirovka, kg"),
                        selected = "Oil, ml"),
            numericInput("ing3_n",
                         "Choose the amount of third ingridient:",
                         step = 0.01, 
                         value = 500)
    ),

    mainPanel(
            htmlOutput("text"),
            tableOutput("table"),
            tableOutput("table2")
    ))


server <- function(input, output) {
        ingridients <- reactive({
            df = read.csv("https://raw.githubusercontent.com/nikolstarostin/GoCook/master/data/ingridients.csv",
                            sep = ";")
            colnames(df)[1] <- "id"
            df
            })
        item1 <- reactive({
            switch(input$ing1, 
                   "Potato, kg" = "potato_kg", 
                   "Chicken, kg" = "chicken_kg",
                   "Sausage, pieces" = "sausage_p", 
                   "Oil, ml" = "oil_ml", 
                   "Panirovka, kg" = "panirovka_kg")
            })
        item2 <- reactive({
            switch(input$ing2, 
                   "Potato, kg" = "potato_kg", 
                   "Chicken, kg" = "chicken_kg",
                   "Sausage, pieces" = "sausage_p", 
                   "Oil, ml" = "oil_ml", 
                   "Panirovka, kg" = "panirovka_kg")
            })
        item3 <- reactive({
            switch(input$ing3, 
                   "Potato, kg" = "potato_kg", 
                   "Chicken, kg" = "chicken_kg",
                   "Sausage, pieces" = "sausage_p", 
                   "Oil, ml" = "oil_ml", 
                   "Panirovka, kg" = "panirovka_kg")
            })
        newrow <- reactive({
            df <- data.frame(9999, 
                       input$ing1_n,
                       input$ing2_n,
                       input$ing3_n)
            names(df) <- c("id", item1(), item2(), item3())
            df
            })
        ingridients_full <- reactive({
            df = bind_rows(ingridients(), newrow())

            for (i in 1:ncol(df)){
                df[,i] = as.numeric(df[,i])
                df[,i] <- if_else(is.na(df[,i]),0,df[,i])
                }
            df
        })
        filter1 <- reactive({
            df <- ingridients_full()
            for (i in 1:ncol(df)){
                num = df[nrow(df),i]
                df = dplyr::filter(df,df[,i]<=num)
            }
            filter(df, df$id != 9999)
        })
        recipe_filter <- reactive({
            df = filter(recipe, id %in% filter1()$id)
            select(df, -id)
        })
        
        output$text <- renderUI({
            text <- c()
            for (i in 1:nrow(recipe_filter())){
                text[i] <- paste("<h3><strong>", 
                                 as.character(recipe_filter()[i,"name"]),
                                 "</strong></h3>",
                                 as.character(recipe_filter()[i,"text"]),
                                "<br/><br/>")
            }
            HTML(text)
        })


        output$table <- renderTable(recipe_filter())
        output$table2 <- renderTable(filter1())
}

shinyApp(ui = ui, server = server)
