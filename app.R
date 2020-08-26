

library(shiny)
library(mailR)
library(googlesheets4)
library(gargle)
library(tidyverse)
options(gargle_oauth_cache = '.secrets')
options(httr_oob_default=TRUE)


# LOADING SCRIPTS
source('scripts/send_email.R')
source('scripts/cook_fit.R')
source('scripts/cook_login.R')
source('scripts/get_recipe.R')



gocook.email <- 'gocook.recipe@gmail.com'
token <- gs4_auth(
    email='ВАША ПОЧТА' ### DONT FORGET ABOUT THIS
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = '.secrets',
    use_oob = TRUE
)

table_url = "xxx" # link to logins and passwords
rates_url = "xxx" # link to ratings

recipes <- read_csv('recipes_clean.csv')
ingredients_list <- read_csv('ingredients_list.csv')
tags_list <- read_csv('tags_list.csv')
login_success <- reactive({'admin'})
model_coef <- reactive({'nolog'})

recipes_recsys <- read_csv('recipes_recsys.csv') 
recipes_id <- read_csv('id_from_rec_matrix.csv')
recipes_recsys$recipe_id <- recipes_id$id
ingredients_df <- read_csv('ingredients.csv') 
tags_df <- read_csv('tags.csv') 

rem_dup <- function(x){d <- unlist(strsplit(x, split=";"))
paste(d[-which(duplicated(d))], collapse = '; ')
}

# Define UI 
ui <- 
  navbarPage("GoCook",
             tabPanel("Авторизация",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            
                            textInput("login",
                                      "Имя пользователя:"),
                            passwordInput("password",
                                          "Пароль:"),
                            actionButton('login_button', 
                                         'Войти'),
                            br(),
                            textOutput('login_status'),
                            br(),
                            textInput("email",
                                      "Забыли пароль?\n Введите вашу электронную почту:"),
                            actionButton('forgot_button', 
                                         'Напомнить пароль'),
                            textOutput('forgot_text')), 
                          
                          mainPanel(
                            
                            br(),
                            br("Введите название или выберите ваш любимый рецепт и оцените его"),
                            selectInput('recipe_title', label = h3('Название рецепта'), c(Choose='', recipes$title), selectize=TRUE),
                            br(),
                            sliderInput("slider", label = h3("Ваша оценка"), min = 1, 
                                        max = 5, step = 1, value = 3),
                            br("1 - вам совершенно не нравится данное блюдо, 5 - вы любите данное блюдо"),
                            h4(textOutput('rate_done')),
                            br(),
                            actionButton('rate_recipes', 'Оценить рецепт')
                            
                          )))
                      
             ),
             tabPanel("Регистрация",
                      fluidPage(textInput("new_login",
                                          "Имя пользователя:"),
                                textInput("new_email",
                                          "Электронная почта:"),
                                passwordInput("new_password",
                                              "Пароль:"),
                                actionButton('create_button', 
                                             'Регистрация'),
                                textOutput('create_text'))),

        
         tabPanel("Рекомендации",
                fluidPage(sidebarLayout(
                  sidebarPanel(
                    selectInput('wanted',
                                'Выберите игредиенты, которые вы хотите включить', 
                                multiple = TRUE, 
                                choices = ingredients_list$ingredients,
                                selected = c('Спагетти', 'Бекон')),
                    selectInput('unwanted',
                                'Выберите игредиенты, которые вы хотите ислючить', 
                                multiple = TRUE, 
                                choices = ingredients_list$ingredients,
                                selected = c('Грибы')),
                    sliderInput('time', 
                                'Выберите желаемую продолжительность готовки (мин)',
                                min = 0,
                                max = 150,
                                value = c(0,150)),
                    selectInput('tags',
                                'Выберите теги', 
                                multiple = TRUE, 
                                choices = tags_list$tags,
                                selected = c('Итальянская кухня')),
                    actionButton('search', 'Поиск')),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Рецепт 1", fluidPage(h3(textOutput("recipe1_title")), 
                                                               br(),
                                                               htmlOutput("recipe1"))),
                                tabPanel("Рецепт 2", fluidPage(h3(textOutput("recipe2_title")), 
                                                               br(),
                                                               htmlOutput("recipe2"))),
                                tabPanel("Рецепт 3",  fluidPage(h3(textOutput("recipe3_title")), 
                                                                br(),
                                                                htmlOutput("recipe3"))),
                                tabPanel("Рецепт 4",  fluidPage(h3(textOutput("recipe4_title")), 
                                                                br(),
                                                                htmlOutput("recipe4"))),
                                tabPanel("Рецепт 5",  fluidPage(h3(textOutput("recipe5_title")), 
                                                                br(),
                                                                htmlOutput("recipe5")))
                    )
 
                  )
                )
                     
        )))



       



server <- function(input, output) {
  output$login_status <- eventReactive(input$login_button, {
    login_check <- cook_login(input$login,input$password)
    if (login_check == 'Вход выполнен'){
      login_success <<- reactive({input$login})}
      model_coef <<- reactive({cook_fit(login_success())})
    login_check
  })
  
  output$create_text <- eventReactive(input$create_button, {
    auth = read_sheet(table_url)
    if (input$new_login %in% auth$login | input$new_email %in% auth$email){
      return('Аккаунт уже существует')
    }
    else {
      new_user = as.data.frame(list(input$new_login, input$new_password, input$new_email), 
                               col.names = c("login", "password",'email'))
      table_url %>% 
        sheet_append(new_user)
      return('Аккаунт создан')
    }
  })
  
  output$forgot_text <- eventReactive(input$forgot_button, {
    send_email(input$email)
  })
  
  
  output$rate_done <- eventReactive(input$rate_recipes, {
    
    rates = read_sheet(rates_url)
    
    if (login_success() != "admin") {
      
      if (input$recipe_title != ''){
        
        rec_id = recipes %>% 
          filter(title == input$recipe_title) %>% 
          select(recipe_id) %>% 
          as.list()
        r_id = rec_id[["recipe_id"]]
        
        new_rate = as.data.frame(list(login_success(), r_id, input$slider), 
                                 col.names = c("login", "recipe_id",'rating'))
        rates_url %>% 
          sheet_append(new_rate)
        model_coef <<- reactive({cook_fit(login_success())})
        return("Оценка сохранена")
      }
      else {
        return("Выберите рецепт")
      }
    }
    else {
      return("Оценка рецептов доступна только авторизованным пользователям")
    }
  })
  
  
  

  output$recipe1_title <- eventReactive(input$search, {
    ingredients_df <- read_csv('ingredients.csv') 
    tags_df <- read_csv('tags.csv') 
    recipes <- read_csv('recipes_clean.csv')
    recipes_id <- read_csv('id_from_rec_matrix.csv')
    
    
    rec_ingredients <- ingredients_df %>% filter(ingredients %in% input$wanted)
    rec_ingredients$wanted <- (rec_ingredients$ingredients %in% input$wanted)
    
    good_len <- length(input$wanted)
    rec_filtered <- rec_ingredients %>% 
      group_by(id) %>% 
      summarize(count_good=sum(wanted)) %>% 
      filter(count_good==good_len)

    rec_ingredients_bad <- ingredients_df
    rec_ingredients_bad$unwanted <- (rec_ingredients_bad$ingredients %in% input$unwanted)
    rec_filtered_bad <- rec_ingredients_bad %>% 
      group_by(id) %>% 
      summarize(count_bad=sum(unwanted)) %>% 
      filter(count_bad==0)

    rec_tags <- tags_df %>% filter(tags %in% input$tags)
    rec_tags$tags <- (rec_tags$tags %in% input$tags)
    good_len_tags <- length(input$tags)
    rec_filtered_tags <- rec_tags %>% 
      group_by(id) %>% 
      summarize(count_good_tags=sum(tags)) %>% 
      filter(count_good_tags==good_len_tags)
    
    rec_time <- recipes %>% select(recipe_id, time2)
    rec_filtered_time <- rec_time %>% filter(time2>=input$time[1] & time2<=input$time[2])
    
    id_filtered_list <- rec_filtered %>% select(id)  
    id_filtered_list <- filter(id_filtered_list, id %in% rec_filtered_tags$id)
    id_filtered_list <- filter(id_filtered_list, id %in% rec_filtered_bad$id)
    id_filtered_list <- filter(id_filtered_list, id %in% rec_filtered_time$recipe_id)
    id_filtered_list$id <- as.integer(id_filtered_list$id)
    
    
    recipes_recsys <- read_csv('recipes_recsys.csv') 
    recipes_recsys$recipe_id <- recipes_id$id
    
    recipes_filtered <- filter(recipes_recsys, recipe_id %in% id_filtered_list$id)
    
    df_id <<- get_recipe(login_success(),recipes_filtered, recipes_id, recipes_recsys, recipes)
        
    ml_recipes <- recipes_recsys 
    ml_recipes$likes <- c(scale(ml_recipes$likes))
    ml_recipes$dislikes <- c(scale(ml_recipes$dislikes))
    ml_recipes$added <- c(scale(ml_recipes$added))
    ml_recipes$time2 <- c(scale(ml_recipes$time2))
    ml_recipes <- ml_recipes %>% filter(recipe_id %in% df_id) %>% select(-recipe_id)
    if (model_coef() != 'nolog') {
    pred <- predict(model_coef(), ml_recipes)
    text_pred = vector(mode="character", length=length(pred))
    text_pred[is.na(pred)]='Пока что у нас недостаточно Ваших оценок, поставьте оценки некоторым рецептам, и мы сможем предсказать понравится ли вам это блюдо'
    text_pred[pred>=4.5]='Мы уверены, что Вам понравится это блюдо'
    text_pred[pred>=3.5 & pred<4.5]='Мы думаем, что Вам понравится это блюдо'
    text_pred[pred>=2.5 & pred<3.4]='Мы считаем, что Вас удовлетворит это блюдо'
    text_pred[pred<2.4]='Вам, скорее всего, не понравится это блюдо'
    
    recommendations <<- recipes %>% 
      filter(recipe_id %in% df_id) %>% 
      mutate(text_pred=text_pred)
    }
    else {recommendations <<- recipes %>% 
      filter(recipe_id %in% df_id) %>% 
      mutate(text_pred='Пока что у нас недостаточно Ваших оценок, поставьте оценки некоторым рецептам, и мы сможем предсказать понравится ли вам это блюдо')}
    
    return(recommendations$title[1])
    
  })
  
  
  output$recipe2_title <- eventReactive(input$search, {
    return(recommendations$title[2])
  })
  
  output$recipe3_title <- eventReactive(input$search, {
    return(recommendations$title[3])
  })
  
  output$recipe4_title <- eventReactive(input$search, {
    return(recommendations$title[4])
  })
  
  output$recipe5_title <- eventReactive(input$search, {
    return(recommendations$title[5])
  })
  
  
  output$recipe1 <- eventReactive(input$search,{
    recipe_all = paste(recommendations$text_pred[1], rem_dup(recommendations$ingredients[1]),
                       recommendations$portions[1], recommendations$instruction[1], recommendations$url[1], sep =  '<br/> <br/>')
    return(HTML(recipe_all))
  })
  
  output$recipe2 <- eventReactive(input$search, {
    recipe_all2 = paste(recommendations$text_pred[2], rem_dup(recommendations$ingredients[2]),
                       recommendations$portions[2], recommendations$instruction[2], recommendations$url[2], sep =  '<br/> <br/>')
    return(HTML(recipe_all2))
  })
  
  output$recipe3 <- eventReactive(input$search, {
    recipe_all3 = paste(recommendations$text_pred[3], rem_dup(recommendations$ingredients[3]),
                       recommendations$portions[3], recommendations$instruction[3], recommendations$url[3], sep =  '<br/> <br/>')
    return(HTML(recipe_all3))
  })
  
  output$recipe4 <- eventReactive(input$search, {
    recipe_all4 = paste(recommendations$text_pred[4], rem_dup(recommendations$ingredients[4]),
                       recommendations$portions[4], recommendations$instruction[4], recommendations$url[4], sep =  '<br/> <br/>')
    return(HTML(recipe_all4))
  })
  
  output$recipe5 <- eventReactive(input$search, {
    recipe_all5 = paste(recommendations$text_pred[5], rem_dup(recommendations$ingredients[5]),
                       recommendations$portions[5], recommendations$instruction[5], recommendations$url[5], sep =  '<br/> <br/>')
    return(HTML(recipe_all5))
  })
  
  
  
  
}
  

 



# Run the application 
shinyApp(ui = ui, server = server)