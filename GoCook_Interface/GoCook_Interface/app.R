#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GoCook(Yourself)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("login",
                        "Login:"),
            passwordInput("password",
                      "Password:"),
            actionButton('login_button', 
                         'Login'),
            selectInput('wanted',
                        'Выберите игредиенты, которые вы хотите включить', 
                        multiple = TRUE, 
                        choices = c('Спагетти', 'Бекон'),
                        selected = c('Спагетти', 'Бекон')),
            selectInput('unwanted',
                        'Выберите игредиенты, которые вы хотите ислючить', 
                        multiple = TRUE, 
                        choices = c('Грибы'),
                        selected = c('Грибы')),
            sliderInput('time', 
                        'Выберите желаемую продолжительность готовки',
                        min = 0,
                        max = 150,
                        value = c(0,150)),
            selectInput('tags',
                        'Выберите теги', 
                        multiple = TRUE, 
                        choices = c('Итальянская кухня'),
                        selected = c('Итальянская кухня')),
            submitButton(text = 'Поиск')
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("title"),
           htmlOutput('prediction'),
           textOutput('time_out'),
           textOutput('tags_out'),
           htmlOutput('ingridients'),
           textOutput('instructions'),
           textOutput('link')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$title <- renderUI(HTML('<h3><strong>Спагетти аматричана</strong></h3>'))
    output$prediction <- renderUI(HTML('<h4>Вам понравится это блюдо с вероятностю <strong>97%</strong></h4>'))
    output$time_out <- renderText('60 минут')
    output$tags_out <- renderText('Пошаговые рецепты, Паста и пицца, Итальянская кухня')
    output$ingridients <- renderUI(HTML('Спагетти <br/> Оливковое масло <br/> Бекон <br/>
                                        Лук <br/> Чеснок <br/> Помидоры <br/> Сыр <br/>
                                        Перец чили <br/> Черный перец <br/> Соль'))
    output$instructions <- renderText('1. Разогреть оливковое масло в глубокой сковороде на среднем огне. Нарезать бекон тонкими поперечными полосками, обжарить до золотистого цвета. Добавить черный перец и чили, хорошо перемешать.')
    output$link <- renderText('https://eda.ru/recepty/pasta-picca/spagetti-amatrichana-49598')
    

}

# Run the application 
shinyApp(ui = ui, server = server)
