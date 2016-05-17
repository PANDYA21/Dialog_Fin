library(shinydashboard)
## Only run this example in interactive R sessions
library(shiny)


body <- dashboardBody(
  h3("Tell me about yourself."),
  h4("First tell me your gender:"),
  fluidRow(
    column(12, align="center", offset = 0,
           textInput("inText1", label="",value = "", width = "60%"),
           tags$style(type="text/css", "#inText1 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
    )
  ),
  
  h4(textOutput("outText1")),
  conditionalPanel("input.inText1 == 'male' | input.inText1 == 'female'", 
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText2", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText2 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )), 
  
  h4(htmlOutput("outText2")),
  conditionalPanel("!isNaN(parseFloat(input.inText2)) && isFinite(input.inText2 )", 
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText3", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText3 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )) 
)


ui2 <- dashboardPage(dashboardHeader(disable = T), dashboardSidebar(disable = T), body)

