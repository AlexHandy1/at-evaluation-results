library(shiny)
library(dplyr)
library(ggplot2)


#TO-DO list:
# - conditional project background page with summary bullets and study design overview and relevant links
# - conditional results page with show results for (Q1, Q2, Q3)
# - Q1 results -> medication category vs individual medication -> (2 options)
  #bar charts
# - Q2 results -> AT vs no AT, AC vs AP, DOACs vs Warfarin (3 options) for Jan 2020 and May 2021 (2 options - maybe 4) -> (min. 6 options, possibly 12)
  #forest plots
  #table of ORs and p-values
# - Q3 results -> AT vs no AT, AC vs AP, DOACs vs Warfarin (3 options) for Jan 2020-May 2021 and Jan 2020-Dec 2020 (2 options) for multivariable basic, propensity and Cox (3 options) -> (18 options) 
  #comparison forest plots
  #individual forest plots
  #table of ORs and p-values

#OPTIONAL / FOR REVIEW
# - additional option for summary characteristics table under Q1
# - additional option for summary characteristics table under Q3

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("intro", strong("Show me: "), 
                  choices = c("Project background","Results"), selected = "Project background"),
      
      conditionalPanel(
        condition = "input.intro == 'Results'",
        conditionalPanel(
          condition = "input.intro == 'Results'",
          selectInput("question", strong("Show results for: "), 
                      choices = c("Question 1: AT use","Question 2: AT use factors","Question 3: AT and COVID-19 outcomes"), selected = "Question 1: AT use"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 1: AT use'",
          selectInput("category", strong("Chart type"), 
                      choices = c("by category","by drug"), selected = "by AT category"),
        ), 
      ),
      
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.intro == 'Project background'",
        textOutput("hello")
      ),
      
      conditionalPanel(
        condition = "input.intro == 'Results'",
        conditionalPanel(
          condition = "input.question == 'Question 1: AT use'",
          imageOutput("q1")
        )
      )
      
    )
  )
)

server <- function(input, output) {
  
  #----------------DATA PREPARATION----------------
  
  #Load data for q1
  
  #ADD UNDERLYING CHART DATA
  
  output$hello = renderText({
    "Hello world"
  })
  
  #REPLACE WITH UNDERLYING CHART DATA
  output$q1 = renderImage({
    
    category = input$category
    
    if (category == "by category"){
      image_file = "www/prescribing_trends_cat_23_06_2021.jpg"
    } else {
      image_file = "www/prescribing_trends_ind_23_06_2021.jpg"
    }

    list(src = image_file,
         contentType = 'image/jpeg',
         width = 700,
         height = 500,
         alt = "This is a bar chart")
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)