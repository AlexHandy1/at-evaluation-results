library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Hello world"),
  sidebarLayout(
    sidebarPanel(
      selectInput("intro", strong("Show me: "), 
                  choices = c("Project background","Results"), selected = "Project background")
    ),
    mainPanel(
      textOutput("hello")
    )
  )
)

server <- function(input, output) {
  output$hello = renderText({
    "Hello world"
  })
}

# Run the application 
shinyApp(ui = ui, server = server)