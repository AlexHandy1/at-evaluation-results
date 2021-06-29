library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


#TO-DO list:
# - conditional project background page with summary bullets and study design overview and relevant links
# - Q1 results -> medication category vs individual medication -> (2 options)
  #replace bar chart images with raw data
# - Q2 results -> AT vs no AT, AC vs AP, DOACs vs Warfarin (3 options) for Jan 2020 and May 2021 (2 options - maybe 4) -> (min. 6 options, possibly 12)
  #forest plots - update underlying data and add facet groups to chart design
  #table of ORs and p-values - update underlying data and column headers (consider adjusting p-value presentation)
# - Q3 results -> AT vs no AT, AC vs AP, DOACs vs Warfarin (3 options) for Jan 2020-May 2021 and Jan 2020-Dec 2020 (2 options) for multivariable basic, propensity and Cox (3 options) -> (18 options) 
  #comparison forest plots - replace plots with underlying data
  #individual forest plots (would have to add another 2 options -> COVID-19 death vs COVID-19 hospitalisation -> 36 in total)
  #table of ORs and p-values

#OPTIONAL / FOR REVIEW
# - additional option for summary characteristics table under Q1
# - additional option for summary characteristics table under Q3
# - add in tab for study inclusion criteria flow

select_q2_data = function(medication, date, q2_results){
  if (medication == "AT vs no AT" & date == "Jan 2020"){ 
    chart_data = q2_results[[1]] 
    title_text = "Multivariate results for Any AT vs no AT Jan 2020"
  }else if (medication == "AT vs no AT" & date == "May 2021") { 
    chart_data = q2_results[[2]] 
    title_text = "Multivariate results for Any AT vs no AT May 2021"
  }else if (medication == "AC vs AP" & date == "Jan 2020") { 
    chart_data = q2_results[[3]] 
    title_text = "Multivariate results for AC vs AP Jan 2020"
  }else if (medication == "AC vs AP" & date == "May 2021") { 
    chart_data = q2_results[[4]] 
    title_text = "Multivariate results for AC vs AP May 2021"
  }else if (medication == "DOACs vs warfarin" & date == "Jan 2020") { 
    chart_data = q2_results[[5]] 
    title_text = "Multivariate results for DOACs vs warfarin Jan 2020"
  }else if (medication == "DOACs vs warfarin" & date == "May 2021") { 
    chart_data = q2_results[[6]] 
    title_text = "Multivariate results for DOACs vs warfarin May 2021"
  }else { }
  
  return_objs = list(chart_data, title_text)
  return(return_objs)
}

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
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes'",
          selectInput("summary", strong("Summary or full results"), 
                      choices = c("Summary", "Full"), selected = "Summary"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 2: AT use factors' | (input.question == 'Question 3: AT and COVID-19 outcomes' &  input.summary == 'Full')",
          selectInput("medication", strong("Medication comparison"), 
                      choices = c("AT vs no AT","AC vs AP", "DOACs vs warfarin"), selected = "AT vs no AT"),
        ), 
        conditionalPanel(
          condition = "input.question == 'Question 2: AT use factors'",
          selectInput("start_date", strong("Date"), 
                      choices = c("Jan 2020", "May 2021"), selected = "Jan 2020"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes' & input.summary == 'Full'",
          selectInput("outcome", strong("Outcome"), 
                      choices = c("COVID-19 death", "COVID-19 hospitalisation"), selected = "COVID-19 death"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes'",
          selectInput("time_period", strong("Time Period"), 
                      choices = c("Jan 2020 - May 2021", "Jan 2020 - Dec 2020"), selected = "Jan 2020 - May 2021"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes'",
          selectInput("method", strong("Method"), 
                      choices = c("Logistic regression", "Logistic regression (adj. propensity score)", "Cox regression"), selected = "Logistic regression (adj. propensity score)"),
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
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 2: AT use factors'",
          plotOutput("q2_plot"),
          br(),
          br(),
          br(),
          DT::dataTableOutput("q2_table")
        ),
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes' &  input.summary == 'Summary'",
          imageOutput("q3_summary_plot")
        ),
        
      )
      
    )
  )
)

server <- function(input, output) {
  
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
  
  #Load data for q2
  
  q2_any_at_2020_01_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_any_at_2020_01_01_14_06_2021.csv", header=T)
  q2_any_at_2021_05_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_any_at_2021_04_01_14_06_2021.csv", header=T)
  
  q2_ac_2020_01_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_ac_only_2020_01_01_14_06_2021.csv", header=T)
  q2_ac_2021_05_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_ac_only_2021_04_01_14_06_2021.csv", header=T)
  
  q2_doacs_2020_01_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_doacs_2020_01_01_14_06_2021.csv", header=T)
  q2_doacs_2021_05_01 = read.csv("results/q2_at_factors/factor_multivariate_res_table_doacs_2021_04_01_14_06_2021.csv", header=T)
  
  q2_results_list = list(
    q2_any_at_2020_01_01,
    q2_any_at_2021_05_01,
    q2_ac_2020_01_01,
    q2_ac_2021_05_01,
    q2_doacs_2020_01_01,
    q2_doacs_2021_05_01
  )

  
  #TABLES NEED TO BE UPDATED AND EXPORTED
  #ADD IN LATEST CHART CODE WHICH WILL INCLUDE FACET GROUPS
  output$q2_plot = renderPlot({
    
    #add if statement for years and medication category
    medication = input$medication
    date = input$start_date
    
    outputs = select_q2_data(medication, date, q2_results_list)
    
    chart_data = outputs[[1]]
    title_text = outputs[[2]]
    
    ggplot(data = chart_data, aes(x=var, y=or, ymin=ci_or_lower, ymax=ci_or_upper)) + 
      geom_pointrange() +
      geom_hline(yintercept=1, lty=2) +
      coord_flip() +
      xlab("Factor") + ylab("Odds Ratio (95% CI)") + labs(title = title_text, caption = "Reference categories, *<2years since AF **White ***IMD dec 1 ****South East") + theme_bw()
    
    
  })
  
  output$q2_table = DT::renderDataTable({
    
    medication = input$medication
    date = input$start_date
    
    outputs = select_q2_data(medication, date, q2_results_list)
    
    chart_data = outputs[[1]]
    title_text = outputs[[2]]
    
    headers <- c("Factor", "OR", "95% CI Lower", "95% CI Upper", "P-value")
    headers_decimals <- c("OR", "95% CI Lower", "95% CI Upper", "P-value")
    colnames(chart_data) <- headers
    #Consider updating input data so p-values replaced with <0.01 if 0.00 (to support presentation)
    DT::datatable(chart_data, rownames = F) %>% DT::formatRound(headers_decimals, 2)
  })
  
  #Load data for q3
  
  
  output$q3_summary_plot = renderImage({
    
    
    method = input$method
    time_period = input$time_period
    
    #will be updated and abstracted to function
    if ( method == "Logistic regression (adj. propensity score)" & time_period == "Jan 2020 - May 2021"){
      image_file = "www/covid_exp_comp_prop_forest_plot_2020_01_01_2021_05_01_24_06_2021.png"
    } else if  ( method == "Logistic regression (adj. propensity score)" & time_period == "Jan 2020 - Dec 2020") {
      image_file = "www/covid_exp_comp_prop_forest_plot_2020_01_01_2020_12_01_24_06_2021.png"
    } else if ( method == "Cox regression" & time_period == "Jan 2020 - May 2021") {
      image_file = "www/covid_exp_comp_cox_forest_plot_2020_01_01_2021_05_01_24_06_2021.png"
    } else if ( method == "Cox regression" & time_period == "Jan 2020 - Dec 2020") {
      image_file = "www/covid_exp_comp_cox_forest_plot_2020_01_01_2020_12_01_24_06_2021.png"
    } else {}
    
    list(src = image_file,
         contentType = 'image/png',
         width = 700,
         height = 500,
         alt = "This is a bar chart")
    
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)