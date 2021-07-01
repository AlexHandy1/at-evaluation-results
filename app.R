library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


#TO-DO list:

#Sum
#- update underlying data
#- adjust formatting of charts and tables to align with updated data (e.g. facet groups)
#- review whether add any other exhibits (e.g. summary characteristics)

# - Q1 results -> medication category vs individual medication -> (2 options)
  #replace bar chart images with raw data
# - Q2 results -> AT vs no AT, AC vs AP, DOACs vs Warfarin (3 options) for Jan 2020 and May 2021 (2 options - maybe 4) -> (min. 6 options, possibly 12)
  #forest plots - update underlying data and add facet groups to chart design
  #table of ORs and p-values - update underlying data and column headers (consider adjusting p-value presentation)
# - Q3 results -> 
  #Summary (2 times x 3 methods = 6 options) comparison plots -> pending log regression + replace plots with underlying data
  #Full (3 meds x 3 methods x 2 times x 2 outcomes = 36)
    #individual forest plots  - update underlying data and add facet groups to chart design
    #table of ORs and p-values - update underlying data and column headers (consider adjusting p-value presentation)

#OPTIONAL / FOR REVIEW
# - additional option for summary characteristics table under Q1
# - additional option for summary characteristics table under Q3
# - add in correlation maps for Q2 and Q3
# - factor all code so its the file assembly method

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

select_full_q3_data = function(medication, outcome, time_period, method){
  
  #medication
  if (medication == "AT vs no AT") {
    med_file = "any_at"
  } else if (medication == "AC vs AP") {
    med_file = "ac_only"
  } else if (medication == "DOACs vs warfarin") {
    med_file = "doacs"
  } else {}
  
  #outcome
  if (outcome == "COVID-19 death") {
    outcome_file = "death"
  } else if (outcome == "COVID-19 hospitalisation"){
    outcome_file = "hospitalisation"
  } else {}
  
  #time period - requires format update for new data
  if (time_period == "Jan 2020 - May 2021") {
    time_file = "2020_01_01"
  } else if (time_period == "Jan 2020 - Dec 2020") {
    time_file = "2020_01_01"
  }
  
  #method
  if (method == "Logistic regression") {
    method_file = "basic"
  } else if (method == "Logistic regression (adj. propensity score)") {
    method_file = "prop"
  } else if (method == "Cox regression") {
    method_file = "cox"
  } else {}
  
  return_objs = list(med_file, outcome_file, time_file, method_file)
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
                      choices = c("Cohort inclusion flow chart (Jan 2020)", "Question 1: AT use","Question 2: AT use factors","Question 3: AT and COVID-19 outcomes"), selected = "Question 1: AT use"),
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
        htmlOutput("background")
      ),
      
      conditionalPanel(
        condition = "input.intro == 'Results'",
        
        conditionalPanel(
          condition = "input.question == 'Cohort inclusion flow chart (Jan 2020)'",
          imageOutput("cohort")
        ),
        
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
        
        conditionalPanel(
          condition = "input.question == 'Question 3: AT and COVID-19 outcomes' &  input.summary == 'Full'",
          plotOutput("q3_full_plot"),
          br(),
          br(),
          br(),
          DT::dataTableOutput("q3_full_table")
        ),
        
      )
      
    )
  )
)

server <- function(input, output) {
  
  getBackground<-function() {
    return(includeHTML("background.html"))
  }
  output$background <- renderUI({getBackground()})
  
  output$cohort = renderImage({
    
    image_file = "www/cohort_inclusion_chart.png"
    
    list(src = image_file,
         contentType = 'image/png',
         width = 600,
         height = 600,
         alt = "This is a cohort inclusion flow chart")
  }, deleteFile = FALSE)
  
  
  
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
  
  output$q3_full_plot = renderPlot({
    #get inputs
    medication = input$medication
    outcome = input$outcome
    time_period = input$time_period
    method = input$method
    
    #convert inputs to file text snippets
    q3_outputs = select_full_q3_data(medication, outcome, time_period, method)
    
    med_file = q3_outputs[[1]]
    outcome_file = q3_outputs[[2]]
    time_file = q3_outputs[[3]]
    method_file = q3_outputs[[4]]
    
    #assemble and load the file
    file_date = "_14_06_2021"
    data_file = paste("results/q3_at_covid/covid_multivariate_res_", method_file, "_table_", med_file, "_covid_", outcome_file, "_", time_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    title_text = paste(method, "for", medication, "on", outcome, "between", time_period, sep = " ")
      
    #create chart
    ggplot(data = chart_data, aes(x=var, y=or, ymin=ci_or_lower, ymax=ci_or_upper)) + 
      geom_pointrange() +
      geom_hline(yintercept=1, lty=2) +
      coord_flip() +
      xlab("Factor") + ylab("Odds Ratio (95% CI)") + labs(title = title_text, caption = "Reference categories, *<2years since AF **White ***IMD dec 1 ****South East") + theme_bw()
    
  })
  
  output$q3_full_table = DT::renderDataTable({
    
    #get inputs
    medication = input$medication
    outcome = input$outcome
    time_period = input$time_period
    method = input$method
    
    #convert inputs to file text snippets
    q3_outputs = select_full_q3_data(medication, outcome, time_period, method)
    
    med_file = q3_outputs[[1]]
    outcome_file = q3_outputs[[2]]
    time_file = q3_outputs[[3]]
    method_file = q3_outputs[[4]]
    
    #assemble and load the file
    file_date = "_14_06_2021"
    data_file = paste("results/q3_at_covid/covid_multivariate_res_", method_file, "_table_", med_file, "_covid_", outcome_file, "_", time_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    
    headers <- c("Factor", "OR", "95% CI Lower", "95% CI Upper", "P-value")
    headers_decimals <- c("OR", "95% CI Lower", "95% CI Upper", "P-value")
    colnames(chart_data) <- headers
    #Consider updating input data so p-values replaced with <0.01 if 0.00 (to support presentation)
    DT::datatable(chart_data, rownames = F) %>% DT::formatRound(headers_decimals, 2)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)