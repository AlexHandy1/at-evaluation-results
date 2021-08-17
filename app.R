library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(RColorBrewer)

select_q2_data = function(medication, start_date){
  
  #medication
  if (medication == "AT vs no AT") {
    med_file = "any_at"
  } else if (medication == "AC vs AP") {
    med_file = "ac_only"
  } else if (medication == "DOACs vs warfarin") {
    med_file = "doacs"
  } else {}
  
  #date
  if (start_date == "Jan 2020") {
    start_date_file = "2020_01_01"
  } else if (start_date == "Jul 2020") {
    start_date_file = "2020_07_01"
  }else if (start_date == "Jan 2021") {
    start_date_file = "2021_01_01"
  }else if (start_date == "May 2021") {
    start_date_file = "2021_05_01"
  } else {}
  
  return_objs = list(med_file, start_date_file)
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
  }else if (outcome == "COVID-19 death (primary diagnosis)"){
    outcome_file = "death_primary_dx"
  } else if (outcome == "COVID-19 hospitalisation"){
    outcome_file = "hospitalisation"
  } else if (outcome == "COVID-19 hospitalisation (primary diagnosis)"){
    outcome_file = "hospitalisation_primary_dx"
  }else {}
  
  #time period
  if (time_period == "Jan 2020 - May 2021") {
    time_file = "2020_01_01_2021_05_01"
  } else if (time_period == "Jan 2020 - Dec 2020") {
    time_file = "2020_01_01_2020_12_01"
  } else {}
  
  #method
  if (method == "Logistic regression (adj. propensity score)") {
    method_file = "prop"
  } else if (method == "Cox regression") {
    method_file = "cox"
  } else {}
  
  return_objs = list(med_file, outcome_file, time_file, method_file)
  return(return_objs)
}

ui <- fluidPage(
  includeCSS("custom.css"),
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
                      choices = c("Cohort inclusion flow chart (Jan 2020)", "AT use coverage","AT use factors","AT and COVID-19 outcomes"), selected = "AT use coverage"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT use coverage'",
          selectInput("category", strong("Chart type"), 
                      choices = c("by category","by drug"), selected = "by category"),
        ), 
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes'",
          selectInput("summary", strong("Summary or full results"), 
                      choices = c("Summary", "Full"), selected = "Summary"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT use factors' | (input.question == 'AT and COVID-19 outcomes' &  input.summary == 'Full')",
          selectInput("medication", strong("Medication comparison"), 
                      choices = c("AT vs no AT","AC vs AP", "DOACs vs warfarin"), selected = "AT vs no AT"),
        ), 
        # conditionalPanel(
        #   condition = "input.question == 'AT use factors'",
        #   selectInput("start_date", strong("Date"), 
        #               choices = c("Jan 2020", "Jul 2020", "Jan 2021", "May 2021"), selected = "Jan 2020"),
        # ),
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes' & input.summary == 'Full'",
          selectInput("outcome", strong("Outcome"), 
                      choices = c("COVID-19 death", "COVID-19 hospitalisation", "COVID-19 death (primary diagnosis)", "COVID-19 hospitalisation (primary diagnosis)"), selected = "COVID-19 death"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes'",
          selectInput("time_period", strong("Time Period"), 
                      choices = c("Jan 2020 - May 2021", "Jan 2020 - Dec 2020"), selected = "Jan 2020 - May 2021"),
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes'",
          selectInput("method", strong("Method"), 
                      choices = c("Logistic regression (adj. propensity score)", "Cox regression"), selected = "Logistic regression (adj. propensity score)"),
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
          condition = "input.question == 'AT use coverage'",
          plotOutput("q1")
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT use factors'",
          plotOutput("q2_plot", height = "100%"),
          br(),
          br(),
          br(),
          DT::dataTableOutput("q2_table")
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes' &  input.summary == 'Summary'",
          imageOutput("q3_summary_plot")
        ),
        
        conditionalPanel(
          condition = "input.question == 'AT and COVID-19 outcomes' &  input.summary == 'Full'",
          plotOutput("q3_full_plot", height = "100%"),
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
  
  
  output$q1 = renderPlot({
    category = input$category
    if (category == "by category"){
        chart_data = read.csv("results/q1_at_use/table_for_all_time_indices_chart_cat_16_08_2021.csv", header=T)
        title_text = "Individual antithrombotic prescriptions by category Jan 2020 - May 2021"
      } else {
        chart_data = read.csv("results/q1_at_use/table_for_all_time_indices_chart_ind_16_08_2021.csv", header=T)
        title_text = "Individual antithrombotic prescriptions by drug Jan 2020 - May 2021"
      }
    
    #order by largest drug category first
    chart_data$Drug.category <- reorder(chart_data$Drug.category, chart_data$individuals_pct)
    chart_data$Drug.category <- factor(chart_data$Drug.category, levels=levels(chart_data$Drug.category))
    
    #tidy labelling
    colnames(chart_data)[3] = "Drug category"
    
    ggplot(chart_data, aes(y = individuals_n, x = time_index, fill = `Drug category`, label = paste(individuals_pct, "%", sep="")))+ geom_bar(stat="identity") + geom_text(size = 3, position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette="Blues") + labs(title=title_text,x ="Time index", y = "Individuals %") + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
  }, width = 1000, height = 800)
  
  output$q2_plot = renderPlot({
    
    #get inputs
    medication = input$medication
    #NOTE: keep structure in case want to add other time points back in
    start_date = "Jan 2020"
    
    #convert inputs to file text snippets
    q2_outputs = select_q2_data(medication, start_date)
    med_file = q2_outputs[[1]]
    start_date_file = q2_outputs[[2]]
    
    #assemble and load the file
    file_date = "_16_08_2021"
    data_file = paste("results/q2_at_factors/factor_multivariable_res_table_", med_file, "_", start_date_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    title_text = paste("Multivariable results for", medication, "at", start_date, sep = " ")
    
    ggplot(data=chart_data, aes(x=clean_var, y=or, ymin=ci_or_lower, ymax=ci_or_upper)) +
      geom_pointrange() +
      facet_grid(var_group~., scales= "free", space="free") +
      geom_hline(yintercept=1, lty=2) + 
      coord_flip() +  
      xlab("Factor") + ylab("Odds Ratio (95% CI)") + labs(title = title_text, caption = "Reference categories , 1) White 2) IMD decile 10 (least deprived) 3) South East") + theme_bw()
    
  },width = 650, height = 800)
  
  output$q2_table = DT::renderDataTable({
    #get inputs
    medication = input$medication
    #NOTE: keep structure in case want to add other time points back in
    start_date = "Jan 2020"
    
    #convert inputs to file text snippets
    q2_outputs = select_q2_data(medication, start_date)
    med_file = q2_outputs[[1]]
    start_date_file = q2_outputs[[2]]
    
    #assemble and load the file
    file_date = "_16_08_2021"
    data_file = paste("results/q2_at_factors/factor_multivariable_res_table_", med_file, "_", start_date_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    
    #select target headers
    chart_data = chart_data %>% select(c("var_group", "clean_var", "or","ci_or_lower","ci_or_upper","p"))
    
    headers <- c("Category", "Factor", "OR", "95% CI Lower", "95% CI Upper", "P-value")
    headers_decimals <- c("OR", "95% CI Lower", "95% CI Upper", "P-value")
    colnames(chart_data) <- headers
    #Consider updating input data so p-values replaced with <0.01 if 0.00 (to support presentation)
    DT::datatable(chart_data, rownames = F) %>% DT::formatRound(headers_decimals, 2)
  })
  

  output$q3_summary_plot = renderPlot({
    
    time_period = input$time_period
    method = input$method
    
    #time period
    if (time_period == "Jan 2020 - May 2021") {
      time_file = "2020_01_01_2021_05_01"
    } else if (time_period == "Jan 2020 - Dec 2020") {
      time_file = "2020_01_01_2020_12_01"
    } else {}
    
    #method
    if (method == "Logistic regression (adj. propensity score)") {
      method_file = "prop"
    } else if (method == "Cox regression") {
      method_file = "cox"
    } else {}
    
    #assemble and load the file
    file_date = "_16_08_2021"
    data_file = paste("results/q3_at_covid/covid_exp_comp_", method_file, "_forest_plot_table_",time_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    title_text = paste("Comparison of exposures on COVID-19 outcomes (", method, ") for ", time_period, sep = "")
    
    if (method == "Cox regression"){
      y_label = "Hazard Ratio (95% CI)"
    } else {
      y_label = "Odds Ratio (95% CI)"
    }
    
    exposures_ordered = c("DOACs vs warfarin", "AC vs AP", "Any AT vs no AT")
    chart_data$clean_var <-factor(chart_data$clean_var, levels=exposures_ordered)
    
    ggplot(data=chart_data, aes(x=clean_var, y=or, ymin=ci_or_lower, ymax=ci_or_upper)) +
      geom_pointrange() +
      facet_grid(outcome~., scales= "free", space="free") +
      geom_hline(yintercept=1, lty=2) + 
      coord_flip() + 
      xlab("Exposure") + ylab(y_label) + labs(title = title_text) + geom_text(label=round(chart_data$or, 2), nudge_x=0.2) + theme_bw()
    
  })
  
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
    file_date = "_16_08_2021"
    data_file = paste("results/q3_at_covid/covid_multivariable_res_", method_file, "_table_", med_file, "_covid_", outcome_file, "_", time_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    title_text = paste(method, "for", medication, "on", outcome, "between", time_period, sep = " ")
    
    if (method == "Cox regression"){
      y_label = "Hazard Ratio (95% CI)"
    } else {
      y_label = "Odds Ratio (95% CI)"
    }
    
    #create chart
    ggplot(data=chart_data, aes(x=clean_var, y=or, ymin=ci_or_lower, ymax=ci_or_upper)) +
      geom_pointrange() +
      facet_grid(var_group~., scales= "free", space="free") +
      geom_hline(yintercept=1, lty=2) + 
      coord_flip() +  
      xlab("Factor") + ylab(y_label) + labs(title = title_text, caption = "Reference categories , 1) White 2) IMD decile 10 (least deprived) 3) South East") + theme_bw()
    
  }, width = 950, height = 950)
  
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
    file_date = "_16_08_2021"
    data_file = paste("results/q3_at_covid/covid_multivariable_res_", method_file, "_table_", med_file, "_covid_", outcome_file, "_", time_file, file_date, ".csv", sep="")
    
    #load data
    chart_data = read.csv(data_file, header=T)
    
    #select target headers
    chart_data = chart_data %>% select(c("var_group", "clean_var", "or","ci_or_lower","ci_or_upper","p"))
    
    if (method == "Cox regression"){
      headers <- c("Category", "Factor", "HR", "95% CI Lower", "95% CI Upper", "P-value")
      headers_decimals <- c("HR", "95% CI Lower", "95% CI Upper", "P-value")
    } else {
      headers <- c("Category", "Factor", "OR", "95% CI Lower", "95% CI Upper", "P-value")
      headers_decimals <- c("OR", "95% CI Lower", "95% CI Upper", "P-value")
    }
    colnames(chart_data) <- headers
    #Consider updating input data so p-values replaced with <0.01 if 0.00 (to support presentation)
    DT::datatable(chart_data, rownames = F) %>% DT::formatRound(headers_decimals, 2)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)