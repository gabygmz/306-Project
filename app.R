library(shiny)
library(tidyverse)
library(ggplot2)
load("39444-0005-Data.rda")

ui <- fluidPage(
  titlePanel("How does Parental Education Affect Teen Screen Time?"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      selectInput("model_name", "Model", 
                  choices = c("Mother education only" = "mother_only",
                              "Father education only" = "father_only",
                              "Mother education + conditions" = "mother_cond",
                              "Father education + conditions" = "father_cond"),
                  
                  selected = "mother_only"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Regression Coefficients", verbatimTextOutput("modelSummary")),
        tabPanel("Predicted Screen Time", plotOutput("pred_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  clean_data <- reactive({
    da39444.0005 |> 
      mutate(
        mother_education = haven::as_factor(V4164),
        father_education = haven::as_factor(V4163),
        sex = haven::as_factor(V4150),
        race = haven::as_factor(V4151),
        hours_worked = haven::as_factor(V4386),
        num_siblings = readr::parse_number(as.character(haven::zap_labels(V49))),
        wkday_ctg = readr::parse_number(as.character(haven::zap_labels(V4466))),
        wkend_ctg = readr::parse_number(as.character(haven::as_factor(V4467))),
        
        wkday_hours = case_when(
          wkday_ctg == 1 ~ 0,
          wkday_ctg == 2 ~ 0.5,
          wkday_ctg == 3 ~ 1.5,
          wkday_ctg == 4 ~ 3.5,
          wkday_ctg == 5 ~ 5.5,
          wkday_ctg == 6 ~ 7.5,
          wkday_ctg == 7 ~ 9,
          TRUE ~ NA_real_
        ),
        
        wkend_hours = case_when(
          wkend_ctg == 1 ~ 0,
          wkend_ctg == 2 ~ 0.5,
          wkend_ctg == 3 ~ 1.5,
          wkend_ctg == 4 ~ 3.5,
          wkend_ctg == 5 ~ 5.5,
          wkend_ctg == 6 ~ 7.5,
          wkend_ctg == 7 ~ 9,
          TRUE ~ NA_real_
        ),
        
        total_hours = wkday_hours + wkend_hours
      )
  })
  
  filtered_data <- reactive({
    da394 <- clean_data()
    da394 <- da394[!is.na(da394$total_hours), ]
    da394
  })
  
  model_fit <- reactive({
    da394 <- filtered_data()
    validate(need(nrow(da394) > 0, "No data after cleaning."))
    
    fit_model(input$model_name, da394)
  })
  
  fit_model <- function(model_name, da394) ({
    if (model_name == "father_only") {
      lm(total_hours ~ father_education, data = da394)
      
    } else if (model_name == "mother_only") {
      lm(total_hours ~ mother_education, data = da394)
      
    } else if (model_name == "father_cond") {
      lm(total_hours ~ father_education + sex + num_siblings + hours_worked + race, 
         data = da394)
      
    } else if (model_name == "mother_cond") {
      lm(total_hours ~ mother_education + sex + num_siblings + hours_worked + race, 
         data = da394)
    }
})
  output$modelSummary <- renderPrint({
    da394 <- filtered_data()
    summary(fit_model(input$model_name, da394))
  })
  
  output$pred_plot <- renderPlot({
    da394 <- filtered_data()
    modelo <- model_fit()
    
    if (input$model_name == "mother_only") {
      edu_variable <- "mother_education"
      x_ll <- "Mother's Education Level"
      
    } else if (input$model_name == "mother_cond"){
      edu_variable <- "mother_education"
      x_ll <- "Mother's Education Level"
      
    } else if (input$model_name == "father_only"){
      edu_variable <- "father_education"
      x_ll <- "Father's Education Level"
      
    } else {
      edu_variable <- "father_education"
      x_ll <- "Father's Education Level"
    }
    
    levs <- levels(da394[[edu_variable]])
    new <- data.frame(tmp = factor(levs, levels = levs))
    names(new)[1] <- edu_variable
    
    if ("sex" %in% names(da394)) {
      new$sex <- factor(
        levels(da394$sex)[1],
        levels = levels(da394$sex)
      )
    }
    
    if ("race" %in% names(da394)) {
      new$race <- factor(
        levels(da394$race)[1],
        levels = levels(da394$race)
      )
    }
    
    if ("num_siblings" %in% names(da394)) {
      new$num_siblings <- mean(da394$num_siblings, na.rm = TRUE)
    }
    
    if ("hours_worked" %in% names(da394)) {
      if (is.factor(da394$hours_worked)) {
        new$hours_worked <- factor(
          levels(da394$hours_worked)[1],
          levels = levels(da394$hours_worked)
        )
      } else {
        new$hours_worked <- mean(da394$hours_worked, na.rm = TRUE)
      }
    }
    
    new$prediction <- predict(modelo, newdata = new)
    
    ggplot(new, aes_string(x = edu_variable, y = "prediction")) +
      geom_point(size = 3) +
      geom_line(aes(group = 1)) +
      labs(
        x = x_ll,
        y = "Predicted Screen Time",
        title = "Predicted Screen Time by Parent Education")
  })
}
shinyApp(ui, server)
