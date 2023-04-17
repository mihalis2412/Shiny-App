
library(shiny)


# Define UI
ui <- fluidPage(
  # Add a plot for the Kaplan-Meier plot
  plotOutput("km_plot", height = "750px", width = "1800px"),
  
  fluidRow(
    # Add three checkboxes in each row
    column(4, checkboxInput("show_expo", "Exponential - Survival/survHE", FALSE)),
    column(4, checkboxInput("show_expo_rms", "Exponential - RMS", FALSE)),
    column(4, checkboxInput("show_weibull", "Weibull (AFT) - RMS", FALSE))
  ),
  
  fluidRow(
    column(4, checkboxInput("show_weibull_rms", "Weibull (AFT) - Survival/survHE", FALSE)),
    column(4, checkboxInput("show_gompertz", "Gompertz - Survival/survHE", FALSE)),
    column(4, checkboxInput("show_loglogistic", "Log-Logistic - Survival/survHE", FALSE))
  ),
  
  fluidRow(
    column(4, checkboxInput("show_loglogistic_rms", "Log-Logistic - RMS", FALSE)),
    column(4, checkboxInput("show_lognormal_rms", "Log-Normal - RMS", FALSE)),
    column(4, checkboxInput("km_ci", "Show KM Confidence Intervals", value = TRUE))
  )
)
























