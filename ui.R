
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Select Parameters for Titanic Analysis"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        radioButtons("model",
                    "Model Type:",
                    c("Generalized Linear Regression" = "glm",
                      "Random Forest" = "rf")
                    ),
        checkboxGroupInput("parameters",
                       "Training Params (3 or more):",
                       c("Passenger Class" = "Pclass",
                         "Fare" = "Fare",
                         "Family Size" = "FsizeD",
                         "Sex" = "Sex",
                         "Child" = "Child"),
                       c("Pclass", "Sex", "Fare")
                       ),
        sliderInput("number",
                    "Number of folds/resampling iterations:",
                    min = 5,
                    max = 20,
                    value = 5),
        submitButton("Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
        h3("Prediction Model:"),
        tableOutput("modelType"),
        h3("Model Predictors:"),
        textOutput("params"),
        h3("Number:"),
        textOutput("number"),
        h3("Prediction Model Results:"),
        textOutput("model")
    )
  )
))