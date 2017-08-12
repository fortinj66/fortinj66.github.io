
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(caret)
library(mice)
library(plyr)
library(readr)
library(parallel)
library(doParallel)
library(xtable)
library(randomForest)
library(e1071)

source("titanic_data.R")

train_ds = load_titanic_data()

shinyServer(function(input, output) {
    output$params <- renderText({ input$parameters })  
    output$number <- renderText({ input$number })
    output$modelType <- renderText({ input$model })
    output$model <- renderPrint({train_titanic_data(train_ds, 
                                                    input$model, 
                                                    input$number,
                                                    input$parameters)}
                                )
    
    

    
    
})
