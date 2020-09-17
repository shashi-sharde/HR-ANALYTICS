#Required Packages
library(dplyr)
library(ggplot2)
library(esquisse)
library(lubridate)
library(knitr)
library(mlr)
library(tidyverse)
library(tidyr)
library(data.table)
library(caret)
library(shiny)
library(shinythemes)

ui<- fluidPage(theme = shinytheme("spacelab"),
               
               #Application Title
               titlePanel("Candidates Offer Acceptance"),
               
               #Sidebar panel
               tabPanel("Model_Building", "Model Building", 
                        sidebarLayout(
                          sidebarPanel(
                            h5("Please upload file for training the model"),
                            h4("Training data"),
                            div(style = "display:inline-block", fileInput('file1', 'Choose CSV File')),
                            actionButton(inputId = "Build_Model", label = "Build Model")
                          ),
                          
                          mainPanel(
                            htmlOutput("upload"), htmlOutput("preprocess"),
                            textOutput("model")
                          )    
                          
                        )
               ),
               
               tabPanel("Prediction", "Prediction", 
                        
                        sidebarLayout(
                          sidebarPanel(
                            h5("Upload test file for prediction"),
                            h4("Test.csv"),
                            div(style= "display:inline-block", fileInput('file2', 'Choose CSV File')),
                            actionButton(inputId = "Predict", label = "Predict")
                          ),
                          
                          mainPanel(
                            textOutput("Predict_p"),
                            textOutput("test")
                            
                          )
                        )
                        
               )         
               
)


server <- function(input, output) {
  
  output$upload<- renderText({ 
    if(is.null(input$file1)) return(NULL)
    
    source("Training_full.R")
    data_historical1<- read.csv(input$file1$datapath, strip.white = T)
    #Build_Model(data_historical1)
    assign("train_hr", data_historical1, .GlobalEnv)
    print("Train.csv uploaded successfully")
    
  })    
  

  output$model<- renderText({
    
    if(input$Build_Model==T) {
      model<- Build_Model(train_hr)
      saveRDS(model, file= "hr_tree.rds")
      path<- getwd()
      
      paste0("3. Model built successfully and stored in the path", path)
      
    }
    
    
  })
  ######## predicting the test data
  output$test<- renderText({
    
    if(is.null(input$file2)) return(NULL)
    test_data<- read.csv(input$file2$datapath, strip.white = T)
    assign("test_data", test_data, .GlobalEnv)
    print("New data successfully uploaded")
    
  })
  
  ###predict
  output$Predict_p<- renderText({
    #source("Prediction.R")
    
    if(input$Predict == T){
      source("Prediction.R")
      model1<- readRDS("hr_tree.rds")
      predict_data(model1, test_data)
      
      path<- getwd()
      paste0("Results successfully saved in the path", path)
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
