library(shiny)
source("helper.R")
#source("mysql.R")
library(jsonlite)
#library(ggvis)
library(DT)
#library(ggplot2)
library(datasets)
#library(googleVis)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Server Dashboard with Switch"),

  # Sidebar with a slider input for the number of bins
  fluidRow(
      column(2,
             
      selectInput("regionlist", label = h4("Server Groups"), choices = fetchRegionOptions(), selected = 0,multiple=FALSE, selectize = TRUE, width = 150)
      ,
      #selectInput("instances", label = h4("Autonomy Servers"), choices = fetchStaticInstances(), selected = 1,
       #           multiple=TRUE, selectize = FALSE, width = 180, size = 15)
      selectInput("instances", label = h4("Servers"), choices = '', selected = 1,
                  multiple=TRUE, selectize = FALSE, width = 180, size = 15)
      
      ,
      
      selectInput("metric_select", label = h4("Select Metric"), choices = fetchStaticMetricChoices(), selected = 0,multiple=TRUE, selectize = FALSE, width = 150, size = 10)
                
      ,
      #checkboxGroupInput
      radioButtons("measurement", "Type of measurement",
                         c("Maximum" = "Maximum",
                           "Average" = "Average",
                           "Minimum" = "Minimum",
                            "Sum" = "Sum" ))
      ,
      actionButton("doPlots","Show Plots")
      ,
      tags$div(title="Slide for more days of data.",
               sliderInput("days", "Number of days", min = 1, max = 7, value = 2, step = 1)
      )),
      textOutput("text1")
    ,
    mainPanel(
      tabsetPanel(type = "tabs", size="100%",
                  tabPanel("Plot & Bars", plotOutput("plot1",height = "900px",width = "1000px")
                  )
                  
      )
      

    )
  )

))
