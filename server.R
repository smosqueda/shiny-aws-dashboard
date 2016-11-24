library(shiny)
source("helper.R")
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observe({
    region_name <- input$regionlist
    str(paste('region name is',region_name))
    choices <- fetchStaticInstances(region_name)
    str(paste('the choices are',choices))
    updateSelectInput(session, "instances", choices=choices)
  })
  
  nPlot <- eventReactive(input$doPlots, {
    num_days <- input$days
    statistics <- as.vector(input$measurement)
    typeOfMeasurement <- input$metric_select
    if (length(statistics) > 0 && length(input$instances) > 0 || length(input$metric_select) > 0) {
         return(typeOfMeasurement)
    } else { 
      return(NULL)
    }
  })
  
  
# START of the return page content
  
   #Display selected items to user
   output$text1 <- renderText({
     instancesList <- ""
     for (s in input$instances) {
       id <- decodeEnvironmentId(s)
       if (length(input$instances) == 1) {
         instancesList <- paste(instancesList,id,"")
       } else {
          instancesList <- paste(instancesList,id,",")
       }
     }
     today <- Sys.time()
     today <- format(today, format="%Y-%m-%d %H:%M:%S") #%Y-%m-%d")
     #print(paste("\nToday IS",today))
     
     tm1.lub <- ymd_hms(as.character(today))
     five.hours <- dhours(0) #5
     today <- tm1.lub - five.hours #CLA server is ahead of us by 5 
     #print(paste("\nToday IS",today,"diff of 5 hours"))
     
     paste("Current date:",today,"You have selected instance: (", input$metric_select," & ",input$measurement, " and ", instancesList, ") for ",input$days, " days.\n")
    
   })

   
 output$plot1 <- renderPlot({
   metric_choices <- nPlot()
   if (is.null(metric_choices)) {
     return(NULL)  
   } else {
     instances <- input$instances
     num_days <- input$days
     statistics <- as.vector(input$measurement)
     typeOfMeasurement <- input$metric_select
     select_choices <- fetchInstancesFromCSV() ##loadInstancesFromDB() #to get names for the legend
     #par(oma=c(2.5,0,0,0),mgp=c(3,1,0))
     if (length(metric_choices) >= 2) {
       #margin size c(bottom, left, top, right) 
       old.par <- par(mfrow=c(2, 2),mar=c(5,5,4,4), xpd=TRUE)
     } else {
       #old.par <- par(oma=c(2.5,0,0,0),mgp=c(3,1,0)) ####
       old.par <- par(mfrow=c(2, 1),mar=c(5,5,2,2), xpd=TRUE)
     }
     cnt <- 1
     for (metric_name in metric_choices) {
        renderFullPlot(num_days,statistics,select_choices,metric_name,instances,cnt)
        cnt <- incrementByOne(cnt)
     }
     par(old.par)
   }
 })  
 

}) #output main function
