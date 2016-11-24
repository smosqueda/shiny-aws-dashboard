#mysql.R
library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 8889, #3306,
  "user" = "root",
  "password" = "root"
))
databaseName <- "myshinydatabase"
metric_table <- "aws_metric_stats"
instance_table <- "aws_instances"

# saveData <- function(data) {
#   # Connect to the database
#   db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
#                   port = options()$mysql$port, user = options()$mysql$user, 
#                   password = options()$mysql$password)
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   # Submit the update query and disconnect
#   dbGetQuery(db, query)
#   dbDisconnect(db)
# }
loadInstancesFromDB <- function() {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", (instance_table))
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  #str(paste("the data:",data))
  #labels <- data$instance_name
  #str(labels)
  #keys <- data$instance_id
  #str(keys)
  return(data)
}

loadMetricDataPerMetricInstance <- function() { ##metricName,instanceId,input$select,input$instances) { ###
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  #format <- "%Y-%m-%d"
  #query <- sprintf("SELECT value as measure, DATE_FORMAT(plot_date, \"%s\") ||\" \"|| plot_time as timeline FROM %s WHERE metric_name = \"%s\" and instance_id = \"%s\"", format,metric_table, metric_name, instance_id)
  #query <- sprintf("SELECT * FROM %s", metric_table," WHERE metric_name='",metric_name,"' and instance_id = '",instance_id,"'") #, metric_name, instance_id)
  #LIMIT 10
  
  query <- sprintf("SELECT * FROM %s",metric_table," ORDER by plot_date, plot_time") #" WHERE metric_name='",metricName,"' and instance_id = '",instanceId,
  
  ###[[numLimit]]
  #query <- sprintf("SELECT * FROM aws_metric_stats WHERE ORDER by plot_date,plot_time")
  
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  #query <- dbSendQuery(db, "select * from aws_metric_stats where metric_name=%s",metric_name," and instance_id = %s",instance_id)
  #data <- fetch(query, n=10)
  #dbClearResult(query)
  
  #print(paste("data is",str(data)))
  #yDate <- data$plot_date
  #str(yDate)
  return(data)
  #x <- data$value
  #str(x)
  #yTime <- data$plot_time
  #str(yTime)
  
}