# helper.R
library(gtx)
library(jsonlite)
library(stringr)
library(curl)
require(lubridate)
# to run in browser
##R -e "shiny::runApp('remote-all-dashboard-wSwitch')"
#to run this from console

#Global data
##colorList <- list()
incrementByNumber <- function(n,num) {
  return(n + num)
}
incrementByOne <- function(n) {
  #print(n+1)
  return(n + 1)
}
decrementByOne <- function(n) {
  #print(n+1)
  return(n - 1)
}
# Gives you the hours backwards fromt eh current time
getTimeIntervals <- function(int_count) {
  time_intervals <- list()
  current_time <- as.character(Sys.time())
  print(paste("IT IS NOW:",current_time))
  hr_phrase <- strsplit(current_time,"\\s+")[[1]]
  str(hr_phrase)
  hr_str <- strsplit(hr_phrase[[2]],":")[[1]]
  hr <- hr_str[[1]]
  min <- hr_str[[2]]
  i <- 1
  time_intervals[[1]] <- paste(hr,":",min,sep="") #the first one
  m <- 2
  hr <- as.numeric(hr)
  min <- as.character(min)
  next_hour <- hr
  
  for (m in 1:int_count) {
    if (next_hour == 0) {
      next_hour <- 24
    } else {
      next_hour <- decrementByOne(next_hour)
    }
   # print(paste("next_hour",next_hour))
    t_hr <- as.character(next_hour)
    time_intervals[[m]] <- paste(t_hr,":",min,sep="") #second - 24th ones
  }
  #str(time_intervals)
  return(time_intervals)
}

getMuleStatsFromTodayBack <- function(metric_name,num_days,environment_id,stats) { ###},metric_count) {
  # Call api in Mule to get values
  #environment_id <- "i-8ab3b6f0"
  metric_count <- 1
  print(paste("metric_name",metric_name))
  print(paste("enviornment_id is:",environment_id))
  today <- Sys.time()
  today <- format(today, format="%Y-%m-%d %H:%M:%S") #%Y-%m-%d")
  print(paste("\nToday IS",today))
  
  tm1.lub <- ymd_hms(as.character(today))
  correct.hours <- dhours(1)
  today <- tm1.lub - correct.hours #CLA server is ahead of us by 5 
  print(paste("\nToday IS",today,"diff of 5 hours"))
  
  days_back <- as.integer(num_days)
  print(paste("days_back",days_back))
  begin_date <- as.Date(today)-days_back
  #begin_date <- as.Date(today)-days_back
  start_date <- format(begin_date, format="%Y-%m-%d %H:%M:%S")
  
  time_part <- substr(as.character(today), 12, 19)
  print(paste("time_part:",time_part))
  date_part <- substr(as.character(start_date),1,10)
  print(paste("date_part:",date_part))
    
  end_date <- paste(date_part,"%20",time_part,sep="")
  print(paste("end_date",end_date))
  start_date <- str_replace(today," ","%20")
  print(paste("begin_date:",start_date,"end_date",end_date))
  
  statistic_type <- "Average"
  print(paste("metric_count=",metric_count))
  if (length(stats) > 0) {
    statistic_type <- stats[[metric_count]]
  }
  print(paste("using stat type:",statistic_type))
  print(paste("metric_name is",metric_name))
  #browser() require(lubridate)
  
  #django_list <- c("i-2773ee49","i-23e2844d","i-f7b97596","i-5ad41a21","i-14212b74")
  
  server_group <- ""
  #for (env in django_list) {
   # if (environment_id == env) {
    #  server_group <- "&alternate=yes"
    #}
  #}
  env_length <- nchar(as.character(environment_id))
  print(paste("env_length",env_length))             
  #print(paste("length(environment_id) is", nchar(as.character(environment_id))))
  #len <- env_length - 1
  region <- substr(as.character(environment_id),env_length,env_length)
  print(paste("region is:",region))
  if (region == "D") {
    server_group <- "&alternate=blogs"
  } else if (region == "V") {
    server_group <- "&alternate=dev"
  } else if (region == "A") {
    server_group <- "&alternate=autonomy"
  } else {
    server_group <- "&alternate=web"
  }
  print(paste("region passing to service is",server_group))
  environment_id <- decodeEnvironmentId(environment_id) #this is the instance id value
  #print(paste("finale environment_id is:",environment_id,"end_date IS",end_date))
  server_address <- "http://127.0.0.1:8084"
                    #http://127.0.0.1:8084/svc/dashboards/cloudwatch/v1/cloudwatchstats?date=2015-11-27&dimensionname=InstanceId&dimensionvalue=i-8ab3b6f0&enddate=2015-11-28&format=json&metricname=CPUUtilization&namespace=AWS/EC2&period=3600&statistics=Average
  mule_rule <- paste(server_address,"/svc/dashboards/cloudwatch/v1/cloudwatchstats?date=",end_date,"&dimensionname=InstanceId&dimensionvalue=",environment_id,"&enddate=",start_date,"&format=json&metricname=",metric_name,"&namespace=AWS/EC2&period=3600&statistics=",statistic_type,server_group, sep="")
  return(mule_rule)
}

decodeEnvironmentId <- function(environment_id) { #have to decouple the instance value from the region value
  len <- nchar(as.character(environment_id)) - 2
  environment_id <- substr(environment_id,1,len)
  return(environment_id)
}
getLegend <- function(otherInstances,select_choices) {
  instanceNames <- list()
  ids <- select_choices$instance_id
  names <- select_choices$instance_name
  i <- 1
  lenn <- length(names)
  number <- 1
  #print(paste("instance length is",length(otherInstances),"length of choices:",lenn))
    
  for (instID in otherInstances) { #selected ones
    print(paste("instance",instID)) 
    i <- 1
    mID <- decodeEnvironmentId(instID)
    print(paste("ID to match on first loop:", mID))
    for (id in ids) {
      #if (i == 1) {
       # env_id <- decodeEnvironmentId(id)
      #} else {
        env_id <- id
      #}
      print(paste("ID to match inner loop:",env_id))
      if (mID == env_id & !is.na(names[i])) {
          instanceNames[[number]] <- as.character(names[i])
          print(paste("MATCHED is",names[i],"number",number,"i",i))
          number <- incrementByOne(number)
          break
      }
      i <- incrementByOne(i)
    }
  }
  return(instanceNames)
}

addOtherLines <- function(other_instances,metric_name,days,stats) { ##,metric_count) {
  colors <- contrasting.rainbow(22)
  iteration <- 1

  for (othr_instance in other_instances) {
    #print(paste("othr_instance:",othr_instance))
    if (iteration >1) {
      metric_count <- 1
      #lapply(othr_instance, plotASingleLine, metric_name, days, othr_instance, stats)
      if (length(metric_name) > 0) {
        new_url <- getMuleStatsFromTodayBack(metric_name,days,othr_instance,stats) ##,metric_count)
        
        #print(paste("metric_name:",metric_name,"days:",num_days))
        raw.data <- readLines(new_url,warn = "F")
        new_data <- fromJSON(raw.data)
        
        if (stats[[metric_count]] == "Average") {
          tLength  <- new_data$average
        } else if (stats[[metric_count]] == "Maximum") {
          tLength  <- new_data$maximum
        } else if (stats[[metric_count]] == "Minimum") {
          tLength  <- new_data$minimum
        } else if (stats[[metric_count]] == "Sum") {
          tLength  <- new_data$sum
        }
        tLength[is.na(tLength)] <- 0  #SET NA's to zero
        #str(tLength)
        #print(paste("ITeration After size after filter",tLength))
        if (tLength == 0) {
          print("SKIPPING HERE NOW YADDA YADDA!")
          newX <- new_data$average
        } else {
          #Default
          #head(new_data$average)
          newX <- tLength
          print(paste("max of x:",max(tLength)))
    
          index <- iteration
          lines(newX, NULL, type="b", pch=22, col = colors[index], lty=2, lwd=2)
        }
      }
      
     }
     iteration <- incrementByOne(iteration)
  } #each instances
  
}

#For percentage unit of measure plotting - range of 0-100% always
buildFirstPercentagePlot <-function(x,data,metric_name,yLabel,xMax,otherInstances,select_choices,days,stats,metric_count) {
  #List of all line data sets
  all_data <- list()
  print(paste("length(otherInstances)=",length(otherInstances)))
  # Good reference for plot details: https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/par.html
  if (days < 3) {
    today <- Sys.Date()
    #format(today, format="%Y-%m-%d")
    
    #today <- format(today, format="%Y-%m-%d %H:%M:%S") #%Y-%m-%d")
    #print(paste("\nToday IS",today))
    #tm1.lub <- ymd_hms("2013-07-24 23:55:26")
    #today <- tm1.lub + dhours(5) 
    
    days_back <- as.integer(days)
    begin_date <- as.Date(today)-days_back
    end_date <- today
    date_range <- paste(begin_date,"-",today)
    bottom_label <- paste("Hourly Timeline","(",date_range,")", sep=" ")
  } else {
    bottom_label <- "Hourly Timeline"
  }
  plot(x, xlim=c(xMax, 1), ylim=c(1,100), pch=4, col="blue",col.axis="black",lty=2,font.lab=477,lwd=4, 
       xaxt="n",yaxt="n", xlab=bottom_label, ylab=yLabel, font = 5, type="n", tck=1)
  
  colors <- contrasting.rainbow(22)
  
  lines(x, NULL, type = "b", pch = 22, col = colors[1], lty = 2, lwd=2)
  #par(new = T)
  addOtherLines(otherInstances,metric_name,days,stats)
  
  #(1=bottom, 2=left, 3=top, 4=right)
  #l <- seq(length(x))
  # draw an axis on the left
  
  at <- c(0,10,20,30,40,50,60,70,80,90,100)
  l <- seq(0, 100, by = 10)
  axis(2, at = at, labels = l, col.axis = "black", las = 2, tck=1, col.ticks="light gray")
  
  # draw an axis on the right, with smaller text and ticks
  #at <- seq(length(data$plot_time)) #from = 29, to = 1, by = (29-1)/input$days)
  int_count <- (24 * days) #includes today in that number
  at <- seq(int_count)
  print(paste("DAYS!",days,"vs.",int_count))
  print(paste("plot count",length(data$plotTime),"at",length(at)))
  if (days < 3 && length(otherInstances) == 1) {
    axis(1, at = at, labels = data$plotTime, col.axis = "blue", las = 2, cex.axis=1, tck=1, col.ticks="light gray")
    #axis(1, at = at, labels = l, col.axis = "blue", las = 2, cex.axis=1, tck=1, col.ticks="light gray")
  } else {
    #show less tick marks as times
    ticks = list()
    t_labels = list()
    hr_length <- length(data$plotTime)
    #ticks[[1]] <- 1
    #ticks <- seq(0,hr_length,by = 8)
    tick <- 1
    cnt <- 1
    for (tick in seq(1, hr_length, 12)) { # i in 1:length(x)-1
      #print(paste("m is",m))
#       if (tick == 0) {
#         ticks[[cnt]] <- 1
#         t_labels[[cnt]] <- data$plotTime[[1]]
#       } else {
        ticks[[cnt]] <- tick
        t_labels[[cnt]] <- paste(data$plotDateAsString[[tick]],data$plotTime[[tick]],sep="-")
      #}
      cnt <- incrementByOne(cnt)
      #print(paste("tick",tick,""))
    }
    axis(1, at = ticks, labels=t_labels, col.axis = "blue", las = 0, cex.axis=1, tck=1, col.ticks="light gray",crt="45") #
  }

  # add a main title and bottom and left axis labels
  title(paste("Metrics for ",metric_name))
  
  instanceNames <- getLegend(otherInstances,select_choices)
  if (metric_count == 1) {
    if (length(instanceNames) > 1) {
      legend("topright", legend = instanceNames, cex=1.0, bty="n", col=colors, pch=19, lwd=c(2.5,2.5), inset=c(0,0))
    }
  }
  
}

#for count/byte unit of measure plotting - based on unique max and min values
buildFirstNonPercentagePlot <- function(x,data,metric_name,yLabel,xMax,otherInstances,select_choices,days,stats,metric_count) {
  biggest <- 0
  instances_len <- length(otherInstances)
  print(paste("allocatING space for matrix; len=",instances_len))
  itemCount <- days * 24
  d_matrix <- matrix(list(), nrow=instances_len, ncol=1)
  #d_matrix <- matrix(, nrow=days, ncol=instances_len)
  print("allocated space for matrix")
  smallest <- 99999999999
  new_x  <- data$average
  #get data for first instance
  if (stats[[1]] == "Average") {
    new_x  <- data$average
  } else if (stats[[1]] == "Maximum") {
    new_x  <- data$maximum
  } else if (stats[[1]] == "Minimum") {
    new_x  <- data$minimum
  } else if (stats[[1]] == "Sum") {
    new_x  <- data$sum
  }
  new_x[is.na(new_x)] <- 0  #SET NA's to zero
  min <- as.character(min(new_x))
  max <- as.character(max(new_x))

  for (val in new_x) {  
    if (is.null(val)) {
      val <- "0"
    }
    txtVal <- as.character(val)
    numVal <- as.numeric(txtVal)
    #print(paste("txtVal",txtVal,"numVal",numVal))
    if (biggest == 0) {
      biggest <- numVal
    }
    if (numVal > biggest) {
      biggest <- numVal
    }
    if (numVal < smallest) {
      smallest <- numVal
    }
  }
  #print(paste("BIGGEST is",biggest,"smallest is",smallest))
  colors <- contrasting.rainbow(22)
  sList = list()
  tick_color <-"light gray"
  interval_graph <- 12
  mins <- c()
  mins <- append(mins, smallest)
  maxes <- c()
  maxes <- append(maxes, biggest)
  index <- 2
  i <- 1
  for (instance in otherInstances) {
    print(paste("instnace:",instance))
    instance_url <- getMuleStatsFromTodayBack(metric_name,days,instance,stats)
    instance_data <- readLines(instance_url,warn = "F")
    inst_data <- fromJSON(instance_data)
    #There's one 1 choice for metric size; max avg min or sum
    print(paste("stats[[1]] is",stats[[1]]))
    if (stats[[1]] == "Average") {
      metric_data  <- inst_data$average
    } else if (stats[[1]] == "Maximum") {
      metric_data  <- inst_data$maximum
    } else if (stats[[1]] == "Minimum") {
      metric_data  <- inst_data$minimum
    } else if (stats[[1]] == "Sum") {
      metric_data  <- inst_data$sum
    }
    metric_data[is.na(metric_data)] <- 0  #SET NA's to zero
    
    for (val in metric_data) {  
      if (is.null(val)) {
        val <- "0"
      }
      txtVal <- as.character(val)
      numVal <- as.numeric(txtVal)
      #print(paste("txtVal",txtVal,"numVal",numVal))
      if (biggest == 0) {
        biggest <- numVal
      }
      if (numVal > biggest) {
        biggest <- numVal
      }
      if (numVal < smallest) {
        smallest <- numVal
      }
    }
    mins <- append(mins, smallest)
    maxes <- append(maxes, biggest)
    print(paste("min",min,"and max",max))
    str(metric_data)
    print(paste("about to add df row to matrix"))
    #Add data for later to graph line
    d_matrix[[i,1]] <- metric_data
    print("added metric_data")
    i <- incrementByOne(i)
  }
  str(mins)
  str(maxes)
  smallest <- min(mins)
  biggest <- max(maxes)
  
  plot(NULL, xlim=c(xMax, 1), ylim=c(0,biggest), pch=4, col="blue",col.axis="light gray",lty=4,font.lab=477,lwd=4, bg="red",
       yaxt="n", xlab="(Hourly) Timeline", ylab=yLabel, main=paste("Metrics for ",metric_name), font = 3, type="n", tck=1)
  
  lines(new_x, NULL, type="b", pch=22, col=colors[1], lty=2, lwd=2)
  cnt <- 1
  for (cnt in seq(1, instances_len, 1)) {
    print(paste("eh, where is my matrix?",cnt))
    d_data <- d_matrix[[cnt,1]]
    str(d_data)
    lines(d_data, NULL, type="b", pch=22, col = colors[index], lty=2, lwd=2)
    index <- incrementByOne(index)
    cnt <- incrementByOne(cnt)
  }
  
  # horizontal
  g <- c(smallest,biggest)
  l <- seq(length(new_x))
  at <- seq(data$plotTime) ##length(
  
  if (days > 1) {
    ticks = list()
    t_labels = list()
    hr_length <- length(data$plotTime)
    tick <- 1
    cnt <- 1
    for (tick in seq(1, hr_length, interval_graph)) {
      ticks[[cnt]] <- tick
      t_labels[[cnt]] <- paste(data$plotDateAsString[[tick]],data$plotTime[[tick]],sep="-")
      cnt <- incrementByOne(cnt)
    } 
    #horizontal axis
    par(xaxt="n")
    axis(1, las=2, labels=FALSE, at=ticks, tck=1, col.ticks=tick_color, cex.axis=1,lty=2)
    tickCount <- days * 24
    t_seq <- tickCount-1
    t_picks <- seq(0,t_seq,by=4)
    lablist<-rev(t_picks)
    print(paste("lablist",lablist))
    str(lablist)
    #lablist<-as.vector(data$plotTime)
    usr <- par("usr")
    print(paste("usr is",usr))
    str(usr)
     #text(data$plotTime, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
    #seq(1, tickCount, by=6)
    text(lablist, par("usr")[3] - 99.1, family = "serif", font = 1,
          labels = lablist, srt = 45, pos = 1, xpd = TRUE, cex = 1.0, col="purple",lty=2)
  } else {
    #horizontal axis
    axis(1, las=2, labels=FALSE, at=at, tck=1, col.ticks=tick_color, cex.axis=1,lty=2)
#    axis(1, las=2, labels=data$plotTime, at=at, tck=1, col.ticks=tick_color, cex.axis=1)
    #example of rotate code
     lablist<-as.vector(seq(data$plotTime))
#     axis(1, at=seq(1, 10, by=1), labels = FALSE)
     text(data$plotTime, par("usr")[3] - 99.1, family = "serif", font = 1, 
          labels = lablist, srt = 45, pos = 1, xpd = TRUE, cex = 1.0, col="purple")
  }
  
  if (smallest == 0 && biggest == 0) { #When values are all NA instead of 0 or 1
    g <- c(-1,0,1)
    at <- g
    #vertical axis
    axis(2, las=1, labels=g, at=g, cex.axis=1, tck=1, col.ticks=tick_color,lty=1)
  } else {
    #vertical axis
    g <- c(smallest,biggest)
    axis(2, las=1, labels=g, at=g, cex.axis=1, tck=1, col.ticks=tick_color,lty=1)
  }
  
  instanceNames <- getLegend(otherInstances,select_choices)
  
  #if (metric_count == 1) {
    if (length(instanceNames) > 1) {
      legend("topright", legend = instanceNames, cex=1.0, bty="n", col=colors, pch=19, lwd=c(2.5,2.5), inset=c(-0.1,0)) #, title="Lines")
    }
  #}
}
fetchRegionOptions <- function() {
  rList <- list()
  rList[["Autonomy"]] <- "A"
  rList[["Django/Blogs"]] <- "D"
  rList[["Dev"]] <- "V"
  rList[["Web"]] <- "W"
  return(rList)
}
fetchStaticMetricChoices <- function() {
  cpu <- "CPUUtilization"
  ##cpuBalance <- "CPUCreditBalance" #only ELK test instance is using this one
  diskReadOps <- "DiskReadOps"
  diskWriteOps <- "DiskWriteOps"
  diskReadBytes <- "DiskReadBytes"
  diskWriteBytes <- "DiskWriteBytes"
  networkIn <- "NetworkIn"
  networkOut <- "NetworkOut"
  statusCheckFailed <- "StatusCheckFailed"
  statusCheckInstance <- "StatusCheckFailed_Instance"
  statusCheckSystem <- "StatusCheckFailed_System"
  
  mList <- list()
  mList[["CPU"]] <- cpu
  ##mList[["CPU Credit Balance"]] <- cpuBalance
  mList[["Disk Read Ops"]] <- diskReadOps
  mList[["Disk Write Ops"]] <- diskWriteOps
  mList[["Disk Read Bytes"]] <- diskReadBytes
  mList[["Disk Write Bytes"]] <- diskWriteBytes
  mList[["Network In"]] <- networkIn
  mList[["Network Out"]] <- networkOut
  mList[["Status Check Failed"]] <- statusCheckFailed  # 0 or 1 for counts possible
  mList[["Status Check Failed Instance"]] <- statusCheckInstance  # 0 or 1 for counts possible
  mList[["Status Check Failed System"]] <- statusCheckSystem  # 0 or 1 for counts possible
  return(mList)
}
fetchStaticInstances <- function(region_name) {
  #region_name <- input$regionlist
  str(paste('region name is',region_name))
  csvfile <-"data/aws_instances.csv"
  df1 <- read.csv(csvfile, header = T,sep="\t") ##
  str(df1)
  is.data.frame(df1)
  if (is.null(df1)) {
    print("no data in this csv")
    return(NULL)
  } else {
#     sList <- list()
#     p <- 1
#     for (instance_name in df1$instance_name) {
#        print(paste("instance_name:",instance_name))
#        region <- df1$region[[p]]
#        instance_id <- df1$instance_id[[p]]
#        #print(paste("region",region,"instance_name",instance_id))
#        value <- paste(instance_id,"_",region,sep="")
#        print(paste("value",value))
#        sList[[instance_name]] <- value
#        p <- incrementByOne(p)
#     }
#     return(list)
    #studentdata[studentdata$Drink == 'water',]
    new_dataframe <-df1[df1$region == region_name,]
    sList <- setNames(paste(as.character(new_dataframe$instance_id),"_",new_dataframe$region,sep=""), as.character(new_dataframe$instance_name))
    return(sList)
  }
}
fetchStaticInstancesOriginal <- function() {
  #static list
  livesite_prd1 <- "i-8ab3b6f0"
  livesite_prd2 <- "i-6be0dd17"
  livesite_prd3 <- "i-e3e3de9f"
  livesite_prd4 <- "i-c7e2dfbb"
  livesite_prd5 <- "i-d3141aa3"
  livesite_prd6 <- "i-5b6d2770"
  livesite_prd_varnish_1 <- "i-12164a6f"
  livesite_prd_varnish_2 <- "i-258e9659"
  livesite_prd_varnish_3 <- "i-ff809883"
  livesite_prd_varnish_4 <- "i-51829a2d"
  livesite_prd_varnish_5 <- "i-cb424bbb"
  livesite_prd_varnish_6 <- "i-bd5950cd"
  django_web_1 <- "i-2773ee49"
  django_web_2 <- "i-23e2844d"
  django_cms_1 <- "i-f7b97596"
  django_cms_2 <- "i-5ad41a21"
  django_db_01 <- "i-14212b74"
  teamsite_prd1 <- "i-7cc5ff50"
  teamsite_prd_search1 <- "i-61e826d7"
  
  sList <- list()
  sList[["livesite-prd-1"]] <- livesite_prd1
  sList[["livesite-prd-2"]] <- livesite_prd2
  sList[["livesite-prd-3"]] <- livesite_prd3
  sList[["livesite-prd-4"]] <- livesite_prd4
  sList[["livesite-prd-5"]] <- livesite_prd5
  sList[["livesite-prd-6"]] <- livesite_prd6
  sList[["livesite-prd-varnish-1"]] <- livesite_prd_varnish_1
  sList[["livesite-prd-varnish-2"]] <- livesite_prd_varnish_2
  sList[["livesite-prd-varnish-3"]] <- livesite_prd_varnish_3
  sList[["livesite-prd-varnish-4"]] <- livesite_prd_varnish_4
  sList[["livesite-prd-varnish-5"]] <- livesite_prd_varnish_5
  sList[["livesite-prd-varnish-6"]] <- livesite_prd_varnish_6
  sList[["django-web-1"]] <- django_web_1
  sList[["django-web-2"]] <- django_web_2
  sList[["django-cms-1"]] <- django_cms_1
  sList[["django-cms-2"]] <- django_cms_2
  sList[["django-db-01"]] <- django_db_01
  sList[["teamsite-prd-1"]] <- teamsite_prd1
  sList[["teamsite-prd-search-1"]] <- teamsite_prd_search1
  return (sList)
}
fetchInstancesFromCSV <- function() {
  #read instance from a file
  #csvfile <-"/Users/smosqueda/r-studio-work/mule-dashboard-app/data/aws_instances.csv"
  #csvfile <-"/srv/shiny-server/health-dashboard/data/aws_instances.csv"
  csvfile <-"data/aws_instances.csv"
  df1 <- read.csv(csvfile, header = T,sep="\t") ##
  is.data.frame(df1)
  if (is.null(df1)) {
    print("no data in this csv")
    return(NULL)
  }
  return (df1)
}

fetchInstances <- function() {
  #read csv file then reorganize the vectors into 1 list as key,value options for select list
  #output$names <- renderUI({
    #csvfile ="/srv/shiny-server/health-dashboard/data/aws_instances.csv"
    csvfile ="data/aws_instances.csv"
    #csvfile ="/Users/smosqueda/r-studio-work/mule-dashboard-app/data/aws_instances.csv"
    df1 <- readLines(csvfile) ##read.csv(csvfile, header = F)$V1 ###, sep = ",", quote = "\"", dec = " ", fill = FALSE, comment.char = "")$V1
    if (is.null(df1)) {
      return(NULL)
    }
    sList = list()
    for (row in df1) {
      pRow <- paste("the row IS ", row)
      key <- unlist(strsplit(row," - "))[[1]]
      value <- unlist(strsplit(row," - "))[[2]]
      sList[[key]] <- value
    }
    return (sList)
}
renderFullPlot <- function(num_days,statistics,select_choices,metric_name,instances,plotNumber) {
  typeMeasLoopCnt <- 1
  #for (stat in statistics) {
  print(paste("renderFullPlot is using",plotNumber))
  #for (meas in typeOfMeasurement) {
    #loopCnt <- 1
    
    for (instance in instances) { ##input$instances) {
      #print(paste("typeMeasLoopCnt is",typeMeasLoopCnt))
      #if (loopCnt < 2 && typeMeasLoopCnt == plotNumber) { 
       # print(paste("using meas",meas))
        #if (meas) > 0) {
          #d_url <- getMuleStatsFromTodayBack(meas,num_days,instance,statistics,metric_name) ##typeMeasLoopCnt)
          d_url <- getMuleStatsFromTodayBack(metric_name,num_days,instance,statistics) ##,metric_name) ##typeMeasLoopCnt)
          print(paste("url IS:",d_url))
          d.raw.data <- readLines(d_url,warn = "F")
          data <- fromJSON(d.raw.data)
          
          #Unit of measure
          yLabel <- data$unitOfMeasure[[1]]
          #metric_name <- data$name[[1]]
          xTitle <- paste(metric_name,"Data")
          xfill = "turquoise"
          yfill = "blue"
          
          #print(paste("len is",len))
          if (statistics[[1]] == "Average") {
            x <- data$average
          } else if (statistics[[1]] == "Maximum") {
            x <- data$maximum
          } else if (statistics[[1]] == "Minimum") {
            x <- data$minimum
          } else if (statistics[[1]] == "Sum") {
            x <- data$sum
          }
          #x <- data$average
          str(x)
          len <- length(x) #data$value
          print(paste("max of x:",max(x)))
          y <- 1:len
          xMax = (num_days * 24)
          print(paste("xMax is",xMax))
          
          
          if (yLabel == "Bytes" || yLabel == "Count") {
            print(paste("almost done 1! yLabel is",yLabel))
            return (buildFirstNonPercentagePlot(x,data,metric_name,yLabel,xMax,instances,select_choices,num_days,statistics,plotNumber))
          } else {
            print(paste("almost done 2! yLabel is",yLabel))
            return (buildFirstPercentagePlot(x,data,metric_name,yLabel,xMax,instances,select_choices,num_days,statistics,plotNumber))
          }
          
#           if (loopCnt == 1) {
#             output$plot_table1 <- renderTable({
#               #data[, c("plot_date", "plot_time", "value","unit_of_measure")]
#               if (statistics[[1]] == "Average") {
#                 data[, c("plotDateAsString", "plotTime", "average","unitOfMeasure","dimensionValue")]
#               } else if (statistics[[1]] == "Maximum") {
#                 data[, c("plotDateAsString", "plotTime", "maximum","unitOfMeasure","dimensionValue")]
#               } else if (statistics[[1]] == "Minimum") {
#                 data[, c("plotDateAsString", "plotTime", "minimum","unitOfMeasure","dimensionValue")]
#               } else if (statistics[[1]] == "Sum") {
#                 data[, c("plotDateAsString", "plotTime", "sum","unitOfMeasure","dimensionValue")]
#               } else {
#                 data[, c("plotDateAsString", "plotTime", "average","unitOfMeasure","dimensionValue")]
#               }
#             })
            
#          } #first instance
          
          #} #has meas value
        
      #} #First
      
      #loopCnt <- incrementByOne(loopCnt)
      
    } #instance loop   

}
