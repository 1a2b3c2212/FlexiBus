
###############################################
#########Yuan Yuan's helper functions##########
###############################################

#input: service referring to the particular route e.g. A1, A2
#rawStart: start date rawEnd: end date
#output: return data frame with the dates with changed stops between the interval
getDistanceData <- function(service, rawStart, rawEnd){
  #change the format for dates
  start <- strptime(as.character(rawStart), "%Y-%m-%d")
  end <- strptime(as.character(rawEnd), "%Y-%m-%d")
  
  #create an empty data frame
  df <- data.frame(Date=as.Date(character(), format = "%y-%m-%d"),
                   Distance=double(),
                   stringsAsFactors=FALSE)
  
  totalChange <- length(service)
  
  small <- NULL
  smallDist <- as.double(0)
  
  #running a for loop to record all the change in distance for the particular service
  for (i in seq(1:totalChange)){
    if (as.Date(strptime(as.character(service[[i]][[1]]), "%Y-%m-%d")) < as.Date(start)){
      if(is.null(small)){
        small = as.Date(strptime(as.character(service[[i]][[1]]), "%Y-%m-%d"))
        smallDist = service[[i]][[2]] * service[[i]][[3]] * service[[i]][[5]]
      }else if(as.Date(strptime(as.character(service[[i]][[1]]), "%Y-%m-%d")) > as.Date(small)){
        small = as.Date(strptime(as.character(service[[i]][[1]]), "%Y-%m-%d"))
        smallDist = service[[i]][[2]] * service[[i]][[3]] * service[[i]][[5]]
      }else{
        next
      }
    }else if(as.Date(strptime(as.character(service[[i]][[1]]), "%Y-%m-%d")) > as.Date(end)){
      next
    }else{
      Dist = service[[i]][[2]] * service[[i]][[3]] * service[[i]][[5]]
      day = strptime(as.character(service[[i]][[1]]), "%Y-%m-%d")
      entry <- data.frame(Date = as.Date(day), Distance = as.double(Dist))
      df =rbind(df, entry)
    }
  }
  
  #add in starting and ending point
  entry <- data.frame(Date = as.Date(start), Distance = as.double(smallDist))
  df = rbind(df,entry)
  n = nrow(df)
  if(as.Date(df[n,1]) != as.Date(end)){
    entry <- data.frame(Date = as.Date(end), Distance = as.double(df[n,2]))
    df =rbind(df, entry)
  }
  
  df = df[order(as.Date(df$Date, format="%Y-%m-%d")),]
  return (df)
}

#input: dataframe with changed date and the distance
#output: two columns, with total distance for each day
eachDay <- function(df,rawStart, rawEnd){
  start <- strptime(as.character(rawStart), "%Y-%m-%d")
  end <- strptime(as.character(rawEnd), "%Y-%m-%d")
  n = nrow(df)
  
  numDay = as.double(as.Date(end) - as.Date(start)) + 1
  newdf <- setNames(data.frame(matrix(ncol = 2, nrow = numDay)), c("Date", "Distance"))
  
  #assigning the dates to the dataframe
  newdf$Date <- seq(as.Date(start), as.Date(end), by = 1)
  date = as.Date(start - 1)
  distance = df[1,2]
  counter = 1
  
  #fill up the dataframe with the distance travelled for the particular day
  for(i in seq(1:numDay)){
    if(as.Date(date + i) == as.Date(df[counter + 1,1])){
      distance = df[counter + 1, 2]
      counter = counter + 1
    }
    newdf[i, 2] = distance
  }
  
  #order the dataframe according to date
  newdf[order(as.Date(newdf$Date, format="%Y-%m-%d")),]
  return (newdf)
}

#input: the whole databse
#output: return the datafram with date and total distance for all routes combined
allRoutesdf <- function(database, rawStart, rawEnd){
  start <- strptime(as.character(rawStart), "%Y-%m-%d")
  end <- strptime(as.character(rawEnd), "%Y-%m-%d")
  df1_return <- data.frame(Date=as.Date(character(), format = "%y-%m-%d"),
                           Distance=double(),
                           stringsAsFactors=FALSE)
  
  numRoutes = length(database)
  numDays = as.double(end - start)
  df_output <- data.frame(Date=as.Date(character(), format = "%y-%m-%d"),
                          Distance=double(),
                          stringsAsFactors=FALSE)
  
  #add all the routes data for each day together
  for (i in seq(1:numRoutes)){
    if (i == 1){
      df1_return = rbind(df1_return, eachDay(getDistanceData(database[[i]],start,end),start,end))
      df_output = rbind(df_output, df1_return)
    }else{
      df2_update <- data.frame(Date=as.Date(character(), format = "%y-%m-%d"),
                               Distance=double(),
                               stringsAsFactors=FALSE)
      df2_update = rbind(df2_update, eachDay(getDistanceData(database[[i]],start,end),start,end))
      #merge two dataframes
      df_output$Distance <- as.double(df_output$Distance) + as.double(df2_update$Distance)
    }
  }
  
  return (df_output)
}

#YUANYUAN END#

################################################
####James Liu Song Yu's helper functions########
################################################

#Helper function(the demand for start location bus stop time series)
predictstartfunction<- function(input,output,session, location, values, nameofGraph){
  
  #create a subset
  summary.UT<-subset(summaryTable, summaryTable$StartLocation==location)
  
  #let the number be 1 because the demand is 1 at each bus stop each time. Ultimately, you want to find the sum which is the total demand of all the bus stops
  summary.UT$number<-1
  summarytest.xts<-xts(x=summary.UT$number,order.by = summary.UT$StartDates)
  
  #find the sum of the demand at each bus stop
  summarytest.monthly<-apply.monthly(summarytest.xts, sum)
  
  #convert to time series
  summarytest.monthly.start<- c(year(start(summarytest.monthly)), month(start(summarytest.monthly)))
  summarytest.monthly.end<- c(year(end(summarytest.monthly)), month(end(summarytest.monthly)))
  summarytest.monthly.train<-  ts(as.numeric(summarytest.monthly), start=summarytest.monthly.start,end=summarytest.monthly.end, frequency = 12)
  
  #use ARIMA forecasting with ACF value of 0 and PACF value of 1
  ut<-arima(log(summarytest.monthly.train), c(0,1,1), seasonal = list( order=c(0,1,1)))
  
  #forecast for a certain number of months
  utprediction<-predict(ut, n.ahead=values())
  
  #plot the graph with the forecasted part in dotted lines
  ts.plot(summarytest.monthly.train, 2.718^utprediction$pred, log="y", lty=c(1,3),xlab="Time",ylab="Number of demand", main=nameofGraph)
}

#Helper function(the demand for end location bus stop time series)
predictendfunction<- function(input,output,session, location, values, nameofGraph){
  
  #create a subset
  summary.UT<-subset(summaryTable, summaryTable$EndLocation==location)
  
  #let the number be 1 because the demand is 1 at each bus stop each time. Ultimately, you want to find the sum which is the total demand of all the bus stops
  summary.UT$number<-1
  summarytest.xts<-xts(x=summary.UT$number,order.by = summary.UT$EndDates)
  
  #find the sum of the demand at each bus stop
  summarytest.monthly<-apply.monthly(summarytest.xts, sum)
  
  #convert to time series
  summarytest.monthly.start<- c(year(start(summarytest.monthly)), month(start(summarytest.monthly)))
  summarytest.monthly.end<- c(year(end(summarytest.monthly)), month(end(summarytest.monthly)))
  summarytest.monthly.train<-  ts(as.numeric(summarytest.monthly), start=summarytest.monthly.start,end=summarytest.monthly.end, frequency = 12)
  
  #use ARIMA forecasting with ACF value of 0 and PACF value of 1
  ut<-arima(log(summarytest.monthly.train), c(0,1,1), seasonal = list( order=c(0,1,1)))
  
  #forecast for a certain number of months
  utprediction<-predict(ut, n.ahead=values())
  
  #plot the graph with the forecasted part in dotted lines
  ts.plot(summarytest.monthly.train, 2.718^utprediction$pred, log="y", lty=c(1,3),xlab="Time",ylab="Number of demand", main=nameofGraph)
}

getSummaryTable <- function(){
  #Function to clean data and convert to data frame so everyone can use the data frame for analysis
  options(stringsAsFactors = F)
  #retrieve data from end bus stop locations
  #uses curl function to fetch contents of url in json format
  endLoc<-curl::curl("https://bt3103-ef12e.firebaseio.com/End Locations.json")
  endTime<-fromJSON(endLoc)
  
  #creates a dataframe where the json contents are in it
  endTiming<-data.frame(as.list(endTime), stringsAsFactors=F)
  
  
  #creates a new dataframe because we want to append to it the end dates and end locations
  newdf<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("EndDates", "EndLocation"))
  
  #uses for loop to loop through EndTiming & bind the two dataframes together
  for (i in 1:ncol(endTiming)){
    EndDates<-c(endTiming[,i][2])
    EndLocation<-c(endTiming[,i][1])
    anotherdf<-data.frame(EndDates,EndLocation)
    
    newdf<- rbind(newdf, anotherdf, stringsAsFactors=F)
  }
  
  #retrieve data from start bus stop locations
  #uses curl function to fetch contents of url in json format
  startLoc<-curl::curl("https://bt3103-ef12e.firebaseio.com/Start Locations.json")
  startTime<-fromJSON(startLoc)
  
  #creates a dataframe where the json contents are in it
  starttimings<-data.frame(as.list(startTime), stringsAsFactors=F)
  
  #creates a new dataframe because we want to append to it the end dates and end locations
  newwdf<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("StartDates", "StartLocation"))
  
  #uses for loop to loop through starttimings & bind the two dataframes together
  for (i in 1:ncol(starttimings)){
    StartDates<-c(starttimings[,i][2])
    StartLocation<-c(starttimings[,i][1])
    startdf<-data.frame(StartDates,StartLocation)
    
    newwdf<- rbind(newwdf, startdf, stringsAsFactors=F)
  }
  
  #columnbind the start and end dataframe together
  summaryTable<-cbind(newdf,newwdf)
  
  #format the dates to POSIXct format
  summaryTable$EndDates<-as.POSIXct(strptime(summaryTable$EndDates,"%Y-%m-%d"))
  summaryTable$StartDates<-as.POSIXct(strptime(summaryTable$StartDates,"%Y-%m-%d"))
  return(summaryTable)
}

##JAMES LIU SONG YU END###

##################################
####Jianzhi's helper functions####
##################################

# Return the list of services
returnServiceList <- function(){
  service <- GET("https://bt3103-ef12e.firebaseio.com/latestRoutes.json")
  service <- content(service,as="parsed")
  return (as.list(toupper(names(service))))
}

# return a list of all the stops
returnStops <- function(){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops,as="parsed")
  stopList <- list()
  i <- 1
  for(stop in stops){
    stopList[i] <- stop$stopName
    i <- i+1
  }
  return (stopList)
}

# Takes in a service of type character and returns the route for that service in a list
returnLatestRoute <- function(service){
  service <- tolower(service)
  route <- GET(paste0("https://bt3103-ef12e.firebaseio.com/latestRoutes/",service,".json"))
  route <- content(route, as="parsed")
  
  date1 = route[[1]]$effDate
  date2 = route[[2]]$effDate
  
  if(date1>date2){
    futureRoute = route[[1]]
    currentRoute = route[[2]]
  }
  else{
    futureRoute = route[[2]]
    currentRoute = route[[1]]
  }
  
  futureRoute <- futureRoute$stops
  futureRoute[1] <- NULL
  return(futureRoute)
}

# Return a list of buses
returnBuses <- function(){
  buses <- fromJSON(content(GET("https://bt3103-ef12e.firebaseio.com/buses.json"), as="text"))
  buses <- buses$busID
  buses <- as.list(buses)
  buses[[1]] <- NULL
  return(buses)
}

# Return the list of services
returnServiceList <- function(){
  service <- GET("https://bt3103-ef12e.firebaseio.com/latestRoutes.json")
  service <- content(service,as="parsed")
  return (as.list(toupper(names(service))))
}

# Return the service of a given busID
returnService <- function(busID){
  return (content(GET(paste0("https://bt3103-ef12e.firebaseio.com/buses/",busID,".json")), as="parsed")$route)
}

#### Author: Jianzhi
#### Desc: Get a stop based on the stopName
getStopByName <- function(stopName){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops, as = "parsed")
  stops <- as.list(stops)
  for(stop in stops){
    if(stopName == stop$stopName){
      return(stop)
    }
  }
}

#### Author: Jianzhi
#### Desc: Get a stop based on the stopID
getStopByID <- function(stopID){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops, as = "parsed")
  stops <- as.list(stops)
  for(stop in stops){
    if(stopID == stop$stopID){
      return(stop)
    }
  }
}

#### Author: Jianzhi
#### Desc: Calculate distance between two stops
findDistance <- function(stop1,stop2){
  lat1 <- stop1$lat
  lat2 <- stop2$lat
  long1 <- stop1$long
  long2 <- stop2$long
  dist <- GET(paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",lat1,",",long1,"&destinations=",lat2,",",long2,"&key=AIzaSyDYNsZrtv9hzi25no8QtdyCf2ukrjssqA0"))
  dist_content <- content(dist, as="parsed")
  distance <- dist_content$rows[[1]]$elements[[1]]$distance$value
  return (distance)
}

#### Author: Jianzhi
#### Desc: Calculate total distance of route
totalDistanceOfRoute <- function(route){
  total <- list()
  
  if(length(route)>0){ 
    for(i in 1:(length(route)-1)){
      currStop <- getStopByName(route[[i]])
      nextStop <- getStopByName(route[[i+1]])
      d <- findDistance(currStop,nextStop)
      total[i] <- d
    }
    return (Reduce("+",total))
  }
  else{
    return (findDistance(getStopByName(route[[1]]),getStopByName(route[[2]])))
  }
}

#### Author: Jianzhi
#### Desc: Creates a table displaying the route with numbered stops
returnRouteTable <- function(raw_route){
  reactiveRoute <- raw_route
  routeObject <- t(data.frame(t(raw_route)))
  routeObject <- cbind(id=seq.int(nrow(routeObject)),routeObject)
  colnames(routeObject) <- c("ID","Stop")
  return(routeObject)
}

#### Author: Jianzhi
#### Desc: Get polyline from route
getPolyline <- function(route){
  originStop <- getStopByName(route[[1]])
  destinationStop <- getStopByName(route[[length(route)]])
  origin <- c(as.numeric(originStop$lat), as.numeric(originStop$long))
  destination <- c(as.numeric(destinationStop$lat),as.numeric(destinationStop$long))
  
  ## and the coordinates in between as waypoints
  if(length(route)>2){
    waypoints <- list()
    for(i in 2:(length(route)-1)){
      currStop <- getStopByName(route[[i]])
      waypoints <- c(waypoints,list(stop=c(as.numeric(currStop$lat),as.numeric(currStop$long))))
    }
    res <- google_directions(origin = origin,
                             destination = destination,
                             waypoints = waypoints,
                             key = google_dir_key)
  }
  else{
    ## get the directions from Google Maps API
    res <- google_directions(origin = origin,
                             destination = destination,
                             key = google_dir_key)
  }
  
  ## include simplify = F to return data as JSON
  
  df_polyline <- res$routes$overview_polyline$points
  return (df_polyline)
  
}

## Jianzhi end ##

##################################################
#######Wee Seng's helper functions################
##################################################

#### Author: Wee Seng
#### Desc: Return names of stops
returnNamesOfStops <- function(){
  stops <- GET(paste0("https://bt3103-ef12e.firebaseio.com/stops.json"))
  stops <- content(stops, as="parsed")
  
  idOfStops <- c()
  namesOfStops <- c()
  counter = 1
  for (i in (stops)){
    namesOfStops[[counter]] <- i$stopName
    idOfStops[[counter]] <- i$stopID
    counter <- counter + 1
  }
  return (namesOfStops)
}

#### Author: Wee Seng
#### Desc: Create DP table for user query
updateServiceMatrix <- function(){
  # Initialise Bus Service Matrix
  current<-matrix(ncol=30, nrow=30)
  for(i in 1:ncol(current)){
    for (j in 1:nrow(current)){
      if(i == j){
        current[i,j] = "HELP LA!"
      } else {
        current[i,j] = "Off"
      }
    }
  }
  
  route <- content(GET(paste0("https://bt3103-ef12e.firebaseio.com/latestRoutes.json")), as="parsed")
  busServices <- names(route)
  
  #initialize the data frame to store the bus stops
  currentRoute <- data.frame(matrix(ncol = length(busServices)))
  for (i in length(busServices)){
    names(currentRoute) <- busServices
  }
  
  currentList = list()
  futureList = list()
  
  #Obtaining the route for each bus service
  for(i in 1:length(busServices)){
    date1 = route[[i]][[1]]$effDate
    date2 = route[[i]][[2]]$effDate
    currentDate <- Sys.Date()
    routeStops = list()
    futureStops = list()
    numFutureStops <- list()
    
    if(date1>date2){
      if(currentDate > date1){
        numStops <- route[[i]][[1]]$stops[2:length(route[[i]][[1]]$stops)]
        numFutureStops <- NULL
        for(j in length(numStops)){
          routeStops[j] <- numStops[[j]]
          #no stops will be added to futureStops till a newer route is updated
          futureStops = NULL 
        }
      } else if (date1> currentDate & currentDate >date2){
        numStops <- route[[i]][[2]]$stops[2:length(route[[i]][[1]]$stops)]
        numFutureStops <- route[[i]][[1]]$stops[2:length(route[[i]][[1]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- route[[i]][[2]]$stops[[j]]
        }
        for(a in length(numFutureStops)){
          futureStops[a] <- route[[i]][[1]]$stops[[a]]
        }
      }
    } else {
      if(currentDate > date2){
        numStops <- route[[i]][[2]]$stops[2:length(route[[i]][[1]]$stops)]
        numFutureStops <- NULL
        for(j in length(route[[i]][[2]]$stops)){
          routeStops[j] <- route[[i]][[2]]$stops[[j]]
          futureStops = NULL
        }
      } else if (date2> currentDate & currentDate >date1){
        numFutureStops <- route[[i]][[2]]$stops[2:length(route[[i]][[1]]$stops)]
        numStops <- route[[i]][[1]]$stops[2:length(route[[i]][[1]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- route[[i]][[1]]$stops[[j]]
        }
        for(a in length(numFutureStops)){
          futureStops[j] <- route[[i]][[2]]$stops[[a]]
        }
      }
    }
    #initialize the list of stops for both current and future routes
    listStops <-list()
    listFutureStops <- list()
    
    #for current route
    for(k in 1:length(numStops)){
      listStops[k] <- numStops[k]
    }
    if(i == 1){
      currentList <- cbind(currentList, listStops)
    } else {
      currentList <- qpcR:::cbind.na(currentList, listStops)
    }
    
    #for future routes
    for(b in 1:length(numFutureStops)){
      listFutureStops[b] <- numFutureStops[b]
    }
    if(i == 1){
      futureList <- cbind(futureList, listFutureStops)
    } else {
      futureList <- qpcR:::cbind.na(futureList, listFutureStops)
    }
  }
  
  #Initialize the dataframe heading
  busColNames<-setNames(data.frame(matrix(ncol = length(busServices), nrow = 0)), c(busServices))
  
  currentList <- rbind(busColNames, currentList)
  colnames(currentList) <- busServices
  
  futureList <- rbind(busColNames, futureList)
  colnames(futureList) <- busServices
  
  #futureRoute <- futureRoute$stops
  #futureRoute[1] <- NULL
  
  #obtaining the bus stop ID
  stops <- GET(paste0("https://bt3103-ef12e.firebaseio.com/stops.json"))
  stops <- content(stops, as="parsed")
  
  idOfStops <- c()
  namesOfStops <- c()
  counter = 1
  for (i in (stops)){
    namesOfStops[[counter]] <- i$stopName
    idOfStops[[counter]] <- i$stopID
    counter <- counter + 1
  }
  
  #filling in the matrix with the bus services
  serviceNumber = 1
  for(i in 1:length(currentList)){ 
    stops <- c()
    for (j in 1:length(currentList[[i]])){
      if (is.na(currentList[[i]][[j]])){
        #skip if there are no more bus stops
      } else {
        stops <- append(stops, currentList[[i]][[j]])
      }
    }
    busStop <- match(stops, namesOfStops)
    for (m in 1:length(busStop)){
      for (n in 1:length(busStop)){
        if (m>=n){ 
          
        } else {
          if(isTRUE(current[busStop[m],busStop[n]] == "Off")){
            current[busStop[m],busStop[n]] <- busServices[serviceNumber]
          } else {
            if (busStop[m] == busStop[n]){
            } else {
              current[busStop[m],busStop[n]] <- busServices[serviceNumber]
            }
          }
        }
      }
    }
    #change of bus service number
    serviceNumber = serviceNumber + 1
  }
  return (t(current))
}

#### Author: Wee Seng
#### Desc: Retrieve bus service from start and end bus stop IDs
retrieveService<- function(start,end){
  if(length(current[start, end] > 1)){
    return (current[start, end][[1]]) #how to determine which bus to return first
  } else {
    return(current[start, end])
  }
}


