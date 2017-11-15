serviceList <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Service"))

namesOfStops <- returnNamesOfStops()

for (i in 1:length(summaryTable$EndLocation)){
  start <- summaryTable$StartLocation[[i]]
  end <- summaryTable$EndLocation[[i]]
  startID = 0
  endID = 0
  for(j in 1:length(namesOfStops)){
    if (namesOfStops[j] == start){
      startID = j
      break
    } 
  }
  for(k in 1:length(namesOfStops)){
    if (namesOfStops[k] == end){
      endID = k
      break
    } 
  }
  service <- retrieveService(startID, endID)
  if(startID == 0 || endID == 0){
    service = "Off"
  }
  serviceList <- rbind(serviceList, service, stringsAsFactors=F)
}

serviceSummaryTable <- cbind(summaryTable, serviceList)
colnames(serviceSummaryTable)[5] <- "Service"

################
### Wee Seng ###
################

dateRange<-reactive(input$daterange_ws)

output$demandGraph <- renderPlot({
  
  startDateRange<-as.POSIXct(as.character(dateRange()[1]))
  endDateRange<-as.POSIXct(as.character(dateRange()[2]))
  
  if(startDateRange == endDateRange){
    filteredSummary <- subset(serviceSummaryTable, serviceSummaryTable$StartDates == startDateRange)
    demandTable <- as.data.frame(table(filteredSummary$Service), stringsAsFactor = F)
    # filter the helpla,off,na rows
    demandTable <- demandTable[!(demandTable$Var1 == "HELP LA!" | demandTable$Var1 == "Off" | is.na(demandTable$Var1)),]
    ggplot(data = demandTable, aes(x=demandTable$Var1, y=demandTable$Freq)) + geom_bar(stat = "identity") + xlab("Bus Services") + ylab("Count") + ggtitle("Demand for Bus Service") 
  } else {
    filteredSummary <- subset(serviceSummaryTable, serviceSummaryTable$StartDates < endDateRange & summaryTable$StartDates > startDateRange)
    demandTable <- as.data.frame(table(filteredSummary$Service), stringsAsFactor = F)
    # filter the helpla,off,na rows
    demandTable <- demandTable[!(demandTable$Var1 == "HELP LA!" | demandTable$Var1 == "Off" | is.na(demandTable$Var1)),]
    ggplot(data = demandTable, aes(x=demandTable$Var1, y=demandTable$Freq)) + geom_bar(stat = "identity") + xlab("Bus Services") + ylab("Count") + ggtitle("Demand for Bus Service")
  }
  
})