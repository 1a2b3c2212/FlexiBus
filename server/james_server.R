#####################################################
#######James Liu Song Yu Descriptive Analytics#######
#####################################################

#two reactive values for start date and end date value
dateRange<-reactive(input$daterange1)
dateRange2<-reactive(input$daterange1)

output$plot<-renderPlot({
  #startbusstops
  minDateRangeStart<-as.POSIXct(as.character(dateRange2()[1]))
  maxDateRangeStart<-as.POSIXct(as.character(dateRange2()[2]))
  
  #if date range are the same
  #plots the descriptive demand for start bus stops
  if (minDateRangeStart==maxDateRangeStart){
    testStart<-subset(summaryTable,summaryTable$StartDates==minDateRangeStart)
    test3<-as.data.frame(table(testStart$StartLocation), stringsAsFactors= F)
    PopularrBusStops<-test3$Freq
    PopularStartBusStops<-PopularrBusStops<mean(test3$Freq)
    cols<-c("red","green")
    ggplot(data=test3,aes(x=test3$Var1, y=test3$Freq)) + geom_bar(stat = 'identity', aes(fill=PopularStartBusStops))+scale_fill_manual(values=cols,labels=c("Popular","Less Popular")) + xlab("Start Bus Stop Locations") +ylab("Count") + ggtitle("Demand for Start Bus Stops") +theme_bw() + theme(axis.text.x=element_text(angle = 90, hjust = 1))
  }
  #else, if date range are different
  #plots the descriptive demand for start bus stops
  else{
    testStart<-subset(summaryTable,summaryTable$StartDates<maxDateRangeStart & summaryTable$StartDates>minDateRangeStart)
    test3<-as.data.frame(table(testStart$StartLocation), stringsAsFactors= F)
    PopularrBusStops<-test3$Freq
    PopularStartBusStops<-PopularrBusStops<mean(test3$Freq)
    cols<-c("red","green")
    ggplot(data=test3,aes(x=test3$Var1, y=test3$Freq)) + geom_bar(stat = 'identity', aes(fill=PopularStartBusStops))+scale_fill_manual(values=cols,labels=c("Popular","Less Popular")) + xlab("Start Bus Stop Locations") +ylab("Count") + ggtitle("Demand for Start Bus Stops") +theme_bw() + theme(axis.text.x=element_text(angle = 90, hjust = 1))
  }
})

#function to display number of people via cursor hovering above the start bus stop graph
output$info<-renderText({
  xy_str<-function(e){
    if(is.null(e)) return ("0\n")
    paste0(round(e$y,1), "\n")
  }
  paste0(
    "Number of people:",xy_str(input$plot_hover)
  )
})

output$plot2<-renderPlot({
  #endbusstops
  minDateRange<-as.POSIXct(as.character(dateRange()[1]))
  maxDateRange<-as.POSIXct(as.character(dateRange()[2]))
  #if date range are the same
  #plots the descriptive demand for end bus stops
  if (minDateRange==maxDateRange){
    test<-subset(summaryTable,summaryTable$EndDates==minDateRange)
    test2<-as.data.frame(table(test$EndLocation), stringsAsFactors= F)
    PopularBusStops<-test2$Freq
    PopularEndBusStops<-PopularBusStops<mean(test2$Freq)
    cols<-c("red","green")
    
    ggplot(data=test2,aes(x=test2$Var1, y=test2$Freq)) + geom_bar(stat = 'identity', aes(fill=PopularEndBusStops))+scale_fill_manual(values=cols,labels=c("Popular","Less Popular")) + xlab("End Bus Stop Locations") +ylab("Count") + ggtitle("Demand for End Bus Stops") +theme_bw() + theme(axis.text.x=element_text(angle = 90, hjust = 1))
  }
  #else, if date range are different
  #plots the descriptive demand for end bus stops
  else{
    test<-subset(summaryTable,summaryTable$EndDates<maxDateRange & summaryTable$EndDates>minDateRange)
    test2<-as.data.frame(table(test$EndLocation), stringsAsFactors= F)
    PopularBusStops<-test2$Freq
    PopularEndBusStops<-PopularBusStops<mean(test2$Freq)
    cols<-c("red","green")
    ggplot(data=test2,aes(x=test2$Var1, y=test2$Freq)) + geom_bar(stat = 'identity', aes(fill=PopularEndBusStops))+scale_fill_manual(values=cols,labels=c("Popular","Less Popular")) + xlab("End Bus Stop Locations") +ylab("Count") + ggtitle("Demand for End Bus Stops") +theme_bw() + theme(axis.text.x=element_text(angle = 90, hjust = 1))
  }
})

#function to display number of people via cursor hovering above the end bus stop graph
output$info2<-renderText({
  xy_strr<-function(e){
    if(is.null(e)) return ("0\n")
    paste0(round(e$y,1), "\n")
  }
  paste0(
    "Number of people:",xy_strr(input$plot2_hover)
  )
})

####################################################
#######James Liu Song Yu Predictive Analytics#######
####################################################


#reactive values for user input bus stops
stopsreactivevalues<-reactive(input$stops)

#reactive values for number of months to predict
utownreactivevalues<-reactive(input$sliderUT)

#using the modularised start predict time series function to avoid long codes

output$SummaryUT<-renderPlot({
  graphtitle<-paste0("Start Location: ",stopsreactivevalues())
  predictstartfunction(location = stopsreactivevalues(),values = utownreactivevalues, nameofGraph = graphtitle)
})

#using the modularised end predict time series function to avoid long codes
output$SummaryUT2<-renderPlot({
  endlocationgraphtitle<-paste0("End Location: ",stopsreactivevalues())
  predictendfunction(location = stopsreactivevalues(),values = utownreactivevalues, nameofGraph = endlocationgraphtitle)
})

##JAMES LIU SONG YU END##