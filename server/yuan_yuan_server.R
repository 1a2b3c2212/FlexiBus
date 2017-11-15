###########################
#######Yuan Yuan###########
###########################

#output datatable
r <- GET("https://bt3103-ef12e.firebaseio.com/routes.json")
raise <- content(r, as = "text")
rawdata  <- fromJSON(raise)
#output$info <- DT::renderDataTable(
#  getDistanceData(rawdata[[2]], as.character(input$startdate), as.character(input$enddate))
#)
#output$dist <- renderDataTable(
#  eachDay(getDistanceData(rawdata[[2]], as.character(input$startdate), as.character(input$enddate)), as.character(input$startdate), as.character(input$enddate))
#)
#output$info <- renderDataTable(
#  allRoutesdf(rawdata, as.character(input$startdate), as.character(input$enddate))
#) 

#myts <- allRoutesdf(rawdata, as.character('2015-01-01'), as.character('2016-12-31'))
#info_ts = ts(myts[, c('Distance')], frequency = 7)
#fit <- HoltWinters(info_ts, beta=FALSE, gamma = FALSE) #with seasonality

#error handle: when the input date range is less than 2 years, message will be returned to remind user to increase the range of input   
output$prediction <- renderPlot({
  shiny::validate(
    need(as.double(as.Date(input$daterange_yy[2]) - as.Date(input$daterange_yy[1])) > 729,'Oops! Please select at least 2 year data')
  )
  plot(forecast(
    HoltWinters(
      ts(
        as.ts(
          with(
            allRoutesdf(
              rawdata, as.character(input$daterange_yy[1]), as.character(input$daterange_yy[2])),
            zoo(Distance, order.by = Date))), frequency = 365)), 30), xlab = "Years")
})
#YUANYUAN END#