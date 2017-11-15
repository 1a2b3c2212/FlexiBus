#########################
########Jianzhi##########
#########################

service <- reactive(input$service)
stop <- reactive(input$stop)
originalRoute <- reactive(returnLatestRoute(tolower(input$service)))
copyRoute <- isolate(originalRoute())
tables <- reactiveValues()
tables$route <- copyRoute
modifiedTable <- reactive(returnRouteTable(tables$route))

# Event handling for pressing of update Route button
observeEvent(input$update, {
  #push out older record in latestRoutes to routes document
  
  #prepare document for insertion
  stopList <- tables$route
  names(stopList) <- seq_len(length(stopList))
  
  updatedRoute <- toJSON(list(
    effDate=input$effDate,
    numCycle=sample(10:20,1),
    numBus=input$numBuses,
    totalDist=sample(8000:16000,1),
    stops=stopList
  ),
  auto_unbox = TRUE
  )
  
  print(updatedRoute)
  #insert into latestRoutes
  POST(paste0("https://bt3103-ef12e.firebaseio.com/latestRoutes/",tolower(service()),".json"), body = updatedRoute, encode="json" )
  
  
  
  # show confirmation msg
  showModal(modalDialog(
    fluidRow(
      column(12, paste("Successfully modified route with parameters:")),
      column(12, paste("Service: ", input$service)),
      column(12, paste("Effective Date: ", input$effDate)),
      column(12, paste("Number of Cycles: ", input$numCycle)),
      column(12, paste("Number of Buses Allocated: ", input$numBuses))
    ),
    title = "Modify Routes"
  ))
})



observeEvent(input$service, {
  # insert into route
  copyRoute <- isolate(originalRoute())
  tables$route <- copyRoute
  
  map_key <- google_java_key
  df_polyline <- data.frame(polyline = getPolyline(tables$route))
  google_map_update(map_id = "routeMap") %>%
    clear_polylines() %>%
    add_polylines(data = df_polyline, polyline = "polyline", stroke_weight = 5)
})
# Output the route of a busID
output$route <- renderText({
  route_text <- as.character(returnLatestRoute(input$service))
  route_text
})

output$table <- renderTable(modifiedTable(),digits=0)

output$originalTable <- renderTable(returnRouteTable(returnLatestRoute(input$service)),digits=0)


output$distance <- renderText({
  print(paste("Service is:",input$service))
  d <- totalDistanceOfRoute(tables$route)
  d <- as.character(d)
  paste("Total distance: ",d)
})

output$routeMap <- renderGoogle_map({
  print("Rendering")
  df_polyline <- data.frame(polyline = getPolyline(tables$route))
  # df_path <- decode_pl(getPolyline(tables$route))
  # View(df_path)
  map_key <- google_java_key
  
  google_map(key = map_key) %>%
    add_polylines(data = df_polyline, polyline = "polyline", stroke_weight = 5)
})

observeEvent(input$add, {
  # insert into route
  tables$route <- append(tables$route,input$stop,as.numeric(input$stopNo))
  
  map_key <- google_java_key
  df_polyline <- data.frame(polyline = getPolyline(tables$route))
  google_map_update(map_id = "routeMap") %>%
    clear_polylines() %>%
    add_polylines(data = df_polyline, polyline = "polyline", stroke_weight = 5)
  
})

output$stopCount <- renderUI({
  selectInput("numStops","Delete Stop",seq_len(length(tables$route)))
})

observeEvent(input$deleteStop, {
  tables$route[[as.numeric(input$numStops)]] <- NULL
  map_key <- google_java_key
  df_polyline <- data.frame(polyline = getPolyline(tables$route))
  google_map_update(map_id = "routeMap") %>%
    clear_polylines() %>%
    add_polylines(data = df_polyline, polyline = "polyline", stroke_weight = 5)
  
})