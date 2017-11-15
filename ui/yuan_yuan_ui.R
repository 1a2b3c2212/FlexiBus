tabPanel("Forecast", icon = icon("line-chart"),
         h2("Forecasting Daily Travel Distance "),
         fluidRow(
           
           dateRangeInput("daterange_yy", "Date range:",
                          start = "2015-01-01", end = "2016-12-31",
                          min = "2015-01-01",
                          max = "2017-12-31", format = "yyyy-mm-dd",
                          startview = "month", weekstart = 0, 
                          language = "en",separator = " to ", width = "500px"),
           
           plotOutput("prediction")
         )
         
         
)