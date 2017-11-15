tabPanel("Bus Service Usage", icon = icon("area-chart"),
         h1("Bus Service Usage",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#101882"),
         fluidRow(
           dateRangeInput("daterange_ws", "Date range:",
                          start = "2015-01-01", end = "2017-12-31",
                          min = "2015-01-01",
                          max = "2017-12-31", format = "yyyy-mm-dd",
                          startview = "month", weekstart = 0, 
                          language = "en",separator = " to ", width = "500px"),
           plotOutput("demandGraph")
         )
)