tabPanel("Bus Stop Demand", icon = icon("bar-chart"),
         h1("Bus Stops Demand",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#101882"),
         fluidRow(
           tabBox(
             title = "",
             id="tabsetjames", width = "NULL",
             tabPanel("Demand", icon = icon("bus"),
                      h2("Demand for Start and End Bus Stops",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#EA5421"),
                      # Date input
                      dateRangeInput("daterange1", "Date range:",
                                     start = "2015-01-01", end = "2017-12-31",
                                     min = "2015-01-01",
                                     max = "2017-12-31", format = "yyyy-mm-dd",
                                     startview = "month", weekstart = 0, 
                                     language = "en",separator = " to ", width = "500px"),
                      plotOutput("plot",hover="plot_hover"),
                      verbatimTextOutput("info"),
                      plotOutput("plot2", hover="plot2_hover"),
                      verbatimTextOutput("info2")
             ),
             tabPanel("Forecasting Demand", icon = icon("bar-chart"),
                      h2("Demand Prediction for Bus Stops",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#EA5421"),
                      
                      sidebarPanel(
                        selectInput("stops", "Bus Stops:", 
                                    choices=c("Kent Ridge Bus Terminal","PGP","Computer Centre","House 7", "Opp. House 12", "Opp. University Health Centre", "University Hall", "LT29", "House 15", "House 12", "Opp. Hon Sui Sen Memorial Library", "Opp. NUSS", "Ventus (Opp. LT13)", "Museum","University Health Centre", "Opp. KR MRT Station","KR MRT Station ", "Raffles Hall" ,"EA", "Carpark 11 (BIZ)", "Central Library",
                                              "University Town","Opp.Yusof Ishak House","Yusof Ishak House", "LT13", "AS7","BIZ2","COM2"))
                        ,sliderInput("sliderUT","Number of months to predict",min=1,max=24,value=0),
                        
                        helpText("Bus Operations As Of 2017")),
                      mainPanel(
                        plotOutput("SummaryUT"),
                        plotOutput("SummaryUT2")
                      )
             ) 
           )
         )
)