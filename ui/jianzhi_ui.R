tabPanel("Modify Bus Route", icon = icon("pencil"),
         h1("Modify Bus Routes",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#101882"),
         uiOutput("routeParameters"),
         fluidRow(
           column(3,selectInput("service","Service",returnServiceList())),
           column(3,dateInput("effDate", "Effective Date")),
           column(3,textInput("numCycle","Cycles/Day")),
           column(3,textInput("numBuses","Buses Allocated")),
           
           column(12, actionButton("update", "Update"))
         ),
         fluidRow(
           column(6,strong("Modified Route")),
           column(6,strong("Original Route"))
         ),
         fluidRow(
           column(6,tableOutput('table')),
           column(6,tableOutput('originalTable'))
         ),
         fluidRow(
           column(3,selectInput("stop","Stops",returnStops())),
           column(3,textInput("stopNo","Insert after stop:")),
           column(6,uiOutput("stopCount")),
           column(6,actionButton("add", "Add Stop")),
           column(6,actionButton("deleteStop", "Delete"))
         ),
         textOutput("distance"),
         google_mapOutput(outputId = "routeMap")
         
)