tabPanel("Bus Route Suggestion", icon = icon("map-marker"),
         h1("Bus Route Suggestions",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:#101882"),
         fluidRow(
           tabBox(
             title = "",
             id = "tabset1", width = "NULL",
             # Tab 1 - Introduction for bus operator
             tabPanel("Introduction", icon = icon("info"),
                      h2("Introduction",style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:	#EA5421"),
                      verbatimTextOutput("intro_info"),
                      h3("Common Terms Used:", style = "text-align: center; color:	#EA5421"),
                      h4("Transaction:"),
                      verbatimTextOutput("transaction_info"),
                      h4("Rule:"),
                      verbatimTextOutput("rules_info"),
                      h4("Support"),
                      verbatimTextOutput("support_info"),
                      h4("Confidence:"),
                      verbatimTextOutput("confidence_info"),
                      h4("Lift:"),
                      verbatimTextOutput("lift_info") 
             ),
             # Tab 2 - View association rule mining table
             tabPanel("Table", icon = icon("table"),
                      h2("Association Rule Mining Table", style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:	#EA5421"),
                      dataTableOutput("ARM_table")),
             # Tab 3 - View scatter plots
             tabPanel("Plot", icon = icon("line-chart"), 
                      h2("Plot:", style = "font-family: Comic Sans; text-align: center; font-weight:bold; color:	#EA5421"),
                      actionButton("action_support", "Support"),
                      actionButton("action_confidence", "Confidence"), 
                      actionButton("action_lift", "Lift"),
                      plotlyOutput("graph")
             )
           )
           
         )
)