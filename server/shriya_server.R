
###############################
###Shriya's Data Preparation###
###############################
# Convert summary table to csv so that it can be read. 
write.csv(summaryTable, file = "summaryTable.csv")

# Convert csv to transactions data type so that apriori algorithm can be used
trans <- read.transactions("summaryTable.csv",
                           format = "single",
                           sep = ",",
                           cols = c("StartLocation", "EndLocation"))

#Use apriori algorithm and subset to show all rules with lift > 1  
rules <- apriori(trans ,parameter = list( support = 0.5, confidence = 0.5,
                                          minlen = 2, maxlen = 5))
rules <- subset(rules, subset = lift > 1)


# Convert rules to data table, change column names and round off values to 2dp
rules_dt <- data.table( Left = labels( lhs(rules) ), 
                        Right = labels( rhs(rules) ), 
                        quality(rules))
names(rules_dt) <- c("Left-Hand Side","Right-Hand Side","Support","Confidence","Lift","Count" )
cols <- names(rules_dt)[3:5]
rules_dt[,(cols) := round(.SD,2), .SDcols=cols]
DT::datatable(rules_dt) 

# Sort rules according to support, confidence and lift 
# Then, show first 1000 rules
rules_lift <- sort(rules, by = "lift")[1:1000,]
rules_support <- sort(rules, by = "support")[1:1000,]
rules_confidence <- sort(rules, by = "confidence")[1:1000,]


#########################
#######Shriya's ARM######
#########################

options(warn = -1)
# Introduction Text:
output$intro_info <- renderText({
  paste("This is to help identify associations between bus stops so that bus routes can be identified.
        ",
        "Support, confidence, and lift are used to understand the strength of the association between the bus stops. ",
        sep="\n")
})
output$transaction_info <- renderText({
  paste("It is a set of bus stops with a minimum of 2 bus stops and a maximum of 5 bus stops. Each bus stop can belong to multiple transactions.")
})
output$rules_info <- renderText({
  paste("A rule is the relationship between two bus stops, X and Y. X -> Y means that if we have X in a transaction, then we have Y in the same transaction.
        ", 
        "X will be the left-hand side and Y will be the right-hand side of the rule.", sep="\n")
})
output$support_info <- renderText({
  paste("Support is the probability of finding a bus stop or a set of bus stops across all transactions. The support value lies between 0 and 1. 
        ", 
        "For example, the support of bus stops X and Y = Number of times both bus stops are found in the data set/total number of transactions",
        sep="\n")
})
output$confidence_info <- renderText({
  paste("Confidence is the probability to find a bus stop or a set of bus stops given that a particular bus stop is in the same transaction. The confidence 
        value lies between 0 and 1. 
        ", 
        "For example, the support of bus stops X and Y = Number of times X and Y are found across all transactions/Number of times Y is found.",
        sep="\n")
})
output$lift_info <- renderText({
  paste("Lift = The support of the itemset grouping of bus stop X and Y/Support of Y. The lift value can be any positive real number.
        ",
        "A value of 1 means there is no effect and the set of bus stops are independent to each other.
        ", 
        "A lift lower than 1 means there is a negative effect of A on B (or B on A) and hence the rule is not as significant. Hence, there is no point having a bus route for bus stops with a lift less than or equal to 1. 
        ",
        "A lift greater than 1 implies a positive effect of bus stop A on B (or B on A) and therefore the rule is significant. These rules should then be considered when planning the bus routes.
        ",
        "In this application, only rules with a lift greater than 1 will be shown. ",
        sep="\n")
})


# Display ARM table and filter it
output$ARM_table <- renderDataTable({ 
  rules_dt
},
filter = 'top',
rownames = TRUE)

# For support actionButton
observeEvent(input$action_support, {
  output$graph<- renderPlotly(plotly_arules(rules_support, jitter = 10, 
                                            marker = list(opacity = .7, size = 10, symbol = 1), 
                                            colors = c("blue", "orange"), engine = "htmlwidget")
                              %>% layout(title = "Support:"))
})
# For confidence actionButton
observeEvent(input$action_confidence, {
  output$graph<- renderPlotly(plotly_arules(rules_confidence, jitter = 10, 
                                            marker = list(opacity = .7, size = 10, symbol = 1), 
                                            colors = c("blue", "orange"), engine = "htmlwidget")
                              %>% layout(title = "Confidence:"))
})
# For lift actionButton
observeEvent(input$action_lift, {
  output$graph<- renderPlotly(plotly_arules(rules_lift, jitter = 10, 
                                            marker = list(opacity = .7, size = 10, symbol = 1), 
                                            colors = c("blue", "orange"), engine = "htmlwidget")
                              %>% layout(title = "Lift:"))
})

