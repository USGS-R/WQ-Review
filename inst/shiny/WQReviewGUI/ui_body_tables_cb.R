tabItem(tabName = "balanceTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Chargebalance table"),
                        sidebarPanel(
                                selectInput("siteSel_balanceTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_balanceTable", "Start date for blank summary", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_balanceTable", "end date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('BalanceDataTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("balanceTable")
                        )
                )
        )
)