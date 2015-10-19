tabItem(tabName = "balanceTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Chargebalance table"),
                        sidebarPanel(
                                selectInput("siteSel_balanceTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_balanceTable", "Start date", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_balanceTable", "End date", 
                                          value=Sys.Date()),
                                ###sidebar options
                                width=3
                        ),
                        mainPanel(
                                downloadButton('BalanceDataTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("balanceTable")
                        )
                )
        )
)