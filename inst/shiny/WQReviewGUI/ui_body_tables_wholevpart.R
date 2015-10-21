tabItem(tabName = "wholvevpartTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Whole vs part summary table"),
                        sidebarPanel(
                                selectInput("siteSel_wholevpartTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_wholevpartTable", "Start date", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_wholevpartTable", "End date", 
                                          value=Sys.Date()),
                                ###sidebar options
                                width=3
                        ),
                        mainPanel(
                                downloadButton('wholevpartTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("wholevpartTable")
                        )
                )
        )
)