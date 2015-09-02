tabItem(tabName = "wholvevpartTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Whole vs part summary table"),
                        sidebarPanel(
                                selectInput("siteSel_wholevpartTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_wholevpartTable", "Start date for blank summary", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_wholevpartTable", "end date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('wholevpartTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("wholevpartTable")
                        )
                )
        )
)