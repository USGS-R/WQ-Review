tabItem(tabName = "repTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Replicate summary table"),
                        sidebarPanel(
                                selectInput("siteSel_repTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_repTable", "Start date for blank summary", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_repTable", "end date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('repTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("repTable")
                        )
                )
        )
)