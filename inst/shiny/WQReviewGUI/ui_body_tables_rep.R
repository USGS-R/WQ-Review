tabItem(tabName = "repTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Replicate summary table"),
                        sidebarPanel(
                                selectInput("siteSel_repTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_repTable", "Start date", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_repTable", "End date", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('repTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("repTable")
                        )
                )
        )
)