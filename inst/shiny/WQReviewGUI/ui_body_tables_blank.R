tabItem(tabName = "blankTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Blank summary table"),
                        sidebarPanel(
                                selectInput("siteSel_blankTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_blankTable", "Start date for blank summary", 
                                               value=Sys.Date() - 365*3),
                                dateInput("endDate_blankTable", "End date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('blankTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("blankTable")
                        )
                )
        )
)

