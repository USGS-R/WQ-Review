tabItem(tabName = "wideDataTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Wide data table"),
                        sidebarPanel(
                                selectInput("siteSel_wideDataTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_wideDataTable", "Start date", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_wideDataTable", "End date", 
                                          value=Sys.Date()),
                                ###sidebar options
                                width=3
                        ),
                        mainPanel(
                                downloadButton('wideDataTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("wideDataTable")
                        )
                )
        )
)