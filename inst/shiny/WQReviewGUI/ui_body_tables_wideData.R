tabItem(tabName = "wideDataTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Wide data table"),
                        sidebarPanel(
                                selectInput("siteSel_wideDataTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_wideDataTable", "Start date for blank summary", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_wideDataTable", "end date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('wideDataTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("wideDataTable"),
                                verbatimTextOutput("sitenoTest")
                        )
                )
        )
)