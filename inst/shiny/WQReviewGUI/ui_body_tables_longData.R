tabItem(tabName = "longDataTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Long data table"),
                        sidebarPanel(
                                selectInput("siteSel_longDataTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_longDataTable", "Start date for blank summary", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_longDataTable", "end date for blank summary", 
                                          value=Sys.Date())
                        ),
                        mainPanel(
                                downloadButton('longDataTableOut', 'Download tab delimited table'),
                                
                                DT::dataTableOutput("longDataTable")
                        )
                )
        )
)