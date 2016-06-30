tabItem(tabName = "longDataTable",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Long data table"),
                        sidebarPanel(
                                selectInput("siteSel_longDataTable","Station",choices="",multiple=TRUE),
                                dateInput("startDate_longDataTable", "Start date", 
                                          value=Sys.Date() - 365*3),
                                dateInput("endDate_longDataTable", "End date", 
                                          value=Sys.Date()),
                                ###sidebar options
                                width=3),
                        mainPanel(
                                downloadButton('longDataTableOut', 'Download tab delimited table'),
                                actionButton(inputId = "longDataTable_popNotes",label="Add to notes"),
                                
                                DT::dataTableOutput("longDataTable")
                        )
                )
        )
)