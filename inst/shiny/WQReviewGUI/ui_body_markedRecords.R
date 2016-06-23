###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


 tabItem(tabName = "markedRecordsTab",


        fluidPage(
                pageWithSidebar(

                        headerPanel("Reviewer notes and DQI Codes"),
                        sidebarPanel(
                                ###Controls items
                                actionButton(inputId = "refreshMarkedRecords",label="View/Refresh Notes"),
                                actionButton(inputId = "flipDQI",label="Make DQI Batch files"),
                                ###Sidebar options
                                width=3
                        ),
                        mainPanel(
                                box(
                                        downloadButton('markedRecordsOut', 'Download tab delimited table'),
                                        DT::dataTableOutput("markedRecords"),
                                        collapsible = TRUE
                                        ),
                                box(
                                        h2("QWSample"),
                                        downloadButton('qwSampleOut',"Download qwsample file"),
                                        DT::dataTableOutput("qwSample"),
                                        collapsible = TRUE
                                ),
                                box(
                                        h2("QWResult"),
                                        downloadButton('qwResultOut',"Download qwresult file"),
                                        DT::dataTableOutput("qwResult"),
                                        collapsible = TRUE
                                )
                        )
                )
        )
)
