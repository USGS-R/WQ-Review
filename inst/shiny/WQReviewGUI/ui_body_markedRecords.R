###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "markedRecordsTab",
        fluidPage(
                headerPanel("Blank summary table"),
                downloadButton('markedRecordsOut', 'Download tab delimited table'),
                actionButton(inputId = "refreshMarkedRecords",label="Refresh table"),
                
                DT::dataTableOutput("markedRecords")
        )
)
  