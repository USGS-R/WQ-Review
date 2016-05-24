###This contains all the ui elements for the parmParm plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.

tabItem(tabName = "resultFlagTable",
        fluidPage(
                headerPanel("Result-level checks"),
                helpText(a("Click Here for description of checks",
                           target="_blank",href="flagDescriptions.htm"
                )
                ),
                
                downloadButton('resultFlagTableOut', 'Download tab delimited table'),
                DT::dataTableOutput("resultFlagTable")
                
                
        )
)

