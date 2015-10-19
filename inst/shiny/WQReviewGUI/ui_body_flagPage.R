###This contains all the ui elements for the parmParm plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.

tabItem(tabName = "flagPage",
        fluidPage(
                        headerPanel("Automated sample and result checks"), 

                         
                       

                        box(title = "Sample Level Flags",
                            downloadButton('sampleFlagTableOut', 'Download tab delimited table'),
                            DT::dataTableOutput("sampleFlagTable"),
                            ###Box options
                            width=12,
                            collapsible=TRUE),
                        box(title = "Result Level Flags",
                            downloadButton('resultFlagTableOut', 'Download tab delimited table'),
                            DT::dataTableOutput("resultFlagTable"),
                            ###Box options
                            width=12,
                            collapsible=TRUE)
                        
                )
        )

