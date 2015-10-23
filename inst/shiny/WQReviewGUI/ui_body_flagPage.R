###This contains all the ui elements for the parmParm plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.

tabItem(tabName = "flagPage",
        fluidPage(
                        headerPanel("Automated sample and result checks"),
                        helpText(a("Click Here for description of checks",
                                   target="_blank",href="flagDescriptions.htm"
                                   )
                                 ),


                         
                       

                        box(title = "Basic Chemical Flags",
                            downloadButton('chemFlagTableOut', 'Download tab delimited table'),
                            DT::dataTableOutput("chemFlagTable"),
                            ###Box options
                            width=12,
                            collapsible=TRUE),
                        box(title = "Pesticide Flags",
                            downloadButton('pestFlagTableOut', 'Download tab delimited table'),
                            DT::dataTableOutput("pestFlagTable"),
                            ###Box options
                            width=12,
                            collapsible=TRUE),
                        box(title = "Result Flags",
                            downloadButton('resultFlagTableOut', 'Download tab delimited table'),
                            DT::dataTableOutput("resultFlagTable"),
                            ###Box options
                            width=12,
                            collapsible=TRUE)
                        
                )
        )

