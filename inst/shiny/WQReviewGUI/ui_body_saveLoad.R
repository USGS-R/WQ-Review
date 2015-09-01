###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "saveLoadTab",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Data download criteria"),
                        sidebarPanel(
                                downloadButton('saveData', 'Save imported data'),
                                fileInput("loadDataFile",label="Load saved data",accept=".gz"),
                                actionButton(inputId = "loadData",label="Load data")

                        ),
                        mainPanel(
                                h3(textOutput("loadWarning"))

                        )
                )
        )
)
