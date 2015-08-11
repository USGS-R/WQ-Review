tabItem(tabName = "wideDataTable",
        fluidPage(
                downloadButton('wideDataTableOut', 'Download tab delimited table'),
                DT::dataTableOutput("wideDataTable")
                )
)
