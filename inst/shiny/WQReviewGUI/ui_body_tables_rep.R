
tabItem(tabName = "repTable",
        fluidPage(
                downloadButton('repTableOut', 'Download tab delimited table'),
                
                DT::dataTableOutput("repTable")
        )
)

