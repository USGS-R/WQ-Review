tabItem(tabName = "wholvevpartTable",
        fluidPage(
                downloadButton('wholevpartTableOut', 'Download tab delimited table'),
                
                DT::dataTableOutput("wholevpartTable")
        )
)