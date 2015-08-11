tabItem(tabName = "cbTable",
        fluidPage(
                downloadButton('BalanceDataTableOut', 'Download tab delimited table'),
                
                DT::dataTableOutput("cbTable")
        )
)

