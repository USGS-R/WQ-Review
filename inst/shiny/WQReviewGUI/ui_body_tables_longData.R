
tabItem(tabName = "longDataTable",
        fluidPage(
                downloadButton('longDataTableOut', 'Download tab delimited table'),
                
                DT::dataTableOutput("longDataTable")
        )
)

