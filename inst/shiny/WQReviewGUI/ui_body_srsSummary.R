tabItem(tabName = "srsSummary",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Batch file generator"),
                        sidebarPanel(
                                downloadButton('srsTemplate', 'Download example template'),
                                h4("Use Excel template for formatting requirements. Each tab must be saved
                                   as a separate .csv file."),
                                
                                fileInput("srsResults",label="SRS lab results (.csv)",accept="text/csv"),
                                fileInput("srsValues",label="SRS MPVs (.csv)",accept="text/csv"),
                                fileInput("srsLRLs",label="SRS reporting levels (.csv)",accept="text/csv"),
                                
                                                
                                actionButton(inputId = "srsSummary",label="Generate SRS summary")
                        ),
                        mainPanel(
                                box(
                                        DT::dataTableOutput("srsSummary"),
                                        
                                        ###Box options
                                        width=12,
                                        collapsible=TRUE,
                                        title="SRS Summary")
                        )
                )
        )
)