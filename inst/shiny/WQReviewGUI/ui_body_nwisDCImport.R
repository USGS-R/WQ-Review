###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "nwisDCImport",
        fluidPage(
                pageWithSidebar(
                        headerPanel("NWIS Datachecks import"),
                        sidebarPanel(
                                fileInput("dataCheckFile",label="Datacheck xlsx file",accept="xlsx"),
                                
                                dateInput(inputId = "begin.date",label="Start date",max=Sys.Date(),value=Sys.Date()-365*10),
                                dateInput(inputId = "end.date",label="End date",max=Sys.Date(),value=Sys.Date()),
                                textInput("DSN",label="Server name (DSN)",value=""),
                                textInput("env.db",label="Environment DB Number",value="01"),
                                textInput("qa.db",label="QA DB Number",value="02"),
                                
                                
                                actionButton(inputId = "dataDownload",label="Get data"),
                                
                                icon = icon("dashboard")
                        ),
                        mainPanel(
                                
                                h2(textOutput("headerReminder")),
                                
                                h3(textOutput("importWarning")),
                                h3(textOutput("samplesRetrieved")),
                                h3(textOutput("resultsRetrieved")),
                                h3(textOutput("sampleModified")),
                                h4(textOutput("recordModified")),
                                h4(textOutput("recordModifiedDate")),
                                h4(textOutput("recordModifiedName")),
                                h3(textOutput("resultModified")),
                                h4(textOutput("resultRecordModified")),
                                h4(textOutput("resultModifiedPARM")),
                                h4(textOutput("resultModifiedDate")),
                                h4(textOutput("resultModifiedName")),
                                verbatimTextOutput("shinyErrors"),
                                verbatimTextOutput("errors")
                        )
                )
        )
)

