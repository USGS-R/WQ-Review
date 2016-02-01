###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "siteSelection",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Data download criteria"),
                        sidebarPanel(
                                selectizeInput("STAIDS",label="Site number",choices=NULL,multiple=TRUE,options=list(create=TRUE)),
                                fileInput("siteFile",label="Site number file",accept="csv"),
                                
                                selectizeInput("dl.parms",label="Parameter codes",choices=NULL,multiple=TRUE,options=list(create=TRUE)),
                                selectInput("dl.parms.group","Parameter groups",choices=c("All" = "All",
                                                                                    "physical" = "PHY",
                                                                                    "cations" = "INM",
                                                                                    "anions" = "INN",
                                                                                    "nutrients" = "NUT",
                                                                                    "microbiological" = "MBI",
                                                                                    "biological" = "BIO",
                                                                                    "metals" = "IMM",
                                                                                    "nonmetals" = "IMN",
                                                                                    "pesticides" = "TOX",
                                                                                    "pcbs"="OPE",
                                                                                    "other organics" = "OPC",
                                                                                    "radio chemicals" = "RAD",
                                                                                    "stable isotopes" = "XXX",
                                                                                    "sediment" = "SED",
                                                                                    "population/community" = "POP")
                                            ,selectize=FALSE,multiple=TRUE),
                                fileInput("parmFile",label="pCode file",accept="csv"),
                                
                                dateInput(inputId = "begin.date",label="Start date",max=Sys.Date(),value=Sys.Date()-365*10),
                                dateInput(inputId = "end.date",label="End date",max=Sys.Date(),value=Sys.Date()),
                                selectizeInput("projectCd",label="Project code (optional)",choices=NULL,multiple=TRUE,options=list(create=TRUE)),
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
