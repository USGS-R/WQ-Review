tabItem(tabName = "dataUpload",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Batch file generator"),
                        sidebarPanel(
                                downloadButton('templateExport', 'Download Excel templates'),
                                
                                selectInput("qwsampleType", "Type of samples", 
                                            choices = c("Logged-in"="1",
                                                        "New samples"="2")),
                                conditionalPanel(
                                        condition = "input.qwsampleType == '1'",
                                        textInput("uploadDSN",label="Server name (DSN)",value="NWISCO"),
                                        textInput("uploadenv.db",label="Environment DB Number",value="01"),
                                        textInput("uploadqa.db",label="QA DB Number",value="02"),
                                        
                                        dateInput(inputId = "qwsampleBeginDate",label="QWSample start date",max=Sys.Date(),value=Sys.Date()-365*1),
                                        dateInput(inputId = "qwsampleEndDate",label="QWSample end date",max=Sys.Date(),value=Sys.Date())
                                ),
                                
                                fileInput("labFile",label="Lab file upload",accept="text/csv"),
                                fileInput("pcodeFile",label="Parameter file upload",accept="text/csv"),
                                
                                conditionalPanel(
                                        condition = "input.qwSampleType == '2'",
                                        fileInput("qwsampleFile",label="QWSample file",accept="text/csv")
                                ),
                                checkboxInput(inputId = "censor",label="Censor lab values to lrl",value=FALSE),
                                
                                actionButton(inputId = "formatFiles",label="Generate batch files"),
                                
                                textInput("qwsampleName","QWSample filename",value="qwsample"),
                                textInput("qwresultName","QWResult filename",value="qwresult"),
                                downloadButton('qwsampleExport', 'Download QWSample file'),
                                downloadButton('qwresultExport', 'Download QWResult file')
                        ),
                        mainPanel(
        box(
                DT::dataTableOutput("qwsample"),

                ###Box options
                width=12,
                collapsible=TRUE,
                title="QWSample file"),
        box(

                DT::dataTableOutput("qwresult"),

                ###Box options
                width=12,
                collapsible=TRUE,
                title="QWResult file")
                        )
        )
)
)