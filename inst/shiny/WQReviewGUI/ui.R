
shinyUI(fluidPage(theme="theme.css",title="WQReview 2.1",
                  navbarPage(title = img(src="Logo.png", width="100px",height = "40px"),
                             navbarMenu("Import/save Data",
                                        tabPanel("Import from internal NWIS server",
                                                 source("ui_dataImport.r",local=TRUE)$value
                                        ),
                                        tabPanel("Save/load imported data",
                                                 source("ui_saveLoad.r",local=TRUE)$value
                                        )
                             ),
                             
                             tabPanel("Result-level review",
                                      pageWithSidebar(headerPanel("Result-level review"),
                                                      sidebarPanel(
                                                        dateInput("newThreshold", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                                                        selectInput("siteSel","Station",choices="",multiple=TRUE),
                                                        selectInput("parmSel","Parameter",choices="",multiple=FALSE),
                                                        selectInput("facetSel","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE),
                                                        checkboxInput("labelDQI","Label DQI codes"),
                                                        hr(),
                                                        actionButton("refresh",label="Refresh from excel"),
                                                        DT::dataTableOutput("UNAPtable")
                                                      ),
                                                      mainPanel(
                                                        hr(),
                                                        fluidRow(
                                                          column(4,checkboxInput("fit_timeseries",label="Add LOESS",value=FALSE)),
                                                          column(4,checkboxInput("showQ",label="Display hydrograph",value=FALSE))
                                                        ),
                                                        plotlyOutput("qwtsPlot"),
                                                        
                                                        hr(),
                                                        
                                                        checkboxInput("fit_seasonal",label="Add LOESS",value=FALSE),
                                                        plotlyOutput("qwseasonalPlot"),
                                                        hr(),
                                                        
                                                        fluidRow(
                                                          column(6,
                                                                 fluidRow(
                                                                   column(4,selectInput("parmSel_parmParmX1","X-Parameter",choices="",multiple=TRUE)),
                                                                   column(4,checkboxInput("fit_parmParm1",label="Add linear fit",value=FALSE)),
                                                                   column(4,checkboxGroupInput("axes_parmParm1","Axis options:",choices = c("Log10X","Log10Y")))
                                                                 ),
                                                                 plotlyOutput("qwparmParmPlot1")
                                                          ),
                                                          column(6,
                                                                 
                                                                 
                                                                 fluidRow(
                                                                   column(4,selectInput("parmSel_parmParmX2","X-Parameter",choices="",multiple=TRUE)),
                                                                   column(4,checkboxInput("fit_parmParm2",label="Add linear fit",value=FALSE)),
                                                                   column(4,checkboxGroupInput("axes_parmParm2","Axis options:",choices = c("Log10X","Log10Y")))
                                                                 ),
                                                                 plotlyOutput("qwparmParmPlot2")
                                                          )
                                                        ),
                                                        
                                                        plotOutput("qwboxplot",height="800px")
                                                      )
                                                      
                                      )
                             ),
                             
                             tabPanel("Troubleshoot charge balance",
                                      pageWithSidebar(headerPanel("Sample charge balance"),
                                                      sidebarPanel(
                                                        selectInput("siteSel_cb","Station",choices="",multiple=TRUE),
                                                        selectInput("facetSel_cb","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE),
                                                        hr(),
                                                        actionButton("refresh_cb",label="Refresh from excel"),
                                                        DT::dataTableOutput("badCBTable")
                                                      ),
                                                      mainPanel(
                                                        plotlyOutput("qwcbPlot",height="800px"),
                                                        plotlyOutput("qwscSumPlot",height="600px")
                                                        
                                                      )
                                      )
                             ),
                             
                             tabPanel("Blanks and Rep comparison",
                                      pageWithSidebar(headerPanel("Blanks and Reps"),
                                                      sidebarPanel(
                                                        selectInput("siteSel_br","Station",choices="",multiple=TRUE),
                                                        selectInput("parmSel_br","Parameter",choices="",multiple=FALSE),
                                                        selectInput("facetSel_br","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE)
                                                      ),
                                                      mainPanel(
                                                        plotOutput("qwblankPlot",height="800px"),
                                                        hr(),
                                                        plotOutput("qwrepPlot",height="800px")
                                                      )
                                      )
                             ),
                             
                             tabPanel("Generate batch DQI flip files",
                                      pageWithSidebar(headerPanel("Batch files"),
                                                      sidebarPanel(
                                                        hr("Generates batch files for upload to QWData from the data in the 'Ready for DQI change' tab in the Excel workbook"),
                                                        actionButton("flipDQI",label="Generate batch files")
                                                      ),
                                                      mainPanel()
                                      )
                             ),
                             tabPanel(title = "WQReview 2.1 User Guide | FAQ | Additional Info",
                                      helpText(a('User Guide', href="WQReviewGUI.html",target="_blank")),
                                      helpText(a('FAQ', href="faq.html",target="_blank")),
                                      helpText(a("Additional Info: WQReview on USGS-R GitHub",
                                                 href="https://github.com/USGS-R/WQ-Review",target="_blank")))
                                      
                             

                  )
)
)




