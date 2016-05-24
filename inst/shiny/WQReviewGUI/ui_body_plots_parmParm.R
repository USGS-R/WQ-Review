###This contains all the ui elements for the parmParm plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "parmParmPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Parameter vs. Paramater Plot"),
                        sidebarPanel(
                                dateInput("newThreshold_parmParm", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                                selectInput("siteSel_parmParm","Station",choices="",multiple=TRUE),
                                selectInput("parmSel_parmParmX","X-Parameter",choices="",multiple=TRUE),
                                selectInput("parmSel_parmParmY","Y-Parameter",choices="",multiple=TRUE),
                                selectInput("facetSel_parmParm","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE),
                                checkboxInput("labelDQI_parmParm",label="Label DQI codes",value=FALSE),
                                
                                checkboxInput("fit_parmParm",label="Add linear fit",value=FALSE),
                                checkboxGroupInput("axes_parmParm","Axis options:",
                                                   choices = c("Log10X","Log10Y")),
                                verbatimTextOutput("parmParm_hoverinfo"),
                                
                                h3("Review comments"),
                                textInput("parmParm_flaggedRecord",label="Record #"),
                                textInput("parmParm_flaggedComment",label = "Comment"),
                                actionButton(inputId = "parmParm_addRecord",label="Add record"),
                                
                                #verbatimTextOutput("parmParm_hoverinfo"),
                                ###Sidebar options
                                width=3
                        ),
                        mainPanel(
                ###This displays the primary plot interaction output
                box(
                        plotOutput("qwparmParmPlot", click="plot_click_parmParm",brush="plot_brush_parmParm",hover="plot_hover"),
                        #verbatimTextOutput("brushx"),
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE),
                
                ###This displays the zoomed plot interaction output
                
                box(
                        plotOutput("qwparmParmPlot_zoom", click="plot_click_parmParm",hover="plot_hover"),
                        ###Box options
                        width=12,
                        collapsible=TRUE),
                
                ###This displays the plot interaction output
                
                box(
                        DT::dataTableOutput("parmParm_clickinfo"),
                        DT::dataTableOutput("parmParm_brushinfo"),
                        
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE)
                        )
        )
)
)