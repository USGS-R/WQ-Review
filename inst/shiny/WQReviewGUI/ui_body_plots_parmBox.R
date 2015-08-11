###This contains all the ui elements for the parmBox plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "parmBoxPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Parameter boxplot"),
                        sidebarPanel(
                                ###Controls items for plot
                                dateInput("newThreshold_parmBox", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                                selectInput("siteSel_parmBox","Station",choices="",multiple=TRUE),
                                selectInput("parmSel_parmBox","Parameter",choices="",multiple=TRUE),
                                selectInput("facetSel_parmBox","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE),
                                
                                checkboxInput("axes_parmBox",label="Log10Y",value=FALSE),
                                checkboxInput("showpoints_parmBox",label="Show sample points",value=FALSE)
                        ),
                        mainPanel(
                
                ###This displays the primary plot interaction output
                box(
                        plotOutput("qwparmBoxPlot"),
                        #verbatimTextOutput("brushx"),
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE),
                
                ###This displays the zoomed plot interaction output
                
                #box(
                #        plotOutput("qwparmBoxPlot_zoom"),
                #        ###Box options
                #        width=12,
                #        collapsible=TRUE)
                
                ###This displays the plot interaction output
                
                box(
                        DT::dataTableOutput("parmBox_sumStats"),
                        ###Box options
                        width=12,
                        collapsible=TRUE)
                        )
                
        )
)
)