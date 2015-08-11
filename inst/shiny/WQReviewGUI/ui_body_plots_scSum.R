###This contains all the ui elements for the chargebalance plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "scSumPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Sum ions vs. conductance"),
                        sidebarPanel(
                                ###Controls items for plot
                                dateInput("newThreshold_scSum", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                                selectInput("siteSel_scSum","Station",choices="",multiple=TRUE),
                                selectInput("facetSel_scSum","Multi-site options",choices=c("Multisite","Facet"),multiple=FALSE)
                                
                        ),
                        mainPanel(
                ###This displays the primary plot interaction output
                box(
                        plotOutput("qwscSumPlot", click="plot_click",brush="plot_brush"),
                        #verbatimTextOutput("brushx"),
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE),
                
                ###This displays the zoomed plot interaction output
                
                box(
                        plotOutput("qwscSumPlot_zoom", click="plot_click"),
                        ###Box options
                        width=12,
                        collapsible=TRUE),
                
                ###This displays the plot interaction output
                
                box(
                        DT::dataTableOutput("scSum_clickinfo"),
                        DT::dataTableOutput("scSum_brushinfo"),
                        ###Box options
                        width=12,
                        collapsible=TRUE)
                        )
        )
)
)