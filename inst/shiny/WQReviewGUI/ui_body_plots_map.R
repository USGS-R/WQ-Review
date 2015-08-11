###This contains all the ui elements for the map plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "mapPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Spatial plotting"),
                        sidebarPanel(
                                ###Controls items for plot
                                actionButton("getMap","Download map"),
                                selectInput("siteSel_map","Station",choices="",multiple=TRUE),
                                selectInput("parmSel_map","Parameter",choices="",multiple=TRUE)
                        ),
                        mainPanel(
                                ###This displays the primary plot interaction output
                                box(
                                        plotOutput("qwmapPlot", click="plot_click",brush="plot_brush"),
                                        #verbatimTextOutput("brushx"),
                                        
                                        ###Box options
                                        width=12,
                                        collapsible=TRUE),
                                
                                ###This displays the zoomed plot interaction output
                                
                                box(
                                        plotOutput("qwmapPlot_zoom", click="plot_click"),
                                        ###Box options
                                        width=12,
                                        collapsible=TRUE),
                                
                                ###This displays the plot interaction output
                                
                                box(
                                        DT::dataTableOutput("map_clickinfo"),
                                        DT::dataTableOutput("map_brushinfo"),
                                        ###Box options
                                        width=12,
                                        collapsible=TRUE)
                        )
                )
        )
)