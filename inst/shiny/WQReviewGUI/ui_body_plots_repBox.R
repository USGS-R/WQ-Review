###This contains all the ui elements for the repBox plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "repBoxPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Replicate RPD boxplot"),
                        sidebarPanel(
                                #dateInput("newThreshold_repBox", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                                dateInput("newReplicates", "New replicates threshold",max=Sys.Date(),value=Sys.Date()-30),
                                
                                selectInput("siteSel_repBox","Station",choices="",multiple=TRUE),
                                selectInput("parmSel_repBox","Parameter",choices="",multiple=TRUE),
                                checkboxInput("showpoints_repBox",label="Show sample points",value=FALSE),
                                ###Sidebar options
                                width=3
                                
                        ),
                        mainPanel(
                ###This displays the primary plot interaction output
                box(
                        plotOutput("qwrepBoxPlot"),
                        #verbatimTextOutput("brushx"),
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE)
                
                ###This displays the zoomed plot interaction output
                
                #box(
                #        plotOutput("qwrepBoxPlot_zoom"),
                #        ###Box options
                #        width=12,
                #        collapsible=TRUE)
                
                ###This displays the plot interaction output
                
                #box(
                #       DT::dataTableOutput("repBox_clickinfo"),
                #       DT::dataTableOutput("repBox_brushinfo"),
                #       ###Box options
                #       width=12,
                #       collapsible=TRUE)
                        )
        )
)
)