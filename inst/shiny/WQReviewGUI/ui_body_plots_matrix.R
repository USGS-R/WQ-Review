###This contains all the ui elements for the matrix plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


tabItem(tabName = "matrixPlot",
        fluidPage(
                pageWithSidebar(
                        headerPanel("Matrix Plot"),
                        sidebarPanel(         ###Controls items for plot
                                selectInput("siteSel_matrix","Station",choices="",multiple=TRUE),
                                selectInput("parmSel_matrix","Parameter",choices="",multiple=TRUE),
                                ###Sidebar options
                                width=3
                        ),
                        mainPanel(
                ###This displays the primary plot interaction output
                box(
                        plotOutput("qwmatrixPlot"),
                        #verbatimTextOutput("brushx"),
                        
                        ###Box options
                        width=12,
                        collapsible=TRUE)
                        )
                
               
        )
)
)