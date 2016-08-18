tabItem(tabName = "dqiTable",
        fluidPage(
                # pageWithSidebar(
                #         headerPanel("Unapproved data table"),
                #         sidebarPanel(
                #                 selectInput("siteSel_dqiTable","Station",choices="",multiple=TRUE),
                #                 dateInput("startDate_dqiTable", "Start date", 
                #                           value=Sys.Date() - 365*3),
                #                 dateInput("endDate_dqiTable", "End date", 
                #                           value=Sys.Date()),
                #                 ###sidebar options
                #                 width=3),
                #         mainPanel(
                                #actionButton(inputId = "dqiTable_popNotes",label="Add to notes and remove"),
                                DT::dataTableOutput("dqiTable")
                        #)
                #)
        )
)