############################
###chemFlagTable
############################


###Render the table
output$chemFlagTable <- DT::renderDataTable(
        reports$chemFlagTable,
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)

output$chemFlagTableOut <- downloadHandler(
        filename = function() {"chemFlagTableOut"},
        content = function(file) {
                write.table(reports$chemFlagTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)

############################
###sampleFlagTable
############################


###Render the table
output$pestFlagTable <- DT::renderDataTable(
        reports$pestFlagTable,
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)

output$pestFlagTableOut <- downloadHandler(
        filename = function() {"pestFlagTableOut"},
        content = function(file) {
                write.table(reports$pestFlagTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)

############################
###resultFlagTable
############################

###Render the table

output$resultFlagTable <- DT::renderDataTable(
        reports$resultFlagTable,
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)



output$resultFlagTableOut <- downloadHandler(
        filename = function() {"resultFlagTableOut"},
        content = function(file) {
                write.table(reports$resultFlagTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)


############################
###Wide Data table
############################


###Render the table
try({
        output$wideDataTable <- DT::renderDataTable(
                subset(qw.data$DataTable, SITE_NO %in% as.character(input$siteSel_wideDataTable) &
                               as.Date(SAMPLE_START_DT) >= input$startDate_wideDataTable &
                               as.Date(SAMPLE_START_DT) <= input$endDate_wideDataTable
                ),
                extensions = list(FixedColumns = list(leftColumns = 1)),
                server=TRUE,
                rownames= FALSE,
                options = list(
                        scrollX=TRUE,
                        autoWidth=TRUE)
        )
})

output$wideDataTableOut <- downloadHandler(
        filename = function() {"wideDataTableOut"},
        content = function(file) {
                write.table(qw.data$DataTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)


############################
###Long Data table
############################



###Render the table
output$longDataTable <- DT::renderDataTable(
        subset(qw.data$PlotTable, SITE_NO %in% as.character(input$siteSel_longDataTable) &
                       as.Date(SAMPLE_START_DT) >= input$startDate_longDataTable &
                       as.Date(SAMPLE_START_DT) <= input$endDate_longDataTable
        ),
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)


output$longDataTableOut <- downloadHandler(
        filename = function() {"longDataTableOut"},
        content = function(file) {
                write.table(qw.data$PlotTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)
############################
###CB table
############################


###Render the table
output$balanceTable <- DT::renderDataTable(
        subset(reports$BalanceDataTable, SITE_NO %in% as.character(input$siteSel_balanceTable) &
                       as.Date(SAMPLE_START_DT) >= input$startDate_balanceTable &
                       as.Date(SAMPLE_START_DT) <= input$endDate_balanceTable
        ),
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)

output$BalanceDataTableOut <- downloadHandler(
        filename = function() {"BalanceDataTableOut"},
        content = function(file) {
                write.table(reports$BalanceDataTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)
############################
###Rep table
############################

###Render the table
output$repTable <- DT::renderDataTable(
        subset(reports$repTable, SITE_NO %in% as.character(input$siteSel_repTable) &
                       as.Date(Env_SAMPLE_START_DT) >= input$startDate_repTable &
                       as.Date(Env_SAMPLE_START_DT) <= input$endDate_repTable
        ),
        server=TRUE,
        rownames= FALSE,
        extensions = list(FixedColumns = list(leftColumns = 1)),
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)

output$repTableOut <- downloadHandler(
        filename = function() {"repTableOut"},
        content = function(file) {
                write.table(reports$repTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)

############################
###wholevpart table
############################


###Render the table
output$wholevpartTable <- DT::renderDataTable(
        subset(reports$wholevpartTable, SITE_NO %in% as.character(input$siteSel_wholevpartTable) &
                       as.Date(SAMPLE_START_DT) >= input$startDate_wholevpartTable &
                       as.Date(SAMPLE_START_DT) <= input$endDate_wholevpartTable
        ),
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        rownames= FALSE,
        #container = wholevpartTableContainer,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)

output$wholevpartTableOut <- downloadHandler(
        filename = function() {"wholevpartTableOut"},
        content = function(file) {
                write.table(reports$wholevpartTable,sep="\t", col.names = T, row.names = F,na="", quote = FALSE,
                            file)
        }
)


############################
###Blank table
############################

###Render the table
try({

        output$blankTable <- DT::renderDataTable(

                suppressWarnings(blankSummary(qw.data,
                             STAIDS = as.character(input$siteSel_blankTable),
                             begin.date = input$startDate_blankTable, 
                             end.date = input$endDate_blankTable,
                             multiple = FALSE)),
                server=TRUE,
                rownames= FALSE,
                extensions = list(FixedColumns = list(leftColumns = 1)),
                options = list(
                        scrollX=TRUE,
                        autoWidth=TRUE)
        )
        output$blankTableOut <- downloadHandler(
                filename = function() {"blankTableOut"},
                content = function(file) {
                        write.table(blankSummary(qw.data,
                                                 STAIDS = as.character(input$siteSel_blankTable),
                                                 begin.date = input$startDate_blankTable, 
                                                 end.date = input$endDate_blankTable,
                                                 multiple = FALSE)
                                    ,sep="\t", col.names = F, row.names = F,na="", quote = FALSE,
                                    file)
                }
        )
        
})


