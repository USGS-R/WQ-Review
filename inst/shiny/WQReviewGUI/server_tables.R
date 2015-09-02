############################
###Wide Data table
############################

###Make the header and footer


###Render the table
output$wideDataTable <- DT::renderDataTable(
                qw.data$DataTable,
                server=TRUE,
                options = list(
                        scrollX=TRUE,
                        autoWidth=TRUE)
)

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
        qw.data$PlotTable,
        server=TRUE,
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
output$cbTable <- DT::renderDataTable(
        reports$BalanceDataTable,
        server=TRUE,
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
        reports$repTable,
        server=TRUE,
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
        reports$wholevpartTable,
        server=TRUE,
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