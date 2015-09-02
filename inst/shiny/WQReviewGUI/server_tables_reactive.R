############################
###Blank table
############################

###Render the table
try({
        output$blankTable <- DT::renderDataTable(
                
                blankSummary(qw.data,
                             STAIDS = as.character(input$siteSel_blankTable),
                             begin.date = input$blankStartDate, 
                             end.date = input$blankEndDate,
                             multiple = FALSE),
        server=TRUE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
)
output$blankTableOut <- downloadHandler(
        filename = function() {"blankTableOut"},
        content = function(file) {
                write.table(blankSummary(qw.data,
                                         STAIDS = as.character(input$siteSel_blankTable),
                                         begin.date = input$blankStartDate, 
                                         end.date = input$blankEndDate,
                                         multiple = FALSE)
                            ,sep="\t", col.names = F, row.names = F,na="", quote = FALSE,
                            file)
        }
)

},
silent=TRUE)