#######################################
###This does the prepBox plotting###
#######################################


output$qwrepBoxPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_repBox) & !is.null(input$parmSel_repBox),
                      "No site or parameter selected"))
        qwrepBoxPlot(qw.data = qw.data,
                      new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                      site.selection = as.character(input$siteSel_repBox),
                      plotparm = as.character(input$parmSel_repBox),
                      new.reps = as.POSIXct(input$newReplicates),
                     show.points = input$showpoints_repBox,
                     highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                          reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD %in% as.character(input$parmSel_repBox))]),
                     printPlot=FALSE
        )
})


