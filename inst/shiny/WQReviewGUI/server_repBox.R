#######################################
###This does the prepBox plotting###
#######################################


output$qwrepBoxPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_repBox) & !is.null(input$parmSel_repBox),
                      "No site or parameter selected"))
        qwrepBoxPlot(reports = reports,
                      new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                      site.selection = as.character(input$siteSel_repBox),
                      plotparm = as.character(input$parmSel_repBox),
                      new.reps = as.POSIXct(input$newReplicates),
                     show.points = input$showpoints_repBox,
                     if(input$recordSelect == "")
                     {
                             highlightrecords = c(reports$sampleFlagTable$RECORD_NO[as.numeric(input$sampleFlagTable_rows_selected)],
                                                  reports$resultFlagTable$RECORD_NO[as.numeric(input$resultFlagTable_rows_selected)])
                     } else{highlightrecords = input$recordSelect} 
        )
})


#output$qwrepBoxPlot_zoom <- renderPlot({
#        qwrepBoxPlot(reports = reports,
#                     new.threshold = Sys.time()-as.POSIXct(input$newThreshold_repBox),
#                     site.selection = as.character(input$siteSel_repBox),
#                     plotparm = as.character(input$parmSel_repBox),
#                     new.reps = as.POSIXct(input$newReplicates)) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
#                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
#})

