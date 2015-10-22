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
                     highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                          reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD %in% as.character(input$parmSel_repBox))]) 
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

