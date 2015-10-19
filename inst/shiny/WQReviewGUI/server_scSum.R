#######################################
###This does the timeseries plotting###
#######################################

output$qwscSumPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_scSum),
                      "No site selected"))
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_scSum),
                 facet = input$facetSel_scSum,
                 if(input$recordSelect == "")
                 {
                         highlightrecords = c(reports$sampleFlagTable$RECORD_NO[as.numeric(input$sampleFlagTable_rows_selected)],
                                              reports$resultFlagTable$RECORD_NO[as.numeric(input$resultFlagTable_rows_selected)])
                 } else{highlightrecords = input$recordSelect},
                 print=FALSE)
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwscSumPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges$x), "Select area in upper plot to zoom"))
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_scSum),
                 facet = input$facetSel_scSum,
                 if(input$recordSelect == "")
                 {
                         highlightrecords = c(reports$sampleFlagTable$RECORD_NO[as.numeric(input$sampleFlagTable_rows_selected)],
                                              reports$resultFlagTable$RECORD_NO[as.numeric(input$resultFlagTable_rows_selected)])
                 } else{highlightrecords = input$recordSelect},
                 printPlot = FALSE) + 
                ###This resescSum the axes to zoomed area, must specify origin because brushedPoinscSum returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_scSum
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_scSum <- "RESULT_VA"
yvar_scSum <- "value"
##################################################

###This sets the ranges variables for brushin
ranges <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
                ranges$x <- c(brush$xmin, brush$xmax)
                ranges$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges$x <- NULL
                ranges$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points

output$scSum_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                         id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_scSum,
                                 yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})


output$scSum_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                            id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                    brush=input$plot_brush,
                                    xvar=xvar_scSum,
                                    yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})

#output$timeseries_hoverinfo <- DT::renderDataTable({
#        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
#        )
#})