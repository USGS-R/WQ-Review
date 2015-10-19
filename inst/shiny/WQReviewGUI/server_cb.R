#######################################
###This does the timeseries plotting###
#######################################

output$qwcbPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_cb),
                      "No site selected"))
        
        qwcbPlot(qw.data = qw.data,
                 site.selection = as.character(input$siteSel_cb),
                 facet = input$facetSel_cb,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 show.smooth = FALSE,
                 if(input$recordSelect == "")
                 {
                         highlightrecords = c(reports$sampleFlagTable$RECORD_NO[as.numeric(input$sampleFlagTable_rows_selected)],
                                              reports$resultFlagTable$RECORD_NO[as.numeric(input$resultFlagTable_rows_selected)])
                 } else{highlightrecords = input$recordSelect},
                 printPlot = FALSE)
        
 
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwcbPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges$x), "Select area in upper plot to zoom"))
        qwcbPlot(qw.data = qw.data,
                 site.selection = as.character(input$siteSel_cb),
                 facet = input$facetSel_cb,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 show.smooth = FALSE,
                 if(input$recordSelect == "")
                 {
                         highlightrecords = c(reports$sampleFlagTable$RECORD_NO[as.numeric(input$sampleFlagTable_rows_selected)],
                                              reports$resultFlagTable$RECORD_NO[as.numeric(input$resultFlagTable_rows_selected)])
                 } else{highlightrecords = input$recordSelect},
                 printPlot = FALSE) +  
                ###This resecb the axes to zoomed area, must specify origin because brushedPoincb returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = as.POSIXct(ranges$x,origin="1970-01-01 00:00.00 UTC"), ylim = ranges$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_cb
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_cb <- "SAMPLE_START_DT"
yvar_cb <- "perc.diff"
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

output$cb_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=unique(subset(qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","perc.diff","sum_an","sum_cat")]
                                               ,SITE_NO %in% dataSelections$siteSel)),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_cb,
                                 yvar=yvar_cb),
                      
                      options=list(scrollX=TRUE)
        )
})


output$cb_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=unique(subset(qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","perc.diff","sum_an","sum_cat")]
                                                     ,SITE_NO %in% dataSelections$siteSel)),
                                    brush=input$plot_brush,
                                    xvar=xvar_cb,
                                    yvar=yvar_cb),
                      
                      options=list(scrollX=TRUE)
        )
})

#output$timeseries_hoverinfo <- DT::renderDataTable({
#        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
#        )
#})