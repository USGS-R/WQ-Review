#######################################
###This does the timeseries plotting###
#######################################

output$qwtsPlot <- renderPlot({
        qwtsPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_TS),
                 plotparm = as.character(input$parmSel_TS),
                 highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                 show.smooth = input$fit_timeseries,
                 facet = input$facetSel_TS,
                 show.q = input$showQ,
                 print=FALSE
        ) + theme_bw()  
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwtsPlot_zoom <- renderPlot({
        qwtsPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_TS),
                 plotparm = as.character(input$parmSel_TS),
                 highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                 show.smooth = input$fit_timeseries,
                 facet = input$facetSel_TS,
                 printPlot = FALSE) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
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
        dataSelections$siteSel <- input$siteSel_TS
        dataSelections$parmSel <- input$parmSel_TS
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_TS <- "SAMPLE_START_DT"
yvar_TS <- "RESULT_VA"
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

output$timeseries_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_TS,
                                 yvar=yvar_TS),
                      
                      options=list(scrollX=TRUE)
        )
})


output$timeseries_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                    brush=input$plot_brush,
                                    xvar=xvar_TS,
                                    yvar=yvar_TS),
                      
                      options=list(scrollX=TRUE)
        )
})

output$timeseries_hoverinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
        )
})