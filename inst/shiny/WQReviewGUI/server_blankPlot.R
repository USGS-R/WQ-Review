#######################################
###This does the timeseries plotting###
#######################################

output$qwblankPlot <- renderPlot({
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold_blank),
                 site.selection = as.character(input$siteSel_blank),
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                 print=FALSE
        ) + theme_bw()  
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwblankPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges$x), "Select area in upper plot to zoom"))
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold_blank),
                 site.selection = as.character(input$siteSel_blank),
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 
                 highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
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
        dataSelections$siteSel <- input$siteSel_blank
        dataSelections$parmSel <- input$parmSel_blank
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_blank <- "SAMPLE_START_DT"
yvar_blank <- "RESULT_VA"
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

output$blank_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_blank,
                                 yvar=yvar_blank),
                      
                      options=list(scrollX=TRUE)
        )
})


output$blank_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                    brush=input$plot_brush,
                                    xvar=xvar_blank,
                                    yvar=yvar_blank),
                      
                      options=list(scrollX=TRUE)
        )
})

