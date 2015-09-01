#######################################
###This does the seasonal plotting###
#######################################


output$qwseasonalPlot <- renderPlot({
        qwseasonalPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold_seasonal),
                 site.selection = as.character(input$siteSel_seasonal),
                 plotparm = as.character(input$parmSel_seasonal),
                 facet = input$facetSel_seasonal,
                 highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                 show.smooth = input$fit_seasonal,
                 print=FALSE
                ) + theme_bw()  
})


output$qwseasonalPlot_zoom <- renderPlot({
        qwseasonalPlot(qw.data = qw.data,
                  new.threshold = Sys.time()-as.POSIXct(input$newThreshold_seasonal),
                 site.selection = as.character(input$siteSel_seasonal),
                plotparm = as.character(input$parmSel_seasonal),
                facet = input$facetSel_seasonal,
                highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                show.smooth = input$fit_seasonal,
                 print = FALSE) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
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
        dataSelections$siteSel <- input$siteSel_seasonal
        dataSelections$parmSel <- input$parmSel_seasonal
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_seasonal <- "DOY"
yvar_seasonal <- "RESULT_VA"
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

output$seasonal_clickinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_seasonal,
                                 yvar=yvar_seasonal)[1:4],
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})


output$seasonal_brushinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                    brush=input$plot_brush,
                                    xvar=xvar_seasonal,
                                    yvar=yvar_seasonal),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})

output$seasonal_hoverinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
        )
        # nearPoints() also works with hover and dblclick events
})