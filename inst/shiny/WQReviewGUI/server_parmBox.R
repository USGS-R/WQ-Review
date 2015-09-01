#######################################
###This does the parameter boxPlot plotting###
#######################################


output$qwparmBoxPlot <- renderPlot({
        qwparmBoxPlot(qw.data = qw.data,
                      new.threshold = Sys.time()-as.POSIXct(input$newThreshold_parmBox),
                      site.selection = as.character(input$siteSel_parmBox),
                      plotparm = as.character(input$parmSel_parmBox),
                      log.scale = input$axes_parmBox,
                      facet = input$facetSel_parmBox,
                      show.points = input$showpoints_parmBox,
                      highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                      print=FALSE
        ) + theme_bw()  
})


#output$qwparmBoxPlot_zoom <- renderPlot({
#        qwparmBoxPlot(qw.data = qw.data,
#                      new.threshold = Sys.time()-as.POSIXct(input$newThreshold_parmBox),
#                      site.selection = as.character(input$siteSel_parmBox),
#                      plotparm = as.character(input$parmSel_parmBox),
#                      log.scale = input$axes_parmBox,
#                      printPlot = FALSE) + theme_bw() +  
#                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
#                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
#})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_parmBox
        dataSelections$parmSel <- input$parmSel_parmBox
})
##################################################


output$parmBox_sumStats <- DT::renderDataTable({
        
        DT::datatable(ddply(subset(qw.data$PlotTable,SITE_NO %in% as.character(dataSelections$siteSel) & 
                                           PARM_CD%in% as.character(dataSelections$parmSel) & 
                                           MEDIUM_CD %in%(c("WG ","WS "))),
                            c("PARM_CD","PARM_NM"),summarize,
                            
                            Quant95 = quantile(na.omit(RESULT_VA),0.90),
                            Quant05 = quantile(na.omit(RESULT_VA),0.10),
                            Median = median(na.omit(RESULT_VA)),
                            Mean = mean(na.omit(RESULT_VA)),
                            Min = min(na.omit(RESULT_VA)),
                            Max = max(na.omit(RESULT_VA))
                            ),
                      options=list(scrollX=TRUE)
        )
})




##################################################
###CHANGE these to the respective plot variables
xvar_parmBox <- "PARM_CD"
yvar_parmBox <- "RESULT_VA"
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

output$parmBox_clickinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_parmBox,
                                 yvar=yvar_parmBox)[1:4],
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})


output$parmBox_brushinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                    brush=input$plot_brush,
                                    xvar=xvar_parmBox,
                                    yvar=yvar_parmBox),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})

output$parmBox_hoverinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
        )
        # nearPoints() also works with hover and dblclick events
})


