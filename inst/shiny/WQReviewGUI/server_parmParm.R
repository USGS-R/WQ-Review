#######################################
###This does the parameter-parameter plotting###
#######################################


output$qwparmParmPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_parmParm) & !is.null(input$parmSel_parmParmX) & !is.null(input$parmSel_parmParmY),
                      "No site or parameter selected"))
        qwparmParmPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = as.character(input$siteSel_parmParm),
                       show.lm = input$fit_parmParm,
                       highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                       facet = input$facetSel_parmParm,
                       xparm = as.character(input$parmSel_parmParmX),
                       yparm = as.character(input$parmSel_parmParmY),
                       if("Log10X" %in% input$axes_parmParm)
                       {
                               log.scaleX = TRUE
                       } else(log.scaleX = FALSE),
                       if("Log10Y" %in% input$axes_parmParm)
                       {
                              log.scaleY = TRUE 
                       } else(log.scaleY = FALSE)
        ) + theme_bw()  
})


output$qwparmParmPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges$x), "Select area in upper plot to zoom"))
        
        qwparmParmPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold_parmParm),
                       site.selection = as.character(input$siteSel_parmParm),
                       show.lm = input$fit_parmParm,
                       highlightrecords = qw.data$DataTable$RECORD_NO[as.numeric(input$wideDataTable_rows_selected)],
                       facet = input$facetSel_parmParm,
                       xparm = as.character(input$parmSel_parmParmX),
                       yparm = as.character(input$parmSel_parmParmY),
                       if("Log10X" %in% input$axes_parmParm)
                       {
                               log.scaleX = TRUE
                       } else(log.scaleX = FALSE),
                       if("Log10Y" %in% input$axes_parmParm)
                       {
                               log.scaleY = TRUE 
                       } else(log.scaleY = FALSE)
        ) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
})




#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSelX = NULL,parmSelY=NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_parmParm
        dataSelections$parmSelX <- input$parmSel_parmParmX
        dataSelections$parmSelY <- input$parmSel_parmParmY
})

observe({
        if(!is.null(qw.data))
        {
        ##Have to get different data table styructure that is the same as used in the plot function
        xpp.plot.data <- 
                subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD == dataSelections$parmSelX)
        
        ypp.plot.data <- 
                subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD == dataSelections$parmSelY)
        #Join x and y data
        pp.plot.data <<- join(xpp.plot.data[,c("RECORD_NO","MEDIUM_CD","RESULT_VA","RESULT_MD","STATION_NM")], ypp.plot.data[,c("RECORD_NO","MEDIUM_CD","RESULT_VA","RESULT_MD","STATION_NM")],by="RECORD_NO")
        names(pp.plot.data) <<- c("RECORD_NO","MEDIUM_CD","RESULT_VA_X","RESULT_MD_X","STATION_NM","MEDIUM_CD","RESULT_VA_Y","RESULT_MD_Y","STATION_NM")
        }
})


##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_parmParm <- "RESULT_VA_X"
yvar_parmParm <- "RESULT_VA_Y"
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


output$parmParm_clickinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(df=na.omit(pp.plot.data),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_parmParm,
                                 yvar=yvar_parmParm),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})


output$parmParm_brushinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=na.omit(pp.plot.data),
                                    brush=input$plot_brush,
                                    xvar=xvar_parmParm,
                                    yvar=yvar_parmParm),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})

#output$parmParm_hoverinfo <- DT::renderDataTable({
#        # With base graphics, need to tell it what the x and y variables are.
#        DT::datatable(nearPoints(qw.data$PlotTable, input$plot_hover)
#        )
#        # nearPoints() also works with hover and dblclick events
#})