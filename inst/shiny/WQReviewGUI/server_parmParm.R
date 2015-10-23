#######################################
###This does the parameter-parameter plotting###
#######################################


output$qwparmParmPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_parmParm) & !is.null(input$parmSel_parmParmX) & !is.null(input$parmSel_parmParmY),
                      "No site or parameter selected"))
        qwparmParmPlot(qw.data = qw.data,
                       site.selection = as.character(input$siteSel_parmParm),
                       xparm = as.character(input$parmSel_parmParmX),
                       yparm = as.character(input$parmSel_parmParmY),
                       facet = input$facetSel_parmParm,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       show.lm = input$fit_parmParm,
                       if("Log10X" %in% input$axes_parmParm)
                       {
                               log.scaleX = TRUE
                       } else(log.scaleX = FALSE),
                       if("Log10Y" %in% input$axes_parmParm)
                       {
                              log.scaleY = TRUE 
                       } else(log.scaleY = FALSE),
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_parmParmX) |
                                                                                            reports$resultFlagTable$PARM_CD == as.character(input$parmSel_parmParmY))]),
                       printPlot = FALSE
                       
                       
                       
        ) 
})


output$qwparmParmPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_parmParm$x), "Select area in upper plot to zoom"))
        
        qwparmParmPlot(qw.data = qw.data,
                       site.selection = as.character(input$siteSel_parmParm),
                       xparm = as.character(input$parmSel_parmParmX),
                       yparm = as.character(input$parmSel_parmParmY),
                       facet = input$facetSel_parmParm,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       show.lm = input$fit_parmParm,
                       if("Log10X" %in% input$axes_parmParm)
                       {
                               log.scaleX = TRUE
                       } else(log.scaleX = FALSE),
                       if("Log10Y" %in% input$axes_parmParm)
                       {
                               log.scaleY = TRUE 
                       } else(log.scaleY = FALSE),
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_parmParmX) |
                                                                                            reports$resultFlagTable$PARM_CD == as.character(input$parmSel_parmParmY))]),
                       printPlot = FALSE
                       
                       
        ) +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges_parmParm$x, ylim = ranges_parmParm$y)
})




#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections_parmParm <- reactiveValues(siteSel = NULL, parmSelX = NULL,parmSelY=NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections_parmParm$siteSel <- input$siteSel_parmParm
        dataSelections_parmParm$parmSelX <- input$parmSel_parmParmX
        dataSelections_parmParm$parmSelY <- input$parmSel_parmParmY
})

observe({
        if(!is.null(qw.data))
        {
        ##Have to get different data table styructure that is the same as used in the plot function
        xpp.plot.data <- 
                subset(qw.data$PlotTable,SITE_NO %in% dataSelections_parmParm$siteSel & PARM_CD == dataSelections_parmParm$parmSelX)
        
        ypp.plot.data <- 
                subset(qw.data$PlotTable,SITE_NO %in% dataSelections_parmParm$siteSel & PARM_CD == dataSelections_parmParm$parmSelY)
        #Join x and y data
        pp.plot.data <<- join(xpp.plot.data[,c("RECORD_NO","SITE_NO","STATION_NM","MEDIUM_CD","SAMPLE_START_DT","RESULT_VA","RESULT_MD")], 
                              ypp.plot.data[,c("RECORD_NO","RESULT_VA","RESULT_MD")],by="RECORD_NO")
        names(pp.plot.data) <<- c("RECORD_NO","SITE_NO","STATION_NM","MEDIUM_CD","SAMPLE_START_DT","RESULT_VA_X","RESULT_MD_X","RESULT_VA_Y","RESULT_MD_Y")
        }
})


##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_parmParm <- "RESULT_VA_X"
yvar_parmParm <- "RESULT_VA_Y"
##################################################


###This sets the ranges_parmParm variables for brushin
ranges_parmParm <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_parmParm
        if (!is.null(brush)) {
                ranges_parmParm$x <- c(brush$xmin, brush$xmax)
                ranges_parmParm$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_parmParm$x <- NULL
                ranges_parmParm$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points


output$parmParm_clickinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(df=na.omit(pp.plot.data),
                                 coordinfo = input$plot_click_parmParm,
                                 xvar=xvar_parmParm,
                                 yvar=yvar_parmParm),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})


output$parmParm_brushinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=na.omit(pp.plot.data),
                                    brush=input$plot_brush_parmParm,
                                    xvar=xvar_parmParm,
                                    yvar=yvar_parmParm),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$parmParm_hoverinfo <- renderPrint({
        
        cat("Record #:",unique(nearPoints(df=na.omit(pp.plot.data),
                                                     coordinfo = input$plot_hover,
                                                     xvar=xvar_parmParm,
                                                     yvar=yvar_parmParm)$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(nearPoints(df=na.omit(pp.plot.data),
                                                   coordinfo = input$plot_hover,
                                                   xvar=xvar_parmParm,
                                                   yvar=yvar_parmParm)$SITE_NO),
            "\n");
        
        cat("Station:",unique(nearPoints(df=na.omit(pp.plot.data),
                                                    coordinfo = input$plot_hover,
                                                    xvar=xvar_parmParm,
                                                    yvar=yvar_parmParm)$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(nearPoints(df=na.omit(pp.plot.data),
                                                  coordinfo = input$plot_hover,
                                                  xvar=xvar_parmParm,
                                                  yvar=yvar_parmParm)$SAMPLE_START_DT),"%Y-%m-%d %H:%M"),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_parmParm,
                                                                              yvar=yvar_parmParm)$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                                                                                                                                              coordinfo = input$plot_hover,
                                                                                                                                                                                              xvar=xvar_parmParm,
                                                                                                                                                                                              yvar=yvar_parmParm)$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_parmParm,
                                                                              yvar=yvar_parmParm)$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                                                                                                                                               coordinfo = input$plot_hover,
                                                                                                                                                                                               xvar=xvar_parmParm,
                                                                                                                                                                                               yvar=yvar_parmParm)$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Result flags:",
            names(subset(reports$resultFlagTable, RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                                  coordinfo = input$plot_hover,
                                                                                  xvar=xvar_parmParm,
                                                                                  yvar=yvar_parmParm)$RECORD_NO) & (PARM_CD == dataSelections_parmParm$parmSelX | PARM_CD == dataSelections_parmParm$parmSelY))[14:17])[which(sapply(subset(reports$resultFlagTable, RECORD_NO == unique(nearPoints(df=na.omit(pp.plot.data),
                                                                                                                                                                                                                                                                                                  coordinfo = input$plot_hover,
                                                                                                                                                                                                                                                                                                  xvar=xvar_parmParm,
                                                                                                                                                                                                                                                                                                  yvar=yvar_parmParm)$RECORD_NO) & (PARM_CD == dataSelections_parmParm$parmSelX | PARM_CD == dataSelections_parmParm$parmSelY))[14:17], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        
        
})