#######################################
###This does the seasonal plotting###
#######################################


output$qwseasonalPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_seasonal) & !is.null(input$parmSel_seasonal),
                      "No site or parameter selected"))
        
        
        qwseasonalPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = as.character(input$siteSel_seasonal),
                       plotparm = as.character(input$parmSel_seasonal),
                       facet = input$facetSel_seasonal,
                       labelDQI = input$labelDQI_seasonal,
                       show.smooth = input$fit_seasonal,
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_seasonal))]),
                       print = FALSE)
 
})


output$qwseasonalPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_seasonal$x), "Select area in upper plot to zoom"))
        qwseasonalPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = as.character(input$siteSel_seasonal),
                       plotparm = as.character(input$parmSel_seasonal),
                       facet = input$facetSel_seasonal,
                       labelDQI = input$labelDQI_seasonal,
                       show.smooth = input$fit_seasonal,
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_seasonal))]),
                       print = FALSE) + 
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
           coord_cartesian(xlim = ranges_seasonal$x, ylim = ranges_seasonal$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections_seasonal <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections_seasonal$siteSel <- input$siteSel_seasonal
        dataSelections_seasonal$parmSel <- input$parmSel_seasonal
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_seasonal <- "DOY"
yvar_seasonal <- "RESULT_VA"
##################################################


###This sets the ranges_seasonal variables for brushin
ranges_seasonal <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_seasonal
        if (!is.null(brush)) {
                ranges_seasonal$x <- c(brush$xmin, brush$xmax)
                ranges_seasonal$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_seasonal$x <- NULL
                ranges_seasonal$y <- NULL
        }
})



###This outputs the data tables for clicked and brushed points

output$seasonal_clickinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                 coordinfo = input$plot_click_seasonal,
                                 xvar=xvar_seasonal,
                                 yvar=yvar_seasonal),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})


output$seasonal_brushinfo <- DT::renderDataTable({
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                    brush=input$plot_brush_seasonal,
                                    xvar=xvar_seasonal,
                                    yvar=yvar_seasonal),
                      
                      options=list(scrollX=TRUE)
        )
        # nearPoints() also works with hover and dblclick events
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$seasonal_hoverinfo <- renderPrint({
        
        cat("Record #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                              coordinfo = input$plot_hover,
                                              xvar=xvar_seasonal,
                                              yvar=yvar_seasonal)$RECORD_NO),
            "\n"
            );
        
        cat("Site #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                       coordinfo = input$plot_hover,
                                       xvar=xvar_seasonal,
                                       yvar=yvar_seasonal)$SITE_NO),
            "\n");
        
        cat("Station:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                       coordinfo = input$plot_hover,
                                       xvar=xvar_seasonal,
                                       yvar=yvar_seasonal)$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                         coordinfo = input$plot_hover,
                                         xvar=xvar_seasonal,
                                         yvar=yvar_seasonal)$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                      coordinfo = input$plot_hover,
                                                                      xvar=xvar_seasonal,
                                                                      yvar=yvar_seasonal)$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                                                                                                                                            coordinfo = input$plot_hover,
                                                                                                                                                                                            xvar=xvar_seasonal,
                                                                                                                                                                                            yvar=yvar_seasonal)$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                      coordinfo = input$plot_hover,
                                                                      xvar=xvar_seasonal,
                                                                      yvar=yvar_seasonal)$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                                                                                                                                            coordinfo = input$plot_hover,
                                                                                                                                                                                            xvar=xvar_seasonal,
                                                                                                                                                                                            yvar=yvar_seasonal)$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Result flags:",
            names(subset(reports$resultFlagTable,PARM_CD == dataSelections_seasonal$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                      coordinfo = input$plot_hover,
                                                                      xvar=xvar_seasonal,
                                                                      yvar=yvar_seasonal)$RECORD_NO))[14:17])[which(sapply(subset(reports$resultFlagTable,PARM_CD == dataSelections_seasonal$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_seasonal$siteSel & PARM_CD %in% dataSelections_seasonal$parmSel),
                                                                                                                                                                                            coordinfo = input$plot_hover,
                                                                                                                                                                                            xvar=xvar_seasonal,
                                                                                                                                                                                            yvar=yvar_seasonal)$RECORD_NO))[14:17], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        
            
})

###This creates a new entry in the marked record table
observeEvent(input$seasonal_addRecord, {
        try({
                newEntry <- data.frame(RECORD_NO = input$seasonal_flaggedRecord,
                                       SITE_NO = unique(qw.data$PlotTable$SITE_NO[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                input$seasonal_flaggedRecord)]
                                       ),
                                       STATION_NM = unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                      input$seasonal_flaggedRecord)]
                                       ),
                                       SAMPLE_START_DT = as.character(unique(qw.data$PlotTable$SAMPLE_START_DT[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                                             input$seasonal_flaggedRecord)])
                                       ),
                                       MEDIUM_CD = unique(qw.data$PlotTable$MEDIUM_CD[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                    input$seasonal_flaggedRecord)]
                                       ),
                                       PARM_CD = as.character(input$parmSel_seasonal),
                                       DQI_CD_Current = unique(qw.data$PlotTable$DQI_CD[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                      input$seasonal_flaggedRecord &
                                                                                                      qw.data$PlotTable$PARM_CD == 
                                                                                                      as.character(input$parmSel_seasonal))]
                                       ),
                                       DQI_CD_New = input$seasonal_dqiCode,
                                       PARM_NM = unique(qw.data$PlotTable$PARM_NM[which(qw.data$PlotTable$PARM_CD == 
                                                                                                as.character(input$parmSel_seasonal))]
                                       ),
                                       Where_Flagged = "seasonal",
                                       Status = input$seasonal_flaggedStatus,
                                       Comment = input$seasonal_flaggedComment
                )
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "seasonal_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "seasonal_flaggedComment",
                                value = " "
                )
                
                
        })
})
