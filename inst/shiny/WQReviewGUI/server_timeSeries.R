#######################################
###This does the timeseries plotting###
#######################################

output$qwtsPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_TS) & !is.null(input$parmSel_TS),
                      "No site or parameter selected"))
        qwtsPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_TS),
                 plotparm = as.character(input$parmSel_TS),
                 highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                      reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_TS))]),
                 show.smooth = input$fit_timeseries,
                 labelDQI = input$labelDQI_timeseries,
                 facet = input$facetSel_TS,
                 show.q = input$showQ,
                 printPlot=FALSE
        ) 
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwtsPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_timeseries$x), "Select area in upper plot to zoom"))
        qwtsPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_TS),
                 plotparm = as.character(input$parmSel_TS),
                 highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                      reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_TS))]),
                 show.smooth = input$fit_timeseries,
                 labelDQI = input$labelDQI_timeseries,
                 facet = input$facetSel_TS,
                 show.q = input$showQ,
                 printPlot=FALSE
        ) + 
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = as.POSIXct(ranges_timeseries$x,origin="1970-01-01 00:00.00 UTC"), ylim = ranges_timeseries$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections_timeseries <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections_timeseries$siteSel <- input$siteSel_TS
        dataSelections_timeseries$parmSel <- input$parmSel_TS
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_timeseries <- "SAMPLE_START_DT"
yvar_timeseries <- "RESULT_VA"
##################################################

###This sets the ranges_timeseries variables for brushin
ranges_timeseries <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_timeseries
        if (!is.null(brush)) {
                ranges_timeseries$x <- c(brush$xmin, brush$xmax)
                ranges_timeseries$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_timeseries$x <- NULL
                ranges_timeseries$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points
#Make this table reactive so that the values can be used to populate the review notes on click
timeseries_clickReactive <<- reactive({
        nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                 coordinfo = input$plot_click_timeseries,
                                 xvar=xvar_timeseries,
                                 yvar=yvar_timeseries)
})

output$timeseries_clickinfo <- DT::renderDataTable(
        # With base graphics, need to tell it what the x and y variables are.
        timeseries_clickReactive(),
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        #rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
        # nearPoints() also works with hover and dblclick events
)

observeEvent(input$timeseries_popNotes, {
        updateTextInput(session, 
                        "sidebar_flaggedRecord",
                        value = timeseries_clickReactive()$RECORD_NO[as.numeric(input$timeseries_clickinfo_rows_selected)]
        )
        updateTextInput(session, 
                        "parmSel_sidebar",
                        value = timeseries_clickReactive()$PARM_CD[as.numeric(input$timeseries_clickinfo_rows_selected)]
        )
        updateSelectInput(session,
                          "sidebar_dqiCode",
                          selected=NA)
        updateRadioButtons(session,
                           "sidebar_flaggedStatus",
                           selected="No selection")
        updateTextInput(session, 
                        "sidebar_flaggedComment",
                        value = " "
        )
})


##Brush info does not need ot be reactive because don't use values for anything else

output$timeseries_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                    brush=input$plot_brush_timeseries,
                                    xvar=xvar_timeseries,
                                    yvar=yvar_timeseries),
                      
                      options=list(scrollX=TRUE)
        )
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$timeseries_hoverinfo <- renderPrint({
        
        cat("Record #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                          coordinfo = input$plot_hover,
                                          xvar=xvar_timeseries,
                                          yvar=yvar_timeseries)$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                        coordinfo = input$plot_hover,
                                        xvar=xvar_timeseries,
                                        yvar=yvar_timeseries)$SITE_NO),
            "\n");
        
        cat("Station:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                         coordinfo = input$plot_hover,
                                         xvar=xvar_timeseries,
                                         yvar=yvar_timeseries)$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                  coordinfo = input$plot_hover,
                                                  xvar=xvar_timeseries,
                                                  yvar=yvar_timeseries)$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_timeseries,
                                                                              yvar=yvar_timeseries)$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                                                                                                                                                coordinfo = input$plot_hover,
                                                                                                                                                                                                xvar=xvar_timeseries,
                                                                                                                                                                                                yvar=yvar_timeseries)$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_timeseries,
                                                                              yvar=yvar_timeseries)$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                                                                                                                                                 coordinfo = input$plot_hover,
                                                                                                                                                                                                 xvar=xvar_timeseries,
                                                                                                                                                                                                 yvar=yvar_timeseries)$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Result flags:",
            names(subset(reports$resultFlagTable,PARM_CD == dataSelections_timeseries$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                                                                               coordinfo = input$plot_hover,
                                                                                                                               xvar=xvar_timeseries,
                                                                                                                               yvar=yvar_timeseries)$RECORD_NO))[14:17])[which(sapply(subset(reports$resultFlagTable,PARM_CD == dataSelections_timeseries$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections_timeseries$siteSel & PARM_CD %in% dataSelections_timeseries$parmSel),
                                                                                                                                                                                                                                                                                                   coordinfo = input$plot_hover,
                                                                                                                                                                                                                                                                                                   xvar=xvar_timeseries,
                                                                                                                                                                                                                                                                                                   yvar=yvar_timeseries)$RECORD_NO))[14:17], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        
        
})

###This creates a new entry in the marked record table
observeEvent(input$timeseries_addRecord, {
        try({
                newEntry <- data.frame(RECORD_NO = input$timeseries_flaggedRecord,
                                       SITE_NO = unique(qw.data$PlotTable$SITE_NO[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                input$timeseries_flaggedRecord)]
                                       ),
                                       STATION_NM = unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                      input$timeseries_flaggedRecord)]
                                       ),
                                       SAMPLE_START_DT = as.character(unique(qw.data$PlotTable$SAMPLE_START_DT[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                                             input$timeseries_flaggedRecord)])
                                       ),
                                       MEDIUM_CD = unique(qw.data$PlotTable$MEDIUM_CD[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                    input$timeseries_flaggedRecord)]
                                       ),
                                       PARM_CD = as.character(input$parmSel_TS),
                                       DQI_CD_Current = unique(qw.data$PlotTable$DQI_CD[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                      input$timeseries_flaggedRecord &
                                                                                                      qw.data$PlotTable$PARM_CD == 
                                                                                                      as.character(input$parmSel_TS))]
                                       ),
                                       DQI_CD_New = input$timeseries_dqiCode,
                                       PARM_NM = unique(qw.data$PlotTable$PARM_NM[which(qw.data$PlotTable$PARM_CD == 
                                                                                                as.character(input$parmSel_TS))]
                                       ),
                                       Where_Flagged = "timeseries",
                                       Status = input$timeseries_flaggedStatus,
                                       Comment = input$timeseries_flaggedComment
                )
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "timeseries_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "timeseries_flaggedComment",
                                value = " "
                )
                
        })
})


