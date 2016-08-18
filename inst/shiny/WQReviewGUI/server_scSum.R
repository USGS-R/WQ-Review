###This subsets the qw.data dataframe to selected sites and results

selData_scSum <- reactive({
        if(input$siteSel_scSum == "All")
        {
                melt(plotTable[plotTable$PARM_CD == "00095",
                               c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                     id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff"))
                
        } else {
                melt(plotTable[plotTable$SITE_NO %in% siteSel_scSum & 
                                       plotTable$PARM_CD == "00095",
                               c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                     id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff"))
                }
})


#######################################
###This does the timeseries plotting###
#######################################

output$qwscSumPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_scSum),
                      "No site selected"))
        
        if(input$siteSel_scSum == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_scSum)
        }
        
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = sites,
                 labelDQI = input$labelDQI_scSum,
                 facet = input$facetSel_scSum,
                 highlightrecords = reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                 printPlot=FALSE)
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwscSumPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_scSum$x), "Select area in upper plot to zoom"))
        
        if(input$siteSel_scSum == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_scSum)
        }
        
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = sites,
                 labelDQI = input$labelDQI_scSum,
                 facet = input$facetSel_scSum,
                 highlightrecords = reports$chemFlagTable$RECORD_NO[which(!is.na(reports$sampleFlagTable$BadCB_30.21))],
                 printPlot = FALSE) + 
                ###This resescSum the axes to zoomed area, must specify origin because brushedPoinscSum returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges_scSum$x, ylim = ranges_scSum$y)
})

#########################################
###This does the plotting interactions###
#########################################

##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_scSum <- "RESULT_VA"
yvar_scSum <- "value"
##################################################

###This sets the ranges_scSum variables for brushin
ranges_scSum <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_scSum
        if (!is.null(brush)) {
                ranges_scSum$x <- c(brush$xmin, brush$xmax)
                ranges_scSum$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_scSum$x <- NULL
                ranges_scSum$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points

output$scSum_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=selData_scSum(),
                                 coordinfo = input$plot_click_scSum,
                                 xvar=xvar_scSum,
                                 yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})


output$scSum_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=selData_scSum(),
                                    brush=input$plot_brush_scSum,
                                    xvar=xvar_scSum,
                                    yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$scSum_hoverinfo <- renderPrint({
        
        hoverTable <- nearPoints(df=selData_scSum(),
                                 coordinfo = input$plot_hover,
                                 xvar=xvar_scSum,
                                 yvar=yvar_scSum)
        
        cat("Record #:",unique(hoverTable$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(hoverTable$SITE_NO),
            "\n");
        
        cat("Station:",unique(hoverTable$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(hoverTable$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        

        
        
})

###This creates a new entry in the marked record table
observeEvent(input$scSum_addRecord, {
        try({
                newEntry <- data.frame(RECORD_NO = input$scSum_flaggedRecord,
                                       SITE_NO = unique(plotTable$SITE_NO[which(plotTable$RECORD_NO == 
                                                                                                input$scSum_flaggedRecord)]
                                       ),
                                       STATION_NM = unique(plotTable$STATION_NM[which(plotTable$RECORD_NO == 
                                                                                                      input$scSum_flaggedRecord)]
                                       ),
                                       SAMPLE_START_DT = as.character(unique(plotTable$SAMPLE_START_DT[which(plotTable$RECORD_NO == 
                                                                                                                             input$scSum_flaggedRecord)])
                                       ),
                                       MEDIUM_CD = unique(plotTable$MEDIUM_CD[which(plotTable$RECORD_NO == 
                                                                                                    input$scSum_flaggedRecord)]
                                       ),
                                       DQI_CD = NA,
                                       PARM_CD = NA,
                                       PARM_NM = NA,
                                       Where_Flagged = "SC vs sum ions",
                                       Comment = input$scSum_flaggedComment
                )
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "scSum_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "scSum_flaggedComment",
                                value = " "
                )
                
                
        })
})