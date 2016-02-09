###This creates a new entry in the marked record table
observeEvent(input$sidebar_addRecord, {
        try({
                newEntry <- data.frame(RECORD_NO = input$sidebar_flaggedRecord,
                                       SITE_NO = unique(qw.data$PlotTable$SITE_NO[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                input$sidebar_flaggedRecord)]
                                       ),
                                       STATION_NM = unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                      input$sidebar_flaggedRecord)]
                                       ),
                                       SAMPLE_START_DT = as.character(unique(qw.data$PlotTable$SAMPLE_START_DT[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                                             input$sidebar_flaggedRecord)])
                                       ),
                                       MEDIUM_CD = unique(qw.data$PlotTable$MEDIUM_CD[which(qw.data$PlotTable$RECORD_NO == 
                                                                                                    input$sidebar_flaggedRecord)]
                                       ),
                                       DQI_CD = NA,
                                       PARM_CD = NA,
                                       PARM_NM = NA,
                                       Where_Flagged = "sidebar",
                                       Comment = input$sidebar_flaggedComment
                )
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "sidebar_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "sidebar_flaggedComment",
                                value = " "
                )
                
        })
})
