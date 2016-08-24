###This creates a new entry in the marked record table
observeEvent(input$sidebar_addRecord, {
        try({
                if(length(strsplit(input$parmSel_sidebar,",")[[1]]) >0 )
                {
                newEntry <- data.frame(RECORD_NO = strsplit(input$sidebar_flaggedRecord,",")[[1]],
                                       PARM_CD = strsplit(input$parmSel_sidebar,",")[[1]])
                } else {
                        newEntry <- data.frame(RECORD_NO = strsplit(input$sidebar_flaggedRecord,",")[[1]],
                                               PARM_CD = rep("",length(strsplit(input$sidebar_flaggedRecord,",")[[1]])))
                }
                newEntry$DQI_CD_New <- rep(input$sidebar_dqiCode,nrow(newEntry))
                newEntry$Status <- rep(input$sidebar_flaggedStatus,nrow(newEntry))
                newEntry$Comment <- rep(input$sidebar_flaggedComment,nrow(newEntry))
                
                ###Join to sample meta data
                newEntry <- dplyr::left_join(newEntry,unique(plotTable[c("RECORD_NO",
                                                                                 "PARM_CD",
                                                                                 "PARM_NM",
                                                                                 "SITE_NO",
                                                                                 "STATION_NM",
                                                                                 "SAMPLE_START_DT",
                                                                                 "MEDIUM_CD",
                                                                                 "DQI_CD")]),
                                                             by=c("RECORD_NO","PARM_CD"))
                newEntry <- dplyr::rename(newEntry,DQI_CD_Current = DQI_CD)
                
                newEntry <- newEntry[c("RECORD_NO",
                                       "SITE_NO",
                                       "STATION_NM",
                                       "SAMPLE_START_DT",
                                       "MEDIUM_CD",
                                       "PARM_CD",
                                       "DQI_CD_Current",
                                       "DQI_CD_New",
                                       "PARM_NM",
                                       "Status",
                                       "Comment")]

                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "sidebar_flaggedRecord",
                                value = ""
                )
                updateTextInput(session, 
                                "parmSel_sidebar",
                                value = ""
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
})


