


####################################
###Save data after import
####################################
output$saveData <- downloadHandler(
        filename = function() {"saveData"},
        content = function(file) {
                save.image(file)
        }
)

#####################
###Load saved data
#####################

output$loadWarning <- renderPrint({
        validate(
                need(!is.null(input$loadDataFile), "Please select a file to load")

        )
        
        #sampleCountHist <- ggplot(data=qw.data$PlotTable[!duplicated(qw.data$PlotTable$RECORD_NO),],aes(x=year(SAMPLE_START_DT)))
        #sampleCountHist <- sampleCountHist + geom_histogram(colour = "darkgreen", fill = "white",binwidth=1) + theme_bw()
        #sampleCountHist <- sampleCountHist + ylab("Sample count") + xlab("Calendar year")
        #sampleCountHist
})


observeEvent(input$loadData, {

        loadDataFile <- input$loadDataFile
        
        if(!is.null(loadDataFile))
        {
                tryCatch({
                load(loadDataFile$datapath)
                source("server_header.r",local=TRUE)$value
                },warning = function(w) {
                        load(loadDataFile$datapath)
                        source("server_header.r",local=TRUE)$value
                },error = function(e){}
                )
        } else{}
        
tryCatch({
###Print retrieval info
output$headerReminder <- renderText({
        print("Please see notifications at the top-right of this page for important information regarding your data")
})
output$samplesRetrieved <- renderText({
        print(paste("Samples retrieved:",length(unique(qw.data$PlotTable$RECORD_NO))))
})
output$resultsRetrieved <- renderText({
        print(paste("Results retrieved:",length(unique(qw.data$PlotTable$RESULT_VA))))
})
output$sampleModified <- renderText({
        print("Most recent sample modification")
})
output$recordModified <- renderText({
        print(unique(paste("Record:",qw.data$PlotTable$RECORD_NO[which(qw.data$PlotTable$SAMPLE_MD == max(qw.data$PlotTable$SAMPLE_MD))])))
})
output$recordModifiedDate <- renderText({
        print(unique(paste("Date:",max(qw.data$PlotTable$SAMPLE_MD))))
})
output$recordModifiedName <- renderText({
        print(unique(paste("Name:",qw.data$PlotTable$SAMPLE_MN[which(qw.data$PlotTable$SAMPLE_MD == max(qw.data$PlotTable$SAMPLE_MD))])))
})
output$resultModified <- renderText({
        print("Most recent result modification")
})
output$resultRecordModified <- renderText({
        print(unique(paste("Record:",qw.data$PlotTable$RECORD_NO[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
})
output$resultModifiedPARM <- renderText({
        print(unique(paste("P-Code",qw.data$PlotTable$PARM_CD[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
})
output$resultModifiedDate <- renderText({
        print(unique(paste("Date:",max(qw.data$PlotTable$RESULT_MD))))
})
output$resultModifiedName <- renderText({
        print(unique(paste("Name:",qw.data$PlotTable$RESULT_MN[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
})

###############################################
###Load all plotting elements of server, they can't run 
###Until afte rthe data has been imported
###############################################
source("server_tablesandplots.r",local=TRUE)$value


###############################################
###Update side bar elements with appropriate inputs based on data import
###############################################
parmSelData <- unique(qw.data$PlotTable[c("PARM_CD","PARM_NM")])
siteSelData <- unique(qw.data$PlotTable[c("SITE_NO","STATION_NM")])

updateSelectInput(session, "siteSel_TS",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_TS",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)
################################
###Seasonal plot inputs update####
################################
updateSelectInput(session, "siteSel_seasonal",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_seasonal",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###parmBox plot inputs update####
################################
updateSelectInput(session, "siteSel_parmBox",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_parmBox",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###parmParmplot inputs update####
################################

updateSelectInput(session, "siteSel_parmParm",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_parmParmX",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)
updateSelectInput(session, "parmSel_parmParmY",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###Matrix plot inputs update####
################################

updateSelectInput(session, "siteSel_matrix",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_matrix",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###Chargebalance plot inputs update####
################################

updateSelectInput(session, "siteSel_cb",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

################################
###SC Sum Plot plot inputs update####
################################

updateSelectInput(session, "siteSel_scSum",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

################################
###repBox plot inputs update####
################################
updateSelectInput(session, "siteSel_repBox",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_repBox",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###blank plot inputs update####
################################
updateSelectInput(session, "siteSel_blank",
                  choices = setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
)

updateSelectInput(session, "parmSel_blank",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###MAp plot inputs update####
################################
#updateSelectInput(session, "siteSel_map",
#                  choices = setNames((siteSelData$SITE_NO),
#paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#)

#updateSelectInput(session, "parmSel_map",
#                     choices = setNames((parmSelData$PARM_CD),
#                                        paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
#
},warning = function(w) {
        ###Print retrieval info
        output$headerReminder <- renderText({
                print("Please see notifications at the top-right of this page for important information regarding your data")
        })
        output$samplesRetrieved <- renderText({
                print(paste("Samples retrieved:",length(unique(qw.data$PlotTable$RECORD_NO))))
        })
        output$resultsRetrieved <- renderText({
                print(paste("Results retrieved:",length(unique(qw.data$PlotTable$RESULT_VA))))
        })
        output$sampleModified <- renderText({
                print("Most recent sample modification")
        })
        output$recordModified <- renderText({
                print(unique(paste("Record:",qw.data$PlotTable$RECORD_NO[which(qw.data$PlotTable$SAMPLE_MD == max(qw.data$PlotTable$SAMPLE_MD))])))
        })
        output$recordModifiedDate <- renderText({
                print(unique(paste("Date:",max(qw.data$PlotTable$SAMPLE_MD))))
        })
        output$recordModifiedName <- renderText({
                print(unique(paste("Name:",qw.data$PlotTable$SAMPLE_MN[which(qw.data$PlotTable$SAMPLE_MD == max(qw.data$PlotTable$SAMPLE_MD))])))
        })
        output$resultModified <- renderText({
                print("Most recent result modification")
        })
        output$resultRecordModified <- renderText({
                print(unique(paste("Record:",qw.data$PlotTable$RECORD_NO[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
        })
        output$resultModifiedPARM <- renderText({
                print(unique(paste("P-Code",qw.data$PlotTable$PARM_CD[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
        })
        output$resultModifiedDate <- renderText({
                print(unique(paste("Date:",max(qw.data$PlotTable$RESULT_MD))))
        })
        output$resultModifiedName <- renderText({
                print(unique(paste("Name:",qw.data$PlotTable$RESULT_MN[which(qw.data$PlotTable$RESULT_MD == max(qw.data$PlotTable$RESULT_MD))])))
        })
        
        ###############################################
        ###Load all plotting elements of server, they can't run 
        ###Until afte rthe data has been imported
        ###############################################
        source("server_tablesandplots.r",local=TRUE)$value
        
        
        ###############################################
        ###Update side bar elements with appropriate inputs based on data import
        ###############################################
        parmSelData <- unique(qw.data$PlotTable[c("PARM_CD","PARM_NM")])
        siteSelData <- unique(qw.data$PlotTable[c("SITE_NO","STATION_NM")])
        
        updateSelectInput(session, "siteSel_TS",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_TS",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        ################################
        ###Seasonal plot inputs update####
        ################################
        updateSelectInput(session, "siteSel_seasonal",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_seasonal",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###parmBox plot inputs update####
        ################################
        updateSelectInput(session, "siteSel_parmBox",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_parmBox",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###parmParmplot inputs update####
        ################################
        
        updateSelectInput(session, "siteSel_parmParm",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_parmParmX",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        updateSelectInput(session, "parmSel_parmParmY",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###Matrix plot inputs update####
        ################################
        
        updateSelectInput(session, "siteSel_matrix",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_matrix",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###Chargebalance plot inputs update####
        ################################
        
        updateSelectInput(session, "siteSel_cb",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###SC Sum Plot plot inputs update####
        ################################
        
        updateSelectInput(session, "siteSel_scSum",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###repBox plot inputs update####
        ################################
        updateSelectInput(session, "siteSel_repBox",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_repBox",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###blank plot inputs update####
        ################################
        updateSelectInput(session, "siteSel_blank",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        updateSelectInput(session, "parmSel_blank",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
        
        ################################
        ###MAp plot inputs update####
        ################################
        #updateSelectInput(session, "siteSel_map",
        #                  choices = setNames((siteSelData$SITE_NO),
        #paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        #)
        
        #updateSelectInput(session, "parmSel_map",
        #                     choices = setNames((parmSelData$PARM_CD),
        #                                        paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        #
},error = function(e) {}
)


})
