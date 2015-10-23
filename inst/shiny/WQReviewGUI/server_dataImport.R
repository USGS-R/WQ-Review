####This generates some summary plots####
###It goes outside the observe so that it displays errors
output$importWarning <- renderPrint({
        validate(
                need(!is.null(input$STAIDS) | !is.null(input$siteFile), "Please enter a site number or a site ID file"),
                need(!is.null(input$dl.parms) | !is.null(input$parmFile) | !is.null(input$dl.parms.group), "Please enter a parameter group(s) or a parameter code file"),
                need(input$DSN != "", "Please enter your DSN name for your NWIS server")
        )

        #sampleCountHist <- ggplot(data=qw.data$PlotTable[!duplicated(qw.data$PlotTable$RECORD_NO),],aes(x=year(SAMPLE_START_DT)))
        #sampleCountHist <- sampleCountHist + geom_histogram(colour = "darkgreen", fill = "white",binwidth=1) + theme_bw()
        #sampleCountHist <- sampleCountHist + ylab("Sample count") + xlab("Calendar year")
        #sampleCountHist
})




observeEvent(input$dataDownload, {
        
        qw.data <<- NULL
        reports <<- NULL
        errors <<- ""
        
        siteFile <- input$siteFile
        parmFile <- input$parmFile

        
        tryCatch({
        
        ###Check for parm group input or pcode file
                
        if(!(is.null(parmFile)))
        {
                parm.group.check <- FALSE
                dl.parms <- scan(parmFile$datapath,what="character")
        } else if (!is.null(input$dl.parms)) {
                parm.group.check <- FALSE
                dl.parms <- as.character(input$dl.parms)
        } else {parm.group.check <- TRUE
               dl.parms <- input$dl.parms.group
               }
        
        ###Check for single site id input or site file
        if(!(is.null(siteFile)))
        {
                STAIDS<-scan(siteFile$datapath,what="character")
        } else(STAIDS<-input$STAIDS)


        ###Run all the data import and report generation functions
        source("server_importandreports.r",local=TRUE)$value
        
        ###Check for succesful data import
        if(!is.null(qw.data))
        {
                
        ###Update header
        source("server_header.r",local=TRUE)$value
        
        ###Print retrieval info
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
        } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}
                },warning = function(w) {
                
                ###Check for parm group input or pcode file
                
                if(!(is.null(parmFile)))
                {
                        parm.group.check <- FALSE
                        dl.parms <- scan(parmFile$datapath,what="character")
                } else if (!is.null(input$dl.parms)) {
                        parm.group.check <- FALSE
                        dl.parms <- as.character(input$dl.parms)
                } else {parm.group.check <- TRUE
                dl.parms <- input$dl.parms.group
                }
                
                ###Check for single site id input or site file
                if(!(is.null(siteFile)))
                {
                        STAIDS<-scan(siteFile$datapath,what="character")
                } else(STAIDS<-input$STAIDS)
                
                
                ###Run all the data import and report generation functions
                source("server_importandreports.r",local=TRUE)$value
                        
                ###Check for succesful data import
                if(!is.null(qw.data))
                {
                
                ###Update header
                source("server_header.r",local=TRUE)$value
                
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
                
                } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}
        }, error = function(e) {} )
        
        
        ###Check for succesful data import
        if(!is.null(qw.data))
        {
        
        
        tryCatch({
                

        
        
        ###############################################
        ###Load all plotting elements of server, they can't run 
        ###Until afte rthe data has been imported
        ###############################################
        source("server_tablesandplots.r",local=TRUE)$value
        
        
        ###############################################
        ###Update side bar elements with appropriate inputs based on data import
        ###############################################
        parmSelData <- unique(qw.data$PlotTable[c("PARM_CD","PARM_NM")])
        ###Reorder by parm_cd
        parmSelData <- parmSelData[order(parmSelData$PARM_CD,parmSelData$PARM_NM),]
        
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
        #)
        
        ################################
        ###blank table inputs update####
        ################################
        updateSelectInput(session, "siteSel_blankTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###balance table inputs update####
        ################################
        updateSelectInput(session, "siteSel_balanceTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###long table inputs update####
        ################################
        updateSelectInput(session, "siteSel_longDataTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###rep table inputs update####
        ################################
        updateSelectInput(session, "siteSel_repTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###wholevpart table inputs update####
        ################################
        updateSelectInput(session, "siteSel_wholevpartTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        ################################
        ###wide table inputs update####
        ################################
        updateSelectInput(session, "siteSel_wideDataTable",
                          choices = setNames((siteSelData$SITE_NO),
                                             paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
        )
        
        },warning = function(w) {
                ###Check for parm group input or pcode file
                if(!(is.null(parmFile)))
                {
                        parm.group.check <- FALSE
                        dl.parms <- scan(parmFile$datapath,what="character")
                } else{parm.group.check <- TRUE
                       dl.parms <- input$dl.parms}
                
                ###Check for single site id input or site file
                if(!(is.null(siteFile)))
                {
                        STAIDS<-scan(siteFile$datapath,what="character")
                } else(STAIDS<-input$STAIDS)
                
                
                ###Run all the data import and report generation functions
                source("server_importandreports.r",local=TRUE)$value
                
                ###Update header
                source("server_header.r",local=TRUE)$value
                
                
                
                
                ###############################################
                ###Load all plotting elements of server, they can't run 
                ###Until afte rthe data has been imported
                ###############################################
                source("server_tablesandplots.r",local=TRUE)$value
                
                parmSelData <- unique(qw.data$PlotTable[c("PARM_CD","PARM_NM")])
                
                
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
                #)
                
                ################################
                ###blank table inputs update####
                ################################
                updateSelectInput(session, "siteSel_blankTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                
                ################################
                ###balance table inputs update####
                ################################
                updateSelectInput(session, "siteSel_balanceTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                
                ################################
                ###long table inputs update####
                ################################
                updateSelectInput(session, "siteSel_longDataTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                
                ################################
                ###rep table inputs update####
                ################################
                updateSelectInput(session, "siteSel_repTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                
                ################################
                ###wholevpart table inputs update####
                ################################
                updateSelectInput(session, "siteSel_wholevpartTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                
                ################################
                ###wide table inputs update####
                ################################
                updateSelectInput(session, "siteSel_wideDataTable",
                                  choices = setNames((siteSelData$SITE_NO),
                                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                )
                },error=function(e){})
        
        ###Clear error messages on successful data import
        output$errors <- renderPrint("")
        output$shinyErrors <- renderPrint("")
        
        } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}

})


