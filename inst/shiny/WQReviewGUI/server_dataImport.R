####This generates some summary plots####
###It goes outside the observe so that it displays errors
output$sampleCountHist <- renderPlot({
        validate(
                need(!is.null(input$STAIDS) | !is.null(input$siteFile), "Please enter a site number or a site ID file"),
                need(!is.null(input$dl.parms) | !is.null(input$parmFile), "Please enter a parameter group(s) or a parameter code file")
                
        )

        #sampleCountHist <- ggplot(data=qw.data$PlotTable[!duplicated(qw.data$PlotTable$RECORD_NO),],aes(x=year(SAMPLE_START_DT)))
        #sampleCountHist <- sampleCountHist + geom_histogram(colour = "darkgreen", fill = "white",binwidth=1) + theme_bw()
        #sampleCountHist <- sampleCountHist + ylab("Sample count") + xlab("Calendar year")
        #sampleCountHist
})




observeEvent(input$dataDownload, {
        
        siteFile <- input$siteFile
        parmFile <- input$parmFile
        
        tryCatch({
        
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
        #)
        
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
                },error=function(e){})
                

})

