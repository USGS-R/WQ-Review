
output$importWarning <- renderPrint({
        validate(
                need(!is.null(input$STAIDS) | !is.null(input$siteFile), "Please enter a site number or a site ID file"),
                need(!is.null(input$dl.parms) | !is.null(input$parmFile) | !is.null(input$dl.parms.group), "Please enter a parameter group(s) or a parameter code file"),
                need(input$DSN != "", "Please enter your DSN name for your NWIS server"),
                need(input$env.db != "", "Please enter the number of your environmental sample database, e.g. '01' "),
                need(input$qa.db != "", "Please enter the number of your QA sample database, e.g. '02' ")
                
        )

        #sampleCountHist <- ggplot(data=plotTable[!duplicated(plotTable$RECORD_NO),],aes(x=year(SAMPLE_START_DT)))
        #sampleCountHist <- sampleCountHist + geom_histogram(colour = "darkgreen", fill = "white",binwidth=1) + theme_bw()
        #sampleCountHist <- sampleCountHist + ylab("Sample count") + xlab("Calendar year")
        #sampleCountHist
})




observeEvent(input$dataDownload, {
        validate(
                need(!is.null(input$STAIDS) | !is.null(input$siteFile), "Please enter a site number or a site ID file"),
                need(!is.null(input$dl.parms) | !is.null(input$parmFile) | !is.null(input$dl.parms.group), "Please enter a parameter group(s) or a parameter code file"),
                need(input$DSN != "", "Please enter your DSN name for your NWIS server")
        )
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
                        print(paste("Samples retrieved:",length(unique(plotTable$RECORD_NO))))
                })
                output$resultsRetrieved <- renderText({
                        print(paste("Results retrieved:",length(unique(plotTable$RESULT_VA))))
                })
                output$sampleModified <- renderText({
                        print("Most recent sample modification")
                })
                output$recordModified <- renderText({
                        print(unique(paste("Record:",plotTable$RECORD_NO[which(plotTable$SAMPLE_MD == max(plotTable$SAMPLE_MD))])))
                })
                output$recordModifiedDate <- renderText({
                        print(unique(paste("Date:",max(plotTable$SAMPLE_MD))))
                })
                output$recordModifiedName <- renderText({
                        print(unique(paste("Name:",plotTable$SAMPLE_MN[which(plotTable$SAMPLE_MD == max(plotTable$SAMPLE_MD))])))
                })
                output$resultModified <- renderText({
                        print("Most recent result modification")
                })
                output$resultRecordModified <- renderText({
                        print(unique(paste("Record:",plotTable$RECORD_NO[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
                })
                output$resultModifiedPARM <- renderText({
                        print(unique(paste("P-Code",plotTable$PARM_CD[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
                })
                output$resultModifiedDate <- renderText({
                        print(unique(paste("Date:",max(plotTable$RESULT_MD))))
                })
                output$resultModifiedName <- renderText({
                        print(unique(paste("Name:",plotTable$RESULT_MN[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
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
                        print(paste("Samples retrieved:",length(unique(plotTable$RECORD_NO))))
                })
                output$resultsRetrieved <- renderText({
                        print(paste("Results retrieved:",length(unique(plotTable$RESULT_VA))))
                })
                output$sampleModified <- renderText({
                        print("Most recent sample modification")
                })
                output$recordModified <- renderText({
                        print(unique(paste("Record:",plotTable$RECORD_NO[which(plotTable$SAMPLE_MD == max(plotTable$SAMPLE_MD))])))
                })
                output$recordModifiedDate <- renderText({
                        print(unique(paste("Date:",max(plotTable$SAMPLE_MD))))
                })
                output$recordModifiedName <- renderText({
                        print(unique(paste("Name:",plotTable$SAMPLE_MN[which(plotTable$SAMPLE_MD == max(plotTable$SAMPLE_MD))])))
                })
                output$resultModified <- renderText({
                        print("Most recent result modification")
                })
                output$resultRecordModified <- renderText({
                        print(unique(paste("Record:",plotTable$RECORD_NO[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
                })
                output$resultModifiedPARM <- renderText({
                        print(unique(paste("P-Code",plotTable$PARM_CD[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
                })
                output$resultModifiedDate <- renderText({
                        print(unique(paste("Date:",max(plotTable$RESULT_MD))))
                })
                output$resultModifiedName <- renderText({
                        print(unique(paste("Name:",plotTable$RESULT_MN[which(plotTable$RESULT_MD == max(plotTable$RESULT_MD))])))
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
        source("server_updateInputs.r",local=TRUE)$value
                
        
        
        
        },error=function(e){})
        
        ###Clear error messages on successful data import
        output$errors <- renderPrint("")
        output$shinyErrors <- renderPrint("")
        
        } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}

})


