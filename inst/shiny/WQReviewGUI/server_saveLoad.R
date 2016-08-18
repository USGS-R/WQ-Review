


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
        
        #sampleCountHist <- ggplot(data=plotTable[!duplicated(plotTable$RECORD_NO),],aes(x=year(SAMPLE_START_DT)))
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
                        qw.data <<- qw.data
                        reports <<- reports
                source("server_header.r",local=TRUE)$value
                },warning = function(w) {
                        load(loadDataFile$datapath)
                        qw.data <<- qw.data
                        reports <<- reports
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

###############################################
###Load all plotting elements of server, they can't run 
###Until afte rthe data has been imported
###############################################
source("server_tablesandplots.r",local=TRUE)$value


###############################################
###Update side bar elements with appropriate inputs based on data import
###############################################
parmSelData <- unique(plotTable[c("PARM_CD","PARM_NM")])
siteSelData <- unique(plotTable[c("SITE_NO","STATION_NM")])

source("server_updateInputs.r",local=TRUE)$value

},error = function(e) {}
)


})
