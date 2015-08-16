
######################################################
###This generates content for the alerts dropdown menu
######################################################


tryCatch({
newSampleCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$SAMPLE_CR > Sys.time() - 60*60*24*7
                                                     &!duplicated(qw.data$PlotTable$RECORD_NO)])
newResultCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$RESULT_CR > Sys.time() - 60*60*24*7])
modifiedCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$RESULT_MD > Sys.time() - 60*60*24*7])
totalSampleCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                                                       &!duplicated(qw.data$PlotTable$RECORD_NO)])
totalResultCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3])
},warning = function(w) {newSampleCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$SAMPLE_CR > Sys.time() - 60*60*24*7
                                                                              &!duplicated(qw.data$PlotTable$RECORD_NO)])
                         newResultCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$RESULT_CR > Sys.time() - 60*60*24*7])
                         modifiedCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$RESULT_MD > Sys.time() - 60*60*24*7])
                         totalSampleCount <- length(qw.data$PlotTable$RECORD_NO[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                                                                                &!duplicated(qw.data$PlotTable$RECORD_NO)])
                         totalResultCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3])
},error = function(e) {
        newSampleCount <- ""
        newResultCount <- ""
        modifiedCount <- ""
        totalSampleCount <- ""
        totalResultCount <- ""
})

alertData <- data.frame(
        text = c(paste(newSampleCount,"New samples in past 7 days"),
                 paste(newResultCount,"New results in past 7 days"),
                 paste(modifiedCount,"Modified results in past 7 days"),
                 paste("3-year sample count",totalSampleCount),
                 paste("3-year result count",totalResultCount)),
        icon = c("exclamation-triangle",
                 "exclamation-triangle",
                 "exclamation-triangle",
                 "info",
                 "info"),
        status = c("warning",
                   "warning",
                   "warning",
                   "info",
                   "info"),
        stringsAsFactors = FALSE
)


output$alerts <- renderMenu({

                alerts <- apply(alertData, 1, function(row) {
                        notificationItem(
                                text = row[["text"]],
                                icon = icon(row[["icon"]]),
                                status=row[["status"]]
                        )
                })
                
                dropdownMenu(type = "notifications", .list = alerts)
        })

######################################################
###This generates content for the progress dropdown menu
######################################################

tryCatch({
reviewedCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                        & (qw.data$PlotTable$DQI_CD %in% c("R","Q","O","X","U","A"))])/totalResultCount*100

waitingCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                                                    & qw.data$PlotTable$DQI_CD %in% c("I","S","P")])/totalResultCount*100
}, warning = function(w) {
        reviewedCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                                                            & (qw.data$PlotTable$DQI_CD %in% c("R","Q"))])/totalResultCount*100
        
        waitingCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365*3
                                                           & qw.data$PlotTable$DQI_CD %in% c("I","S")])/totalResultCount*100
}, error = function(e) {
        reviewedCount <- ""
        waitingCount <- ""
})
taskData <- data.frame(
        text = c("Reviewed results (last 3years)", "Waiting for review (last 3years)"),
        value = c(
                round(reviewedCount,0),
                round(waitingCount,0)
                ),
        color= c("green","red"),
        stringsAsFactors = FALSE
)


output$progress <- renderMenu({

        tasks <- apply(taskData, 1, function(row) {
                taskItem(
                        text = row[["text"]],
                        value = row[["value"]],
                        color = row[["color"]]
                )
        })
        
        dropdownMenu(type = "tasks", .list = tasks)
})


######################################################
###This generates content for the messages dropdown menu
######################################################
overdueCount <- ""
wayOverdueCount <- ""

tryCatch({
overdueCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*30*3
                                                    & qw.data$PlotTable$DQI_CD %in% c("I","S")])

wayOverdueCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365
                                                   & qw.data$PlotTable$DQI_CD %in% c("I","S")])
},warning = function(w) {
        overdueCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*30*3
                                                           & qw.data$PlotTable$DQI_CD %in% c("I","S")])
        
        wayOverdueCount <- length(qw.data$PlotTable$RESULT_VA[qw.data$PlotTable$SAMPLE_START_DT > Sys.time() - 60*60*24*365
                                                              & qw.data$PlotTable$DQI_CD %in% c("I","S")])
}, error = function(e) {
        
        overdueCount <- 0
        wayOverdueCount <- 0
})

output$messages <- renderMenu({
        dropdownMenu(type = "messages",

        messageItem(
                from="DQI Police",
                message = paste(overdueCount,"results in DQI of I or S > 3 months."),
                icon = icon("exclamation-triangle")
                ),
        

        
                messageItem(
                        from="DQI Police",
                        message = paste(wayOverdueCount,"results in DQI of I or S > 1 year!."),
                        icon = icon("exclamation-triangle")
                        )
        
      
        )
        
})