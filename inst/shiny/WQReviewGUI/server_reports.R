######################################
#######Run report generation##########
######################################

tryCatch({
  ###Run ion balance function
  ionBalanceOut <- suppressWarnings(ionBalance(qw.data = qw.data,wide=TRUE))
  chargebalance.table <- ionBalanceOut$chargebalance.table
  chargebalance.table$RECORD_NO <- as.character(chargebalance.table$RECORD_NO)
  reports$BalanceDataTable <<- ionBalanceOut$BalanceDataTable
  reports$balanceTable <<- ionBalanceOut$chargebalance.table
  
  
  ###Check that a balance was calculated
  if(nrow(chargebalance.table) > 0)
  {
    
    ###Join charge balance table to plot table
    chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
    qw.data$PlotTable <<- dplyr::left_join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
    
    
  } else {}
  
}, error = function(e) {
  output$errors <- renderPrint("Error with charge balance. please report this on the github issues page")
  
})


tryCatch({
  
  ###Run repTabler
  reports$repTable <<- suppressWarnings(repTabler(qw.data))
  
  
}, error = function(e) {
  output$errors <- renderPrint("Error with replicate table. please report this on the github issues page")
})


tryCatch({
  ###Run wholevPart
  reports$wholevpartTable <<- suppressWarnings(wholevPart(qw.data))
  
}, error = function(e) {
  output$errors <- renderPrint("Error with whole vs part table. please report this on the github issues page")
  
})


###Generate chem flag summary
tryCatch({
  reports$chemFlagTable <<- suppressWarnings(chemCheck(qw.data,returnAll = FALSE))
  
  #Fill first row with NAs so its not an empty table or excel.link bombs
  if(nrow(reports$chemFlagTable) == 0)
  {
    reports$chemFlagTable[1,1] <<- "No Flags"
  }
  
  reports$badCBTable <<- reports$chemFlagTable[c("RECORD_NO",
                                                 "SITE_NO",
                                                 "STATION_NM",
                                                 "SAMPLE_START_DT",
                                                 "SAMPLE_END_DT",
                                                 "MEDIUM_CD",
                                                 "SAMPLE_CM_TX",
                                                 "BadCB_30.21")]
  
  
  reports$badCBTable$NEW_SAMPLE_CM_NWIS <<- NA
  reports$badCBTable$SAMPLE_NOTE_NO_NWIS <<- NA

  if(nrow(reports$badCBTable) > 0)
  {
    reports$badCBTable <<- reports$badCBTable[!is.na(reports$badCBTable$BadCB_30.21),]
  }
  
  if(nrow(reports$badCBTable) == 0)
  {
    reports$badCBTable[1,1] <<- "No Flags"
  }
  
  
  
},error = function(e) {
  output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
})

###Generate pest flag summary
tryCatch({
  reports$pestFlagTable <<- suppressWarnings(pestCheck(qw.data))
},
error = function(e) {
  output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
})

###Generate sample flag summary
# tryCatch({
#         reports$sampleFlagTable <<- suppressWarnings(flagSummary(qw.data))
#         ###Need to reset row names
#         rownames(reports$sampleFlagTable) <<- seq(from = 1, to = nrow(reports$sampleFlagTable))
# },
# error = function(e) {
#         output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
# })

###Generate result flags
tryCatch({
  reports$resultFlagTable <<- suppressWarnings(historicCheck(qw.data,returnAll=FALSE))
  ###Need to reset row names
  rownames(reports$resultFlagTable) <<- seq(from = 1, to = nrow(reports$resultFlagTable))
},
error = function(e) {
  output$errors <- renderPrint("Error with auto result flagging, please report this on the github issues page")
})

tryCatch({
  ###Run blnk table summary
  reports$blankTable_all <<- suppressWarnings(blankSummary(qw.data, STAIDS = unique(qw.data$PlotTable$SITE_NO), multiple = FALSE))
  
  
  
}, warning = function(w) {
  ###Run blnk table summary
  reports$blankTable_all <<- blankSummary(qw.data, multiple = FALSE)
  warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
}, error = function(e) {
  
  output$errors <- renderPrint("Error with blank table summary. Please check your data import criteria and QWToolbox manual and if not user error report this whole message on the github issues page and generate a bug report using the button at the top of QWToolbox")
  
})

tryCatch({
  ###Summarize flags
  # reports$flaggedData <<- suppressWarnings(flagSummary(qw.data))
  
  # reports$unaprovedData <<- suppressWarnings(needsReview(qw.data,flaggedResults = reports$flaggedData))
  # reports$unaprovedData <<- dplyr::arrange(reports$unaprovedData,PARM_SEQ_GRP_CD,desc(FLAGGED))
  # 
  # names(reports$unaprovedData) <<- paste0(names(reports$unaprovedData),"_UNAP") 
  # reports$unaprovedData$NEW_RESULT_CM_NWIS <<- NA
  # reports$unaprovedData$RESULT_NOTE_NO_NWIS <<- NA
  
  # reports$unaprovedData <<- qw.data$PlotTable[qw.data$PlotTable$DQI_CD %in% c("I","S","P"),
  #                           c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PARM_CD","PARM_NM","PARM_SEQ_GRP_CD","DQI_CD","RESULT_CM_TX")]
  
  reports$unaprovedData <<- suppressWarnings(historicCheck(qw.data,returnAll=TRUE))
  reports$unaprovedData <<- reports$unaprovedData[c("RECORD_NO",
                                                    "SITE_NO",
                                                    "STATION_NM",
                                                    "SAMPLE_START_DT",
                                                    "MEDIUM_CD",
                                                    "PARM_CD",
                                                    "PARM_NM",
                                                    "DQI_CD",
                                                    "PARM_SEQ_GRP_CD",
                                                    "newMax_30.11",
                                                    "newMin_30.12",
                                                    "greaterQuant99_30.15",
                                                    "lessQuant01_30.16",
                                                    "unusualNonDetect")]
  reports$unaprovedData$hasFlags <<- ifelse(!is.na(reports$unaprovedData$newMax_30.11) |
                                              !is.na(reports$unaprovedData$newMin_30.12) |
                                              !is.na(reports$unaprovedData$greaterQuant99_30.15) |
                                              !is.na(reports$unaprovedData$lessQuant01_30.16) |
                                              !is.na(reports$unaprovedData$unusualNonDetect),
                                            TRUE,FALSE)
  reports$unaprovedData$FLAGS <<- paste(reports$unaprovedData$newMax_30.11,
                                        reports$unaprovedData$newMin_30.12,
                                        reports$unaprovedData$greaterQuant99_30.15,
                                        reports$unaprovedData$lessQuant01_30.16,
                                        reports$unaprovedData$unusualNonDetect,
                                        sep=" ")
  reports$unaprovedData$FLAGS <<- gsub("NA","",reports$unaprovedData$FLAGS)
  reports$unaprovedData <<- dplyr::arrange(reports$unaprovedData,PARM_SEQ_GRP_CD,desc(hasFlags))
  reports$hasFlags <<- NULL
  reports$unaprovedData <<- reports$unaprovedData[c("RECORD_NO",
                                                    "SITE_NO",
                                                    "STATION_NM",
                                                    "SAMPLE_START_DT",
                                                    "MEDIUM_CD",
                                                    "PARM_CD",
                                                    "PARM_NM",
                                                    "DQI_CD",
                                                    "PARM_SEQ_GRP_CD",
                                                    "FLAGS")]
  
  
  
}, error = function(e) {
  output$errors <- renderPrint("Error with blank table summary. Please check your data import criteria and QWToolbox manual and if not user error report this whole message on the github issues page and generate a bug report using the button at the top of QWToolbox")
})

