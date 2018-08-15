###Server file for flipping DQI codes
observeEvent(input$flipDQI,{
  STAIDS <- unique(xl_DQI$SITE_NO)
  records <- xl_DQI$RECORD_NO
  parms <- xl_DQI$PARM_CD
  dqis <- xl_DQI$DQI_CD_REVIEWED
  comments <- xl_DQI$RESULT_CM_NWIS_REVIEWED
  commentType <- xl_DQI$RESULT_CM_TYPE_NWIS_REVIEWED
  
  comments[is.na(comments)] <- ""
  commentType[is.na(commentType)] <- ""
  
  commentType[!(commentType %in% c("F","L"))] <- "F"
  
  batchFiles <<- flipDQI(STAIDS = STAIDS,
                         records = records,
                         parameter = parms,
                         dqiCodes = dqis,
                         comments = comments,
                         commentType = commentType,
                         DSN = pullCriteria$DSN,
                         env.db = pullCriteria$env.db,
                         qa.db = pullCriteria$qa.db)
  
  tryCatch({
          xl.sheet.activate("Reviewed unapproved data")
          xl.sheet.delete("QWSample")
          xl.sheet.delete("QWResult")
  },error = function(e){})
  tryCatch({
          xl.sheet.add("QWSample")
          xlc$a1 = batchFiles$qwsample
          
          xl.sheet.add("QWResult")
          xlc$a1 = batchFiles$qwresult         
  },error = function(e){})
  
  # tryCatch({
  # xl.sheet.add("QWSample")
  # xlc$a1 = batchFiles$qwsample
  # 
  # xl.sheet.add("QWResult")
  # xlc$a1 = batchFiles$qwresult
  # },error = function(e) {
  #   xl.sheet.delete("QWSample")
  #   xl.sheet.delete("QWResult")
  #   
  #   xl.sheet.add("QWSample")
  #   xlc$a1 = batchFiles$qwsample
  #   
  #   xl.sheet.add("QWResult")
  #   xlc$a1 = batchFiles$qwresult
  # })
  
})
                           
                           