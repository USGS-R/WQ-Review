


####################################
###Save data after import
####################################
output$saveRData <- downloadHandler(
  filename = function() {paste0("WQRevOUT_",Sys.time(),".rda")},
  content = function(file) {
    save(list=c("pullCriteria","medium.colors","errors","qual.shapes","qw.data","reports"),file=file)
  }
)


#####################
###Load saved data
#####################

output$loadWarning <- renderText({
  validate(
    need(!is.null(input$loadRDataFile), "Please select an R data file to load"),
    need(!is.null(input$loadXLDataFile), "Please select the cooresponding XL workbook to load")
    
    
  )
  
})


observeEvent(input$loadData, {
  
  loadRDataFile <- input$loadRDataFile
  loadXLDataFile <- input$loadXLDataFile
  
  
  if(!is.null(loadRDataFile) &
     !is.null(loadXLDataFile))
  {
    tryCatch({
      load(loadRDataFile$datapath,envir = .GlobalEnv)

      
      excel.link::xl.workbook.open(loadXLDataFile$datapath)
      ###Link excel tables
      xl.sheet.activate("Charge balance issues")
      xl_CB <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)
      
      xl.sheet.activate("Ready for DQI change")
      xl_DQI <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)
      
      xl.sheet.activate("DQI needs review")
      xl_UNAP <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)
      
    },error = function(e){}
    )
  } else{}
  
  tryCatch({
    source("server_reports.r",local=TRUE)$value
    }, error = function(e) {} )
  
  
  ##################################################
  
  ###Check for succesful data import
  if(!is.null(qw.data))
  {
    
    ###############################################
    ###Load all plotting elements of server, 
    ###they can't run until afte rthe data has been imported
    ###############################################
    
    tryCatch({
      
      ###Result-level review tab
      source("server_resultReview.R",local=TRUE)$value
      
      ###Charge balance review
      source("server_cbReview.r",local=TRUE)$value
      
      ###Blanks and reps
      source("server_blankRep.R",local=TRUE)$value
      
      
      # 
      # 
      # source("server_repBox.r",local=TRUE)$value
      # 
      # source("server_blankPlot.r",local=TRUE)$value
      # 
      # #source("server_map.r",local=TRUE)$value
      # 
      # ###Tables###
      # source("server_tables.r",local=TRUE)$value
      
      ###############################
      ###update all inputs with site IDs and parm codes from data pull
      ###############################
      source("server_updateInputs.r",local=TRUE)$value
      
      ###print complete
      output$loadMess <- renderText("Data load complete")
      
    },error=function(e){})
    
    ###Clear error messages on successful data import
    output$errors <- renderPrint("")
    output$shinyErrors <- renderPrint("")
    
    ###Validate against database for changes
    qw.data.current <<- suppressWarnings(readNWISodbc(DSN=pullCriteria$DSN,
                                              env.db = pullCriteria$env.db,
                                              qa.db=pullCriteria$qa.db,
                                              STAIDS=pullCriteria$STAIDS,
                                              dl.parms=pullCriteria$dl.parms,
                                              parm.group.check=pullCriteria$parm.group.check,
                                              begin.date=as.character(pullCriteria$begin.date),
                                              end.date=as.character(pullCriteria$end.date),
                                              projectCd = pullCriteria$projectCd))
    qw.data.current$PlotTable <<- qw.data.current$PlotTable[order(qw.data.current$PlotTable$PARM_SEQ_GRP_CD),]
    
    if(isTRUE(all.equal(qw.data.current$PlotTable,qw.data$PlotTable[1:76],check.attributes = FALSE)))
    {
      output$uptodate <- renderText("Data is current with NWIS database")
    } else {
      output$uptodate <- renderText("Data have been modified in NWIS since being saved")
    }

    
  } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}
  
})





