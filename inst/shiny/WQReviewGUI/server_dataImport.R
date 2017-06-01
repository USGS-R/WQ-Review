observeEvent(input$dataDownload, {
  validate(
    need(!is.null(input$STAIDS) | !is.null(input$siteFile), "Please enter a site number or a site ID file"),
    need(!is.null(input$dl.parms) | !is.null(input$parmFile) | !is.null(input$dl.parms.group), "Please enter a parameter group(s) or a parameter code file"),
    need(input$DSN != "", "Please enter your DSN name for your NWIS server")
  )
  qw.data <<- NULL
  reports <<- NULL
  errors <<- ""
  
  pullCriteria <<- list()
  
  siteFile <- input$siteFile
  parmFile <- input$parmFile
  
  
  tryCatch({
    
    ##############Get all data download inputs
    
    ###Check for parm group input or pcode file
    if(!(is.null(parmFile)))
    {
      pullCriteria$parm.group.check <<- FALSE
      pullCriteria$dl.parms <<- scan(parmFile$datapath,what="character")
    } else if (!is.null(input$dl.parms)) {
      pullCriteria$parm.group.check <<- FALSE
      pullCriteria$dl.parms <<- as.character(input$dl.parms)
    } else {pullCriteria$parm.group.check <<- TRUE
    pullCriteria$dl.parms <<- input$dl.parms.group
    }
    
    ###Check for single site id input or site file
    if(!(is.null(siteFile)))
    {
      pullCriteria$STAIDS<<-scan(siteFile$datapath,what="character")
    } else(pullCriteria$STAIDS<<-input$STAIDS)
    
    pullCriteria$DSN <<- input$DSN
    pullCriteria$env.db <<- input$env.db
    pullCriteria$qa.db <<- input$qa.db
    pullCriteria$begin.date <<- input$begin.date
    pullCriteria$end.date <<- input$end.date
    pullCriteria$projectCd <<- input$projectCd
    ##################################################
    
    ###Run the data import and generate all reports
    withProgress(message="Data import",detail="Pulling data from NWIS",value=0,{
      
      tryCatch({
        
        
        qw.data <<- suppressWarnings(readNWISodbc(DSN=pullCriteria$DSN,
                                                  env.db = pullCriteria$env.db,
                                                  qa.db=pullCriteria$qa.db,
                                                  STAIDS=pullCriteria$STAIDS,
                                                  dl.parms=pullCriteria$dl.parms,
                                                  parm.group.check=pullCriteria$parm.group.check,
                                                  begin.date=as.character(pullCriteria$begin.date),
                                                  end.date=as.character(pullCriteria$end.date),
                                                  projectCd = pullCriteria$projectCd))
        qw.data$PlotTable <<- qw.data$PlotTable[order(qw.data$PlotTable$PARM_SEQ_GRP_CD),]
        
        
      },error = function(e){
        output$errors <- renderPrint(geterrmessage())
        stop(geterrmessage())
      })
      
      incProgress(0.3,detail="Autoflagging")
      
      source("server_reports.r",local=TRUE)$value
      

      ###Populate excel tables
      source("server_excelLink.r",local=TRUE)$value
    })
      ##################################################
      
      ###Populate data pull info
      if(!is.null(qw.data))
      {
        
        ###Update header
        #source("server_header.r",local=TRUE)$value
        
        ###Print retrieval info
        
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
        
        ###Blank reps
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
        
        ###Load the sidebars
        
        
      },error=function(e){})
      
      ###Clear error messages on successful data import
      output$errors <- renderPrint("")
      output$shinyErrors <- renderPrint("")
      
    } else{output$shinyErrors <- renderPrint("Problem with data import, please check import criteria")}
    
  })
  