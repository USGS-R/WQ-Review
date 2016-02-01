###Run readNWISodbc to get data

withProgress(message="Data import",detail="Pulling data from NWIS",value=0,{
        
        tryCatch({
        qw.data <<- suppressWarnings(readNWISodbc(DSN=input$DSN,
                                 env.db = input$env.db,
                                 qa.db=input$qa.db,
                                 STAIDS=STAIDS,
                                 dl.parms=dl.parms,
                                 parm.group.check=parm.group.check,
                                 begin.date=as.character(input$begin.date),
                                 end.date=as.character(input$end.date),
                                 projectCd = input$projectCd)
        )
        },warning = function(w){        qw.data <<- suppressWarnings(readNWISodbc(DSN=input$DSN,
                                                                                  env.db = input$env.db,
                                                                                  qa.db=input$qa.db,
                                                                                  STAIDS=STAIDS,
                                                                                  dl.parms=dl.parms,
                                                                                  parm.group.check=parm.group.check,
                                                                                  begin.date=as.character(input$begin.date),
                                                                                  end.date=as.character(input$end.date),
                                                                                  projectCd = input$projectCd
                                                                                  )
                                                                     )
        },error = function(e){
                output$errors <- renderPrint(geterrmessage())
                stop(geterrmessage())
        })
        
        incProgress(0.3,detail="Calculating charge balance")
        
        ######################################
        #######Run report generation##########
        ######################################
        
        tryCatch({
                ###Run ion balance function
                ionBalanceOut <- suppressWarnings(ionBalance(qw.data = qw.data,wide=TRUE))
                chargebalance.table <- ionBalanceOut$chargebalance.table
                reports$BalanceDataTable <<- ionBalanceOut$BalanceDataTable
                reports$balanceTable <<- ionBalanceOut$chargebalance.table
                
                ###Check that a balance was calculated
                if(nrow(chargebalance.table) > 0)
                {
                        
                        ###Join charge balance table to plot table
                        chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
                        qw.data$PlotTable <<- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
                } else {}
                
        }, warning = function(w) {
                ###Run ion balance function
                ionBalanceOut <- ionBalance(qw.data = qw.data,wide=TRUE)
                chargebalance.table <- ionBalanceOut$chargebalance.table
                reports$BalanceDataTable <<- ionBalanceOut$BalanceDataTable
                reports$balanceTable <<- ionBalanceOut$chargebalance.table
                
                ###Check that a balance was calculated
                if(nrow(chargebalance.table) > 0)
                {
                        
                        ###Join charge balance table to plot table
                        chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
                        qw.data$PlotTable <<- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
                } else {}
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                output$errors <- renderPrint("Error with charge balance. please report this on the github issues page")
                        
        })
        
        incProgress(0.5,detail="Generating replicate report")
        
        tryCatch({
                
                ###Run repTabler
                reports$repTable <<- suppressWarnings(repTabler(qw.data))
        
        }, warning = function(w) {
                
                ###Run repTabler
                reports$repTable <<- repTabler(qw.data)
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                output$errors <- renderPrint("Error with replicate table. please report this on the github issues page")
        })
        
        incProgress(0.75,detail="Generating filtered/unfiltered report")
        
        tryCatch({
                ###Run wholevPart
                reports$wholevpartTable <<- suppressWarnings(wholevPart(qw.data))
                
        }, warning = function(w) {
                ###Run wholevPart
                reports$wholevpartTable <<- wholevPart(qw.data)
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                output$errors <- renderPrint("Error with whole vs part table. please report this on the github issues page")
                
        })
        
        incProgress(0.9,detail="Auto flagging")
        
        ###Generate chem flag summary
        tryCatch({
                reports$chemFlagTable <<- suppressWarnings(chemCheck(qw.data))
        },
        warning = function(w) {
                reports$chemFlagTable <<- suppressWarnings(chemCheck(qw.data))
        },
        error = function(e) {
                output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
        })
        
        ###Generate chem flag summary
        tryCatch({
                reports$pestFlagTable <<- suppressWarnings(pestCheck(qw.data))
        },
        warning = function(w) {
                reports$pestFlagTable <<- suppressWarnings(pestCheck(qw.data))
        },
        error = function(e) {
                output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
        })
        
        ###Generate sample flag summary
        tryCatch({
                reports$sampleFlagTable <<- suppressWarnings(flagSummary(qw.data))
                ###Need to reset row names
                rownames(reports$sampleFlagTable) <<- seq(from = 1, to = nrow(reports$sampleFlagTable))
                },
                warning = function(w) {
                        reports$sampleFlagTable <<- suppressWarnings(flagSummary(qw.data))
                        ###Need to reset row names
                        rownames(reports$sampleFlagTable) <<- seq(from = 1, to = nrow(reports$sampleFlagTable))
                        
                        },
                error = function(e) {
                        output$errors <- renderPrint("Error with auto sample flagging, please report this on the github issues page")
                        })
        
        ###Generate result flags
        tryCatch({
                reports$resultFlagTable <<- suppressWarnings(historicCheck(qw.data,returnAll=FALSE))
                ###Need to reset row names
                rownames(reports$resultFlagTable) <<- seq(from = 1, to = nrow(reports$resultFlagTable))
                },
                warning = function(w) {
                        
                        reports$resultFlagTable <<- suppressWarnings(historicCheck(qw.data,returnAll=FALSE)) 
                        ###Need to reset row names
                        rownames(reports$resultFlagTable) <<- seq(from = 1, to = nrow(reports$resultFlagTable))
                        },
                error = function(e) {
                        output$errors <- renderPrint("Error with auto result flagging, please report this on the github issues page")
                })
                                
                        
        #tryCatch({       
        ###Run blnk table summary
        #        reports$blankTable_all <<- suppressWarnings(blankSummary(qw.data, STAIDS = unique(qw.data$PlotTable$SITE_NO), multiple = FALSE))
                
                
                
        #}, warning = function(w) {
                ###Run blnk table summary
        #        reports$blankTable_all <<- blankSummary(qw.data, multiple = FALSE)
        #        warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        #}, error = function(e) {
                                
        #        output$errors <- renderPrint("Error with blank table summary. Please check your data import criteria and QWToolbox manual and if not user error report this whole message on the github issues page and generate a bug report using the button at the top of QWToolbox")
                
        #})
})