#' flagReport
#' 
#' Pulls data from internal NWIS server and runs automated data checks, generates tables, and produces
#' an indexed notebook of flagged samples, tables, and plots for all sites entered.
#' @param stationFile Filename of csv file containing network names in 1st column and station IDs in second column.
#' @param DSN A character string containing the DSN for your local server.
#' @param env.db A character string containing the database number of environmental samples.
#' @param qa.db A character string containing the database number of QA samples.
#' @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd). Default is 10 years from current date.
#' @param end.date Character string containing ending date of data pull (yyyy-mm-dd). Default is current date.
#' @param outputDir Directory to store output files. Must be an absolute path, e.g. "D:/flagReports"
#' @export


flagReport <- function(stationFile,
                       DSN,
                       env.db = "01",
                       qa.db = "02",
                       begin.date = as.character(Sys.Date()-365*10),
                       end.date = as.character(Sys.Date()),
                       outputDir)
        {
        
        markdownDir <- system.file("markdown", package = "WQReview")
        if (markdownDir == "") {
                stop("Could not find GUI directory. Try re-installing `WQReview`.", call. = FALSE)
        }

        stations <- read.csv(stationFile,header=FALSE,colClasses = "character")
        colnames(stations) <- c("network","SITE_NO")
        
        ###Make directory to hold output
        #outputDir <- paste(getwd(),"/",outputDir,sep="")
        dir.create(outputDir)
        
        ##Make subdirectories for each network
        networks <- unique(stations$network)
        for(i in 1:length(networks))
        {
                dir.create(paste0(outputDir,"/",networks[i],sep=""))
        }
        
        ########################################################################
        ########################################################################
        
        ###Pull data and generate all reports
        
        ###Pull data
        qw.data <- suppressWarnings(readNWISodbc(DSN=DSN,
                                                 env.db = env.db,
                                                 qa.db=qa.db,
                                                 STAIDS=stations$SITE_NO,
                                                 begin.date=begin.date,
                                                 end.date=end.date)
        )
        
        ###Run report generation on qw.data 
        reports <- NULL
        
        ######################################
        #######Run report generation##########
        ######################################
        
        tryCatch({
                ###Run ion balance function
                ionBalanceOut <- suppressWarnings(ionBalance(qw.data = qw.data,wide=TRUE))
                chargebalance.table <- ionBalanceOut$chargebalance.table
                reports$BalanceDataTable <- ionBalanceOut$BalanceDataTable
                reports$balanceTable <- ionBalanceOut$chargebalance.table
                
                ###Check that a balance was calculated
                if(nrow(chargebalance.table) > 0)
                {
                        
                        ###Join charge balance table to plot table
                        chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
                        qw.data$PlotTable <- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
                } else {}
                
        }, warning = function(w) {
                ###Run ion balance function
                ionBalanceOut <- ionBalance(qw.data = qw.data,wide=TRUE)
                chargebalance.table <- ionBalanceOut$chargebalance.table
                reports$BalanceDataTable <- ionBalanceOut$BalanceDataTable
                reports$balanceTable <- ionBalanceOut$chargebalance.table
                
                ###Check that a balance was calculated
                if(nrow(chargebalance.table) > 0)
                {
                        
                        ###Join charge balance table to plot table
                        chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
                        qw.data$PlotTable <- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
                } else {}
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                errors <- print("Error with charge balance. please report this on the github issues page")
                
        })
        
        tryCatch({
                
                ###Run repTabler
                reports$repTable <- suppressWarnings(repTabler(qw.data))
                
        }, warning = function(w) {
                
                ###Run repTabler
                reports$repTable <- repTabler(qw.data)
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                errors <- print("Error with replicate table. please report this on the github issues page")
        })
        
        
        tryCatch({
                ###Run wholevPart
                reports$wholevpartTable <- suppressWarnings(wholevPart(qw.data))
                
        }, warning = function(w) {
                ###Run wholevPart
                reports$wholevpartTable <- wholevPart(qw.data)
                warning(w,"This caused a warning DO NOT REPORT UNLESS YOU NOTICE A PROBLEM WITH PERFORMANCE")
        }, error = function(e) {
                errors <- print("Error with whole vs part table. please report this on the github issues page")
                
        })
        
        
        ###Generate chem flag summary
        tryCatch({
                reports$chemFlagTable <- suppressWarnings(chemCheck(qw.data))
        },
        warning = function(w) {
                reports$chemFlagTable <- suppressWarnings(chemCheck(qw.data))
        },
        error = function(e) {
                errors <- print("Error with auto sample flagging, please report this on the github issues page")
        })
        
        ###Generate chem flag summary
        tryCatch({
                reports$pestFlagTable <- suppressWarnings(pestCheck(qw.data))
        },
        warning = function(w) {
                reports$pestFlagTable <- suppressWarnings(pestCheck(qw.data))
        },
        error = function(e) {
                errors <- print("Error with auto sample flagging, please report this on the github issues page")
        })
        
        
        
        ###Generate result flags
        tryCatch({
                reports$resultFlagTable <- suppressWarnings(historicCheck(qw.data,returnAll=FALSE))
                ###Need to reset row names
                rownames(reports$resultFlagTable) <- seq(from = 1, to = nrow(reports$resultFlagTable))
        },
        warning = function(w) {
                
                reports$resultFlagTable <- suppressWarnings(historicCheck(qw.data,returnAll=FALSE)) 
                ###Need to reset row names
                rownames(reports$resultFlagTable) <- seq(from = 1, to = nrow(reports$resultFlagTable))
        },
        error = function(e) {
                print("Error with auto result flagging, please report this on the github issues page")
        })
        
        ########################################################################
        ########################################################################
        
        ###Save data file for use in markdown
        save(list=c("qw.data","reports"),file=paste0(outputDir,"/data.rda",sep=""))
        
        ###Remove stations with no data from station list
        stations <- subset(stations, SITE_NO %in% unique(qw.data$PlotTable$SITE_NO))
        
        ###Run markdown fto make index.htm
        rmarkdown::render(paste(markdownDir,"/networkIndex.rmd",sep=""),params = list(networks=networks,outputDir = outputDir),
                          output_file="networkIndex.htm",
                          output_dir=(outputDir)
        )
        
        
        ###Run markdown for each site and create seperate html file

        for(i in 1:length(networks))
            {
            
            sites <- subset(stations,network == networks[i])$SITE_NO
            network <- networks[i]
            
            rmarkdown::render(paste(markdownDir,"/networkSummary.rmd",sep=""),params = list(sites = sites, network=network,outputDir = outputDir),
                              output_file="networkSummary.htm",
                              output_dir=paste0(outputDir,"/",networks[i],sep="")
            )
        
            for(k in 1:length(sites))
            {
            site <- subset(stations,network == networks[i])$SITE_NO[k]
            siteName <- unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$SITE_NO == site)])
            rmarkdown::render(paste(markdownDir,"/flagReport.rmd",sep=""),params = list(site = site,siteName=siteName,outputDir = outputDir),
                          output_file=paste(site,".htm",sep=""),
                          output_dir=paste0(outputDir,"/",networks[i],sep="")
                          )
            
            }
        }
        
}


