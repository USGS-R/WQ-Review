#' Auto-generates a QAQC report website in html
#' 
#' Pulls data from internal NWIS server and runs automated data checks, generates tables, and produces
#' an indexed notebook of flagged samples, tables, and plots for all sites entered.
#' @param networkList A dataframe containing network names in 1st column and station IDs in second column. Both columns must be of class character
#' @param DSN A character string containing the DSN for your local server.
#' @param env.db A character string containing the database number of environmental samples.
#' @param qa.db A character string containing the database number of QA samples.
#' @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd). Default is 10 years from current date.
#' @param end.date Character string containing ending date of data pull (yyyy-mm-dd). Default is current date.
#' @param outputDir Directory to store output files. Must be an absolute path, e.g. "D:/flagReports"
#' @importFrom dplyr left_join
#' @importFrom rmarkdown render
#' @details 
#' flagReport uses the readNWISodbc function to pull data for sites in networkList. Sites are grouped by the cooresponding network name
#' in the first column of networkList. Reports for each site are generated and written to subfolders by network name. A top-level index
#' is then generated to navigate between networks. Reports consist of automated data checks defined in  http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check30-sql.html and plots.
#' Panels containing a timeseries, seasonal, parameter vs discharge and parameter vs SC plot are generated and indexed.
#' This function can be used to auto-generate network QAQC reports overnight using R in batch mode with a script and Windows task scheduler.
#' @examples 
#'\dontrun{
#' #This will not run if not connected to NWISCO server
#' networkList <- data.frame(network = c("BigThompson","BigThompson","Gunnison River"),
#'                           site = c("06733000","06741520","09109850"),
#'                           stringsAsFactors=FALSE)
#' flagReport(networkList = networkList,
#'           DSN = "NWISCO",
#'           env.db="01",
#'           qa.db="02",
#'           begin.date = as.character(Sys.Date()-365*10),
#'           end.date = as.character(Sys.Date()),
#'           outputDir = "C:/networkReportExample"
#'           )
#'           }
#' @importFrom gridExtra grid.arrange
#' @importFrom rmarkdown render
#' @importFrom plyr rbind.fill
#' @import ggplot2
#'           
#' @export


flagReport <- function(networkList,
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

        stations <- networkList
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
        
        ###Remove parameters that have not been modified for 3 years
        modParms <- unique(qw.data$PlotTable$PARM_CD[which(qw.data$PlotTable$RESULT_MD > Sys.time() - 60*60*24*365*3)])
        qw.data$PlotTable <- subset(qw.data$PlotTable, PARM_CD %in% modParms)
        
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
                        qw.data$PlotTable <- dplyr::left_join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
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
                        qw.data$PlotTable <- dplyr::left_join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")
                        
                        
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
            try(
            rmarkdown::render(paste(markdownDir,"/networkSummary.rmd",sep=""),params = list(sites = sites, network=network,outputDir = outputDir),
                              output_file="networkSummary.htm",
                              output_dir=paste0(outputDir,"/",networks[i],sep="")
            )
            )
        
            for(k in 1:length(sites))
            {
            site <- subset(stations,network == networks[i])$SITE_NO[k]
            siteName <- unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$SITE_NO == site)])
            try(
            rmarkdown::render(paste(markdownDir,"/flagReport.rmd",sep=""),params = list(site = site,siteName=siteName,outputDir = outputDir),
                          output_file=paste(site,".htm",sep=""),
                          output_dir=paste0(outputDir,"/",networks[i],sep="")
                          )
            )
            
            }
        }
        
}


