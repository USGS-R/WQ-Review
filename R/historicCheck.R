#' Function to flag samples if values are out of range of historic data
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Compares each sample with DQI code of "I","S", or "P" to ranges of all prior approved data ("R","O","A"),
#' and flags samples that are suspisciously high or low. 
#' Definitions of checks can be found at http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check30-sql.html
#' @examples 
#' data("exampleData",package="WQReview")
#' historicCheckOut <- historicCheck(qw.data=qw.data,
#'              returnAll = FALSE)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join

#' @export
#' @return A dataframe containing all samples with applicable flags

historicCheck <- function(qw.data, returnAll = FALSE)
{
        #subset to in review and approved data, removing information pcodes
        inReviewData <- qw.data$PlotTable[is.finite(qw.data$PlotTable$RESULT_VA) &
                                                  qw.data$PlotTable$DQI_CD %in% c("I","S","P") &
                                                  qw.data$PlotTable$PARM_SEQ_GRP_CD != "INF" &
                                                  qw.data$PlotTable$MEDIUM_CD %in% c("WS ","WG ", "OA "),]
        approvedData <- qw.data$PlotTable[is.finite(qw.data$PlotTable$RESULT_VA) &
                                                  qw.data$PlotTable$DQI_CD %in% c("R","O","A") &
                                                  qw.data$PlotTable$PARM_SEQ_GRP_CD != "INF" &
                                                  qw.data$PlotTable$MEDIUM_CD %in% c("WS ","WG ", "OA "),]
        
        if(nrow(inReviewData) > 0)
        {
                #Get stats by parm for each site
                siteStats <- dplyr::summarize(group_by(approvedData,SITE_NO,PARM_CD),
                                              min = min(RESULT_VA[REMARK_CD != "<"],na.rm=TRUE),
                                              max = max(RESULT_VA[REMARK_CD != ">"],na.rm=TRUE),
                                              percNonDetects = length(na.omit(RESULT_VA[REMARK_CD == "<"]))/length(na.omit(RESULT_VA))*100,
                                              quant99 = quantile(RESULT_VA[REMARK_CD != ">"],probs=(0.99),na.rm=TRUE),
                                              quant01 = quantile(RESULT_VA[REMARK_CD != "<"],probs=(0.01),na.rm=TRUE),
                                              N = length(RESULT_VA)
                )
                inReviewData <- dplyr::left_join(inReviewData,siteStats, by = c("SITE_NO","PARM_CD"))
                
                ##Check if new max
                inReviewData$newMax_30.11 <- NA
                inReviewData$newMax_30.11[is.finite(inReviewData$max) &
                                                  inReviewData$REMARK_CD != ">" & 
                                                  inReviewData$RESULT_VA > inReviewData$max &
                                                  inReviewData$N > 4] <-
                        paste0("newMax_",
                               inReviewData$RESULT_VA[is.finite(inReviewData$max) &
                                                              inReviewData$REMARK_CD != ">" &
                                                              inReviewData$RESULT_VA > inReviewData$max &
                                                              inReviewData$N > 4]
                        )
                
                ##Check if new min
                inReviewData$newMin_30.12 <- NA
                inReviewData$newMin_30.12[is.finite(inReviewData$min) &
                                                  inReviewData$REMARK_CD != "<" & 
                                                  inReviewData$RESULT_VA < inReviewData$min &
                                                  inReviewData$N > 4] <-
                        paste("newMin_",
                              inReviewData$RESULT_VA[is.finite(inReviewData$min) &
                                                             inReviewData$REMARK_CD != "<" & 
                                                             inReviewData$RESULT_VA < inReviewData$min &
                                                             inReviewData$N > 4]
                        )  
                
                ##Check if greater than 99th percentile
                inReviewData$greaterQuant99_30.15 <- NA
                inReviewData$greaterQuant99_30.15[is.finite(inReviewData$quant99) &
                                                          inReviewData$REMARK_CD != ">" & 
                                                          inReviewData$RESULT_VA > inReviewData$quant99 &
                                                          inReviewData$N > 4] <-
                        paste(">99perc_",
                              inReviewData$RESULT_VA[is.finite(inReviewData$quant99) &
                                                             inReviewData$REMARK_CD != ">" &
                                                             inReviewData$RESULT_VA > inReviewData$quant99 &
                                                             inReviewData$N > 4]
                        )
                
                ##Check if less than 1th percentile
                inReviewData$lessQuant01_30.16 <- NA
                inReviewData$lessQuant01_30.16[is.finite(inReviewData$quant01) &
                                                       inReviewData$REMARK_CD != "<" & 
                                                       inReviewData$RESULT_VA < inReviewData$quant01 &
                                                       inReviewData$N > 4] <-
                        paste("<1perc_",
                              inReviewData$RESULT_VA[is.finite(inReviewData$quant01) &
                                                             inReviewData$REMARK_CD != "<" &
                                                             inReviewData$RESULT_VA < inReviewData$quant01 &
                                                             inReviewData$N > 4]
                        )
                
                ##Check if unusual non detect
                inReviewData$unusualNonDetect <- NA
                inReviewData$unusualNonDetect[is.finite(inReviewData$percNonDetects) &
                                                      inReviewData$REMARK_CD == "<" &
                                                      inReviewData$percNonDetects < 5 &
                                                      inReviewData$N > 4] <- "nonDetect_"
                
                #Extract site info and flags
                flaggedSamples <- inReviewData[c("RECORD_NO",
                                                 "SITE_NO",
                                                 "STATION_NM",
                                                 "SAMPLE_START_DT",
                                                 "SAMPLE_END_DT",
                                                 "MEDIUM_CD",
                                                 "PARM_CD",
                                                 "PARM_NM",
                                                 "DQI_CD",
                                                 "RESULT_VA",
                                                 "REMARK_CD",
                                                 "PARM_SEQ_GRP_CD",
                                                 "min",
                                                 "max",
                                                 "quant99",
                                                 "quant01",
                                                 "newMax_30.11",
                                                 "newMin_30.12",
                                                 "greaterQuant99_30.15",
                                                 "lessQuant01_30.16",
                                                 "unusualNonDetect"
                )]
                if(returnAll == FALSE)
                {
                        #remove NAs from result flags
                        flaggedSamples <- unique(flaggedSamples[which(!is.na(flaggedSamples[17]) |
                                                                              !is.na(flaggedSamples[18]) |
                                                                              !is.na(flaggedSamples[19]) |
                                                                              !is.na(flaggedSamples[20]) |
                                                                                             !is.na(flaggedSamples[21])
                                                                              ),]) 
                } else {}
                
                return(unique(flaggedSamples))
                
        } else{ print("No in-review data for comparison to approved data")
                return(data.frame(RECORD_NO = NA,
                                  SITE_NO = NA,
                                  STATION_NM = NA,
                                  SAMPLE_START_DT = NA,
                                  SAMPLE_END_DT = NA,
                                  MEDIUM_CD = NA,
                                  PARM_CD = NA,
                                  PARM_NM = NA,
                                  DQI_CD = NA,
                                  PARM_SEQ_GRP_CD = NA,
                                  min = NA,
                                  max = NA,
                                  quant99 = NA,
                                  quant01 = NA,
                                  newMax_30.11 = NA,
                                  newMin_30.12 = NA,
                                  greaterQuant99_30.15 = NA,
                                  lessQuant01_30.16 = NA)
                )
        }
}