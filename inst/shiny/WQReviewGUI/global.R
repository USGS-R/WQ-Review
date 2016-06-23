library(WQReview)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(reshape2)
library(ggplot2)

qw.data <- NULL
reports <- NULL

markedRecords <- data.frame(RECORD_NO = NA,
                            SITE_NO = NA,
                            STATION_NM = NA,
                            SAMPLE_START_DT = NA,
                            MEDIUM_CD = NA,
                            PARM_CD = NA,
                            DQI_CD_Current = NA,
                            DQI_CD_New = NA,
                            PARM_NM = NA,
                            Where_Flagged = NA,
                            Status = NA,
                            Comment = NA
                            )


                            
errors <- ""
###Plot global settings
medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ")
## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
qual.shapes <- c(19,0,2,5)
names(qual.shapes) <- c("Sample","<",">","E")