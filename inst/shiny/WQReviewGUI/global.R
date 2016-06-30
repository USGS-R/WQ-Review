library(WQReview)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(reshape2)
library(ggplot2)

qw.data <- NULL
reports <- NULL

markedRecords <- data.frame(RECORD_NO = character(),
                            SITE_NO = character(),
                            STATION_NM = character(),
                            SAMPLE_START_DT = character(),
                            MEDIUM_CD = character(),
                            PARM_CD = character(),
                            DQI_CD_Current = character(),
                            DQI_CD_New = character(),
                            PARM_NM = character(),
                            Status = character(),
                            Comment = character()
                            )


                            
errors <- ""
###Plot global settings
medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ")
## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
qual.shapes <- c(19,0,2,5)
names(qual.shapes) <- c("Sample","<",">","E")