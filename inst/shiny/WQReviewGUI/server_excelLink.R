# #Populate excel sheets
xl.workbook.add()

xl.sheet.add("Unnaproved data")
xlc$a1 = reports$unaprovedData

xl.sheet.add("Reviewed unnaproved data")
xlc$a1 = data.frame(RECORD_NO = NA,
                    SITE_NO = NA,
                    STATION_NM = NA,
                    SAMPLE_START_DT = NA,
                    MEDIUM_CD = NA,
                    PARM_CD = NA,
                    PARM_NM = NA,
                    DQI_CD_CURRENT = NA,
                    PARM_SEQ_GRP_CD = NA,
                    FLAGGED = NA,
                    DQI_CD_REV = NA,
                    RESULT_CM_NWIS_REV = NA,
                    RESULT_CM_TYPE_NWIS_REV = NA,
                    RESULT_NOTE_NO_NWIS = NA
)

# xl.sheet.add("Summary of flags")
# xlc$a1 = reports$flaggedData

xl.sheet.add("Chemical sense")
xlc$a1 = reports$chemFlagTable

xl.sheet.add("Charge balance issues")
xlc$a1 = reports$badCBTable

xl.sheet.add("Whole vs Part")
xlc$a1 = reports$wholevpartTable

xl.sheet.add("Pesticide flags")
xlc$a1 = reports$pestFlagTable

xl.sheet.add("Replicate table")
xlc$a1 = reports$repTable

xl.sheet.add("Blank table")
xlc$a1 = reports$blankTable_all

xl.sheet.add("All charge balance")
xlc$a1 = reports$BalanceDataTable

xl.sheet.add("Data by result")
xlc$a1 = qw.data$PlotTable

xl.sheet.add("Data by sample")
xlc$a1 = qw.data$DataTable

xl.sheet.delete("sheet1")
xl.sheet.delete("sheet2")
xl.sheet.delete("sheet3")

xl.sheet.activate("Charge balance issues")
xl_CB <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)

xl.sheet.activate("Reviewed unnaproved data")
xl_DQI <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)

xl.sheet.activate("Unnaproved data")
xl_UNAP <<- xl.connect.table("a1",row.names = FALSE, col.names = TRUE)