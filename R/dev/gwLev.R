# library(RODBC)
# DSN <- "NWISCO"
# #############################################################################
# Chan1 <- RODBC::odbcConnect(DSN)###Start of ODBC connection
# #############################################################################
# 
# ###List all tables
# Query <- "select owner, table_name from all_tables"
# 
# allTables <- RODBC::sqlQuery(Chan1, Query, as.is=T)
# gwLevTables <- filter(allTables, OWNER=="NWISCO")
# gwLevTables <- gwLevTables[grep("GW_LEV",gwLevTables$TABLE_NAME),]
# 
# ###
# query <- "select * from NWISCO.GW_LEV_01 where site_no in ('383513107542601')"
# test <- RODBC::sqlQuery(Chan1, query, as.is=T)
# 
# ###
# query <- "select * from NWISCO.GW_LEV_01 where site_no in ('383513107542601')"
# test <- RODBC::sqlQuery(Chan1, query, as.is=T)
# 
# ###
# query <- "select * from NWISCO.GW_MPNT_01 where site_no in ('383513107542601')"
# test_mvpt <- RODBC::sqlQuery(Chan1, query, as.is=T)
