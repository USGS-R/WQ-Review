dashboardBody(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tabItems(
                
        source("ui_body_siteSelection.r",local=TRUE)$value,
        #source("ui_body_nwisDCImport.r",local=TRUE)$value,
        
        ################################################################
        ###Different functional elements of the body of the dashboard###
        ###plotting, tables, etc. add items here if desired          ###
        ################################################################
        
        source("ui_body_plots_timeseries.r",local=TRUE)$value,
        
        source("ui_body_plots_seasonal.r",local=TRUE)$value,
        
        source("ui_body_plots_parmBox.r",local=TRUE)$value,
        
        source("ui_body_plots_parmParm.r",local=TRUE)$value,
        
        source("ui_body_plots_matrix.r",local=TRUE)$value,
        
        source("ui_body_plots_cb.r",local=TRUE)$value,
        
        source("ui_body_plots_scSum.r",local=TRUE)$value,
        
        source("ui_body_plots_repBox.r",local=TRUE)$value,
        
        source("ui_body_plots_blank.r",local=TRUE)$value,
        
        #source("ui_body_plots_map.r",local=TRUE)$value,
        
        source("ui_body_tables_wideData.r",local=TRUE)$value,
        source("ui_body_tables_longData.r",local=TRUE)$value,
        source("ui_body_tables_cb.r",local=TRUE)$value,
        source("ui_body_tables_blank.r",local=TRUE)$value,
        source("ui_body_tables_rep.r",local=TRUE)$value,
        source("ui_body_tables_wholevpart.r",local=TRUE)$value,
        #source("ui_body_flagPage.r",local=TRUE)$value,
        source("ui_body_tables_chemFlagTable.r",local=TRUE)$value,
        source("ui_body_tables_pestFlagTable.r",local=TRUE)$value,
        source("ui_body_tables_resultFlagTable.r",local=TRUE)$value,
        source("ui_body_tables_dqiTable.r",local=TRUE)$value,
        
        
        #source("ui_body_srsSummary.r",local=TRUE)$value,
        
        source("ui_body_dataUpload.r",local=TRUE)$value,
        source("ui_body_saveLoad.r",local=TRUE)$value,
        source("ui_body_markedRecords.r",local=TRUE)$value
        
        
)
)
                        

