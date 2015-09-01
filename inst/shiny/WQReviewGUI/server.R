server <- function(input, output, session) {
        
        #############################################################
        ###Global internal functionality such as plot hovers, etc.###
        ###Not for additions!                                     ###
        #############################################################
        source("server_dataupload.r",local=TRUE)$value
        source("server_saveLoad.r",local=TRUE)$value
        
        source("server_srsSummary.r",local=TRUE)$value
        
        source("server_dataImport.r",local=TRUE)$value
        
        ###This generates reactive tables
        source("server_tables_reactive.r",local=TRUE)$value
        #############################################################
        ###Different functional elements of the dashboard,        ###
        ###plotting, tables, etc. add items here if desired       ###
        #############################################################
        
        session$onSessionEnded(function() {
                stopApp()
        })
        

        
}

