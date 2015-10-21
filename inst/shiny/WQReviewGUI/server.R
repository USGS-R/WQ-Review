server <- function(input, output, session) {
        
        #############################################################
        ###Global internal functionality such as plot hovers, etc.###
        ###Not for additions!                                     ###
        #############################################################
        #source("server_dataupload.r",local=TRUE)$value
        source("server_saveLoad.r",local=TRUE)$value
        
        #source("server_srsSummary.r",local=TRUE)$value
        
        source("server_dataImport.r",local=TRUE)$value
        
        ###This generates reactive tables
        #############################################################
        ###Different functional elements of the dashboard,        ###
        ###plotting, tables, etc. add items here if desired       ###
        #############################################################
        source("server_dataUpload.r",local=TRUE)$value
        
        session$onSessionEnded(function() {
                stopApp()
        })
        

        
}

