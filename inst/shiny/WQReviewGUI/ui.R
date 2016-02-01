
###Replace this with the library once package is revised to supply fuctions that work well with Shiny
#source("functionLoads.R",local=TRUE)$value

library(WQReview)

ui <- 
        #library(shinyBS)
        #library(QWToolbox)

        
        dashboardPage(skin="green",
        ###Load the header
        source("ui_header.r",local=TRUE)$value,
        ##Load the sidebar
        source("ui_sidebar.r",local=TRUE)$value,
        ##Load the body
        source("ui_body.r",local=TRUE)$value
        
        
)
                   
