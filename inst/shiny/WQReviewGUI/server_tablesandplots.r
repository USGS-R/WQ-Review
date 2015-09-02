
#############################################################
###Different functional elements of the dashboard,        ###
###plotting, tables, etc. add items here if desired       ###
#############################################################

###Plotting###



###This does the timeSeries plotting
source("server_timeSeries.r",local=TRUE)$value

source("server_seasonal.r",local=TRUE)$value

source("server_parmBox.r",local=TRUE)$value

source("server_parmParm.r",local=TRUE)$value

source("server_matrix.r",local=TRUE)$value

source("server_cb.r",local=TRUE)$value

source("server_scSum.r",local=TRUE)$value

source("server_repBox.r",local=TRUE)$value

source("server_blankPlot.r",local=TRUE)$value

#source("server_map.r",local=TRUE)$value

###Tables###
source("server_tables_reactive.r",local=TRUE)$value