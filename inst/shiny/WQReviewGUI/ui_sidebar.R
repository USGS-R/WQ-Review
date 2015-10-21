###This constructs the sidebar###
###Source this script in the main ui.r

dashboardSidebar(sidebarMenu(
        
                

                        
                       
                
                       ###Load the controls
        menuItem(tabName="siteSelection","Data import"),
        menuItem(tabName="flagPage","Auto-flagged samples"),
        
                menuItem("Plots",
                         
                         dateInput("newThreshold", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                         #textInput("recordSelect", "Highlight record #"),
                         
                           
                         menuSubItem(tabName="timeSeries",text="Timeseries"),
                         
                         menuSubItem(tabName="seasonalPlot",text="Seasonal"),
                         
                         menuSubItem(tabName="parmBoxPlot",text="Parameter boxplot"),
                         
                         menuSubItem(tabName="parmParmPlot",text="Parameter vs. parameter plot"),
                         
                         menuSubItem(tabName="matrixPlot",text="Matrix plot"),
                         
                         menuSubItem(tabName="cbPlot",text="Chargebalance plot"),
                         
                         menuSubItem(tabName="scSumPlot",text="Ions vs conductance plot"),
                         
                         menuSubItem(tabName="repBoxPlot",text="Replicate boxplot"),
                         
                         menuSubItem(tabName="blankPlot",text="Blank timeseries"),
                         
                         #menuSubItem(tabName="mapPlot",text="Spatial plot"),
                         
                                                   
                         ###Other arguments to top menu item
                          tabName = "Plots", 
                          icon = icon("bar-chart"),selected=FALSE
                         ),
                 ###Next sidebar entry
                 menuItem("Tables", 
                          ###Table types, each one will appear in dashboard
                          menuSubItem(tabName="wideDataTable","Wide data table"),
                          menuSubItem(tabName="longDataTable","Long data table"),
                          menuSubItem(tabName="balanceTable","Charge balance table"),
                          menuSubItem(tabName="repTable","Replicate table"),
                          menuSubItem(tabName="blankTable","Blank table"),
                          menuSubItem(tabName="wholvevpartTable","Fil vs. Unf table"),
                          ###Other arguments ot top menu item                          
                          tabName = "Tables", icon = icon("table")
                          ),
                
        #menuSubItem(tabName="dataUpload","Data upload tools"),
        #menuSubItem(tabName="srsSummary","SRS Summary tool"),
        menuItem(tabName="saveLoadTab","Save/load data")
        )
)