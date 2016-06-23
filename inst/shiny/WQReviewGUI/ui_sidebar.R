###This constructs the sidebar###
###Source this script in the main ui.r

dashboardSidebar(sidebarMenu(
        
        
        
        
        
        
        ###Load the controls
        menuItem("Data import",
                 menuSubItem(tabName="siteSelection", text = "Import sites and samples"),
                 menuSubItem(tabName="nwisDCImport", text = "NWIS Datachecks import")
        ),
        
        menuItem("Chemical sense checks",icon = icon("table"),
                 menuSubItem(tabName="chemFlagTable","Chemical sense flags",icon = icon("table")),
                 menuSubItem(tabName="balanceTable","Charge balance table",icon = icon("table")),
                 menuSubItem(tabName="cbPlot",text="Chargebalance plot",icon = icon("bar-chart")),
                 menuSubItem(tabName="scSumPlot",text="Ions vs conductance plot",icon = icon("bar-chart"))
                 
        ),
        
        menuItem("Pesticide checks",icon = icon("table"),
                 menuSubItem(tabName="pestFlagTable","Pesticide flags",icon = icon("table"))
        ),
        
        menuItem("Bias analysis",icon = icon("table"),
                 menuItem("Blanks",
                          menuSubItem(tabName="blankTable","Blank summary",icon = icon("table")),
                          menuSubItem(tabName="parmBoxPlot",text="Parameter boxplot",icon = icon("bar-chart")),
                          menuSubItem(tabName="blankPlot",text="Blank timeseries",icon = icon("bar-chart"))
                 ),
                 
                 menuItem("Replicates",
                          menuSubItem(tabName="repTable","Replicate table",icon = icon("table")),
                          menuSubItem(tabName="repBoxPlot",text="Replicate boxplot",icon = icon("bar-chart"))
                 ),
                 
                 menuItem("Fil. vs Unf.",
                          menuSubItem(tabName="wholvevpartTable","Fil vs. Unf table",icon = icon("table"))
                 )
        ),
        
        menuItem("Result-level checks",icon = icon("table"),
                 menuSubItem(tabName="resultFlagTable","Result-level flags",icon = icon("table")),
                 menuItem("Plots",
                             
                             dateInput("newThreshold", "New samples threshold",max=Sys.Date(),value=Sys.Date()-30),
                             #textInput("recordSelect", "Highlight record #"),
                             
                             
                             menuSubItem(tabName="timeSeries",text="Timeseries"),
                             
                             menuSubItem(tabName="seasonalPlot",text="Seasonal"),
                             
                             menuSubItem(tabName="parmBoxPlot",text="Parameter boxplot"),
                             
                             menuSubItem(tabName="parmParmPlot",text="Param vs. Param plot"),
                             
                             menuSubItem(tabName="matrixPlot",text="Matrix plot"),
                             
                             
                             
                             
                             
                             
                             
                             #menuSubItem(tabName="mapPlot",text="Spatial plot"),
                             
                             
                             ###Other arguments to top menu item
                             tabName = "Plots", 
                             icon = icon("bar-chart"),selected=FALSE
                 )
                 
        ),
        



menuItem("Data Tables",
         menuSubItem(tabName="wideDataTable","Wide data table"),
         menuSubItem(tabName="longDataTable","Long data table"),
         tabName = "dataTables", icon = icon("table")
),

#menuSubItem(tabName="dataUpload","Data upload tools"),
#menuSubItem(tabName="srsSummary","SRS Summary tool"),
menuItem(tabName="saveLoadTab","Save/load data"),
menuItem(tabName="helpTab","Help",
         menuSubItem(href="FAQ.htm",text="FAQ"),
         menuSubItem(href="WQReviewGUI.html",text="User Guide")
),

h3("Review comments"),
menuItem(tabName="markedRecordsTab","View table"),
textInput("sidebar_flaggedRecord",label="Record #"),
textInput("parmSel_sidebar",label="PCODE"),
radioButtons("sidebar_flaggedStatus",choices=c("No selection","Looks good","Not OK"),label="Status"),
selectInput("sidebar_dqiCode",choices = c(NA,"R","Q","I","S","O","X","U","A","P"),label="DQI Code",multiple=FALSE),
textInput("sidebar_flaggedComment",label = "Comment"),
actionButton(inputId = "sidebar_addRecord",label="Add record")


###Can't get this to look nice in sidebar yet, tabled for now
#DT::dataTableOutput("needsReviewTable")

)
)