###This contains all the ui elements for the timeseries plot tab.
###It is sourced from the ui-body tab, which is eventually sourced from the ui tab.
###These are contained in individual scripts just for organization sake.


fluidPage(
  pageWithSidebar(
    headerPanel("Data download criteria"),
    sidebarPanel(
      h1("BOTH R DATA AND EXCEL WORKBOOK MUST BE SAVED AND USED TOGETHER \n"),
      downloadButton('saveRData', 'Save imported R data'),
      h1("SAVE YOUR EXCEL WORKBOOK! \n"),
      
      fileInput("loadRDataFile",label="Load saved R data",accept=".rda"),
      fileInput("loadXLDataFile",label="Load saved Excel workbook",accept=".xlsx"),
      
      actionButton(inputId = "loadData",label="Load data")
      

    ),
    mainPanel(
      textOutput("loadWarning"),
      h3(textOutput("loadMess")),
      h3(textOutput("uptodate"))
      


      
    )
  )
)

