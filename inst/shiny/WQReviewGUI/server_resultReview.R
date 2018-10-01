###################################
###This contains all plotting and sidebar server-side
###elements for the result-level review tab
###################################

####################
###Sidebar
####################

###This creates a reactive data frame to update off the excel unnaproved data tab
UNAPdata <<- eventReactive(input$refresh, {
  tryCatch({
    data.frame(RECORD_NO = xl_UNAP$RECORD_NO,
               SITE_NO = xl_UNAP$SITE_NO,
               STATION_NM = xl_UNAP$STATION_NM,
               PARM_CD = xl_UNAP$PARM_CD,
               PARM_NM = xl_UNAP$PARM_NM,
               PARM_GRP = xl_UNAP$PARM_SEQ_GRP_CD,
               FLAGGED = xl_UNAP$FLAGS)
  },error=function(e) {
    newErr <- simpleError("Empty rows need to be deleted from Unnaproved data tab before refresh")
    return(message(newErr))  
  })
}, ignoreNULL = FALSE)


###Render the unapproved data tab in shiny
output$UNAPtable <- DT::renderDataTable(UNAPdata(),
                                        ##extensions = list(FixedColumns = list(leftColumns = 1)),
                                        server=TRUE,
                                        rownames= FALSE,
                                        selection = 'single',
                                        options = list(
                                          dom='tip',
                                          ordering=F,
                                          scrollX=TRUE,
                                          autoWidth=TRUE,
                                          pageLength = 25)
)

###Another reactive variable to hold the values for what is selected in the unnaproved data table in shiny
UNAP_sel <- reactive({
  tryCatch({
    unique(UNAPdata()[input$UNAPtable_rows_selected,c("RECORD_NO","SITE_NO","STATION_NM","PARM_CD","PARM_NM","PARM_GRP","FLAGGED")])
  },error=function(e) {
    newErr <- simpleWarning("Empty rows need to be deleted from 'DQI needs review' data tab before refresh")
    return(message(newErr))  
  })
})

###This watches for changes in the selected rows (i.e. UNAP_sel) and then updates the selction boxes
###for the plots with what is selected in the table, and thus changes the plots as well

observe({
  tryCatch({
    updateSelectInput(session, "siteSel",
                      # choices = c("All",
                      #             setNames((siteSelData$SITE_NO),
                      #                      paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                      # ),
                      selected = setNames(UNAP_sel()$SITE_NO,
                                          paste((UNAP_sel()$SITE_NO),(UNAP_sel()$STATION_NM),sep="-")
                      )
    )
    
    updateSelectInput(session, "parmSel",
                      # choices = setNames((parmSelData$PARM_CD),
                      #                    paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-")
                      # ),
                      selected = setNames(UNAP_sel()$PARM_CD,
                                          paste((UNAP_sel()$PARM_CD),(UNAP_sel()$PARM_NM),sep="-")
                      )
                      
    )
  },error=function(e) {
    newErr <- simpleWarning("Empty rows need to be deleted from 'DQI needs review' data tab before refresh")
    return(message(newErr))  
  })
  
})


#####################
###Plotting elements
#####################

###Timeseries

output$qwtsPlot <- renderPlotly({
  validate(need(!is.null(input$siteSel) & !is.null(input$parmSel),
                "No site or parameter selected"))
  
  if(input$siteSel == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel)
  }
  ggplotly(
    qwtsPlot(qw.data = qw.data,
             new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
             site.selection = sites,
             plotparm = as.character(input$parmSel),
             highlightrecords = UNAP_sel()$RECORD_NO,
             show.smooth = input$fit_timeseries,
             labelDQI = input$labelDQI,
             facet = input$facetSel,
             show.q = input$showQ,
             printPlot=FALSE) + theme(legend.title = element_blank()), tooltip = 'text') %>% 
    config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
})

###Seasonal

output$qwseasonalPlot <- renderPlotly({
  validate(need(!is.null(input$siteSel) & !is.null(input$parmSel),
                "No site or parameter selected"))
  
  if(input$siteSel == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel)
  }
  
  ggplotly(qwseasonalPlot(qw.data = qw.data,
                          new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                          site.selection = sites,
                          plotparm = as.character(input$parmSel),
                          facet = input$facetSel,
                          labelDQI = input$labelDQI,
                          show.smooth = input$fit_seasonal,
                          highlightrecords = UNAP_sel()$RECORD_NO,
                          print = FALSE) + theme(legend.title = element_blank()), tooltip = 'text') %>% 
    config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
  
})

###Left-side parameter parameter plot
output$qwparmParmPlot1 <- renderPlotly({
  validate(need(!is.null(input$siteSel) & !is.null(input$parmSel) & !is.null(input$parmSel_parmParmX1),
                "No site or parameter selected"))
  
  if(input$siteSel == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel)
  }
  
  if("Log10X" %in% input$axes_parmParm1)
  {
    log.scaleX1 <- TRUE
  } else(log.scaleX1 <- FALSE)
  if("Log10Y" %in% input$axes_parmParm1)
  {
    log.scaleY1 <- TRUE 
  } else(log.scaleY1 <- FALSE)
  
  ggplotly(qwparmParmPlot(qw.data = qw.data,
                          site.selection = sites,
                          xparm = as.character(input$parmSel_parmParmX1),
                          yparm = as.character(input$parmSel),
                          facet = input$facetSel,
                          new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                          show.lm = input$fit_parmParm1,
                          labelDQI = input$labelDQI,
                          log.scaleY = log.scaleY1,
                          log.scaleX = log.scaleX1,
                          highlightrecords = UNAP_sel()$RECORD_NO,
                          printPlot = FALSE) + theme(legend.title = element_blank()), tooltip = 'text') %>% 
    config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
  
  
  
})

###Right-side parameter parameter plot
output$qwparmParmPlot2 <- renderPlotly({
  validate(need(!is.null(input$siteSel) & !is.null(input$parmSel) & !is.null(input$parmSel_parmParmX2),
                "No site or parameter selected"))
  
  if(input$siteSel == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel)
  }
  
  if("Log10X" %in% input$axes_parmParm2)
  {
    log.scaleX2 <- TRUE
  } else(log.scaleX2 <- FALSE)
  if("Log10Y" %in% input$axes_parmParm2)
  {
    log.scaleY2 <- TRUE 
  } else(log.scaleY2 <- FALSE)
  
  ggplotly(qwparmParmPlot(qw.data = qw.data,
                          site.selection = sites,
                          xparm = as.character(input$parmSel_parmParmX2),
                          yparm = as.character(input$parmSel),
                          facet = input$facetSel,
                          new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                          show.lm = input$fit_parmParm2,
                          labelDQI = input$labelDQI,
                          log.scaleY = log.scaleY2,
                          log.scaleX = log.scaleX2,
                          highlightrecords = UNAP_sel()$RECORD_NO,
                          printPlot = FALSE) + theme(legend.title = element_blank()), tooltip = 'text') %>% 
    config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
  
})

###Parameter boxplot
output$qwboxplot <- renderPlot({
        validate(
                need(!is.null(input$siteSel) &
                             !is.null(input$parmSel),
                     "No site selected"))
        
        if(input$siteSel == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel)
        }
        qwparmBoxPlot(qw.data = qw.data,
                    plotparm = as.character(input$parmSel),
                    site.selection = sites,
                    printPlot=FALSE) 
})

