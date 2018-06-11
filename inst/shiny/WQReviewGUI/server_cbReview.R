###################################
###This contains all plotting and sidebar server-side
###elements for the result-level review tab
###################################

####################
###Sidebar
####################

###This creates a reactive data frame to update off the excel charge balance issues data tab
CBdata <<- eventReactive(input$refresh_cb, {
        tryCatch({
                data.frame(RECORD_NO = xl_CB$RECORD_NO,
                           SITE_NO = xl_CB$SITE_NO,
                           STATION_NM = xl_CB$STATION_NM,
                           MEDIUM_CD = xl_CB$MEDIUM_CD,
                           SAMPLE_CM_TX = xl_CB$SAMPLE_CM_TX,
                           FLAGGED = xl_CB$BadCB_30.21)  
        }, error = function(e){})
}, ignoreNULL = FALSE)


###Render the CB issues data tab in shiny
output$badCBTable <- DT::renderDataTable(CBdata(),
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


###Another reactive variable to hold the values for what is selected in the CB data table in shiny
CB_sel <- reactive({
  unique(CBdata()[input$badCBTable_rows_selected,c("RECORD_NO","SITE_NO","STATION_NM","MEDIUM_CD","SAMPLE_CM_TX","FLAGGED")])
})


###This watches for changes in the selected rows (i.e. CB_sel) and then updates the selction boxes
###for the plots with what is selected in the table, and thus changes the plots as well
observe({
        tryCatch({
                updateSelectInput(session, "siteSel_cb",
                                  # choices = c("All",
                                  #             setNames((siteSelData$SITE_NO),
                                  #                      paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                                  # ),
                                  selected = setNames(CB_sel()$SITE_NO,
                                                      paste((CB_sel()$SITE_NO),(CB_sel()$STATION_NM),sep="-")
                                  )
                )
                
                
                
        }, error = function(e){})
})

output$qwcbPlot <- renderPlotly({
  validate(need(!is.null(input$siteSel_cb),
                "No site selected"))

  if(input$siteSel_cb == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel_cb)
  }
  ggplotly(
    qwcbPlot(qw.data = qw.data,
             #new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
             site.selection = sites,
             highlightrecords = CB_sel()$RECORD_NO,
             #show.smooth = input$fit_timeseries,
             #labelDQI = input$labelDQI,
             facet = input$facetSel_cb,
             printPlot=FALSE), tooltip = 'text') %>%
    config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
})

output$qwscSumPlot <- renderPlotly({
  validate(need(!is.null(input$siteSel_cb),
                "No site selected"))

  if(input$siteSel_cb == "All")
  {
    sites <- unique(qw.data$PlotTable$SITE_NO)
  } else {
    sites <- as.character(input$siteSel_cb)
  }

  ggplotly(qwscSumPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = sites,
                       #labelDQI = input$labelDQI_scSum,
                       facet = input$facetSel_cb,
                       highlightrecords = CB_sel()$RECORD_NO,
                       printPlot=FALSE))%>% config(collaborate=F,cloud=F,showLink=F,displaylogo=F,modeBarButtonsToRemove=c("lasso2d","select2d","autoScale2d","zoom2d","sendDataToCloud"))
})