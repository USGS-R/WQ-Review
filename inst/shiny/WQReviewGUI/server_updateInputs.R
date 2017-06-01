###############################################
###Update side bar elements with appropriate inputs based on data import
###############################################
parmSelData <- unique(qw.data$PlotTable[c("PARM_CD","PARM_NM")])
###Reorder by parm_cd
parmSelData <- parmSelData[order(parmSelData$PARM_CD,parmSelData$PARM_NM),]

siteSelData <- unique(qw.data$PlotTable[c("SITE_NO","STATION_NM")])

updateSelectInput(session, "siteSel",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
                  
)

##CB tab
updateSelectInput(session, "siteSel_cb",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)


if("00061" %in% parmSelData$PARM_CD)
{
        updateSelectInput(session, "parmSel_parmParmX1",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-")),
                          selected = "00061"
        )
} else {
        updateSelectInput(session, "parmSel_parmParmX1",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
}

if("00095" %in% parmSelData$PARM_CD)
{
        updateSelectInput(session, "parmSel_parmParmX2",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-")),
                          selected = "00095"
        )
} else{
        updateSelectInput(session, "parmSel_parmParmX2",
                          choices = setNames((parmSelData$PARM_CD),
                                             paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
        )
}

# ################################
# ###Matrix plot inputs update####
# ################################
# 
# updateSelectInput(session, "siteSel_matrix",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                      paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# updateSelectInput(session, "parmSel_matrix",
#                   choices = setNames((parmSelData$PARM_CD),
#                                      paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
# )
# 


# ################################
# ###rep and blank plot inputs update####
# ################################
updateSelectInput(session, "siteSel_br",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_br",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

# ################################
# ###MAp plot inputs update####
# ################################
# #updateSelectInput(session, "siteSel_map",
# #                  choices = setNames((siteSelData$SITE_NO),
# #paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
# #)
# 
# #updateSelectInput(session, "parmSel_map",
# #                     choices = setNames((parmSelData$PARM_CD),
# #                                        paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
# #)
# 
# 
# ################################
# ###blank table inputs update####
# ################################
# updateSelectInput(session, "siteSel_blankTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# ################################
# ###balance table inputs update####
# ################################
# updateSelectInput(session, "siteSel_balanceTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# ################################
# ###long table inputs update####
# ################################
# updateSelectInput(session, "siteSel_longDataTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# ################################
# ###rep table inputs update####
# ################################
# updateSelectInput(session, "siteSel_repTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# ################################
# ###wholevpart table inputs update####
# ################################
# updateSelectInput(session, "siteSel_wholevpartTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )
# 
# ################################
# ###wide table inputs update####
# ################################
# updateSelectInput(session, "siteSel_wideDataTable",
#                   choices = c("All",
#                               setNames((siteSelData$SITE_NO),
#                                        paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#                   )
# )