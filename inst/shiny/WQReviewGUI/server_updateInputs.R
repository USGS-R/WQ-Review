###############################################
###Update side bar elements with appropriate inputs based on data import
###############################################
parmSelData <- unique(plotTable[c("PARM_CD","PARM_NM")])
###Reorder by parm_cd
parmSelData <- parmSelData[order(parmSelData$PARM_CD,parmSelData$PARM_NM),]

siteSelData <- unique(plotTable[c("SITE_NO","STATION_NM")])

updateSelectInput(session, "siteSel_TS",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_TS",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
                  
)
################################
###Seasonal plot inputs update####
################################
updateSelectInput(session, "siteSel_seasonal",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_seasonal",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
                  
)

################################
###parmBox plot inputs update####
################################
updateSelectInput(session, "siteSel_parmBox",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_parmBox",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###parmParmplot inputs update####
################################

updateSelectInput(session, "siteSel_parmParm",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_parmParmX",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)
updateSelectInput(session, "parmSel_parmParmY",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###Matrix plot inputs update####
################################

updateSelectInput(session, "siteSel_matrix",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_matrix",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###Chargebalance plot inputs update####
################################

updateSelectInput(session, "siteSel_cb",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###SC Sum Plot plot inputs update####
################################

updateSelectInput(session, "siteSel_scSum",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###repBox plot inputs update####
################################
updateSelectInput(session, "siteSel_repBox",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_repBox",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###blank plot inputs update####
################################
updateSelectInput(session, "siteSel_blank",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                     paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

updateSelectInput(session, "parmSel_blank",
                  choices = setNames((parmSelData$PARM_CD),
                                     paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
)

################################
###MAp plot inputs update####
################################
#updateSelectInput(session, "siteSel_map",
#                  choices = setNames((siteSelData$SITE_NO),
#paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
#)

#updateSelectInput(session, "parmSel_map",
#                     choices = setNames((parmSelData$PARM_CD),
#                                        paste((parmSelData$PARM_CD),(parmSelData$PARM_NM),sep="-"))
#)


################################
###blank table inputs update####
################################
updateSelectInput(session, "siteSel_blankTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###balance table inputs update####
################################
updateSelectInput(session, "siteSel_balanceTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###long table inputs update####
################################
updateSelectInput(session, "siteSel_longDataTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###rep table inputs update####
################################
updateSelectInput(session, "siteSel_repTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###wholevpart table inputs update####
################################
updateSelectInput(session, "siteSel_wholevpartTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)

################################
###wide table inputs update####
################################
updateSelectInput(session, "siteSel_wideDataTable",
                  choices = c("All",
                              setNames((siteSelData$SITE_NO),
                                       paste((siteSelData$SITE_NO),(siteSelData$STATION_NM),sep="-"))
                  )
)