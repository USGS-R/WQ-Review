
flagReport <- function(DSN = "NWISCO",
                       env.db = "01",
                       qa.db = "02",
                       STAIDS = c("09163500"),
                       dl.parms = "All",
                       parm.group.check = TRUE,
                       begin.date = Sys.Date()-365*10,
                       end.date = Sys.Date()){
        
        rmarkdown::render("R/markdownTest.rmd",params = list(DSN=DSN,
                                                     env.db = env.db,
                                                     qa.db=qa.db,
                                                     STAIDS=STAIDS,
                                                     dl.parms=dl.parms,
                                                     parm.group.check=parm.group.check,
                                                     begin.date=begin.date,
                                                     end.date=end.date
                                                     ), 
                          output_format = "html_document"
                          )
        
}
