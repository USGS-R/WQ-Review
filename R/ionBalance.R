#' ionBalance
#' 
#' Takes output data object from NWISPullR and calculates a QWData standard ion charge balance.
#' @param qw.data A qw.data object generated from NWISPullR
#' @import plyr
#' @export
#' 

ionBalance <- function(qw.data, wide = FALSE)
{
  #ion.balance.data <- read.csv("Data/ion_bal_mills.csv",header=TRUE,colClasses = "character")
  
  ###join charge balance info to plot table by parameter code
  ion.charges <- join(qw.data$PlotTable,ion.balance.data, by = "PARM_CD")
  
  ###Make some collumns numeric for operations
  ion.charges$ION_BAL_MEQ_FC <- as.numeric(ion.charges$ION_BAL_MEQ_FC)
  ion.charges$RESULT_VA <- as.numeric(ion.charges$RESULT_VA)
  ion.charges$ION_BAL_PREF_NU <- as.numeric(ion.charges$ION_BAL_PREF_NU)
  
  ###Round RESULT_VA to 3 decimal places 
  ion.charges$RESULT_VA <- round(ion.charges$RESULT_VA,digits = 3)
  
  
  ###Calculate pH charge variables
  ion.charges$ION_BAL_CD[which(ion.charges$PARM_CD == "00400")] <- "H_ion"
  ion.charges$ION_BAL_PREF_NU[which(ion.charges$PARM_CD == "00400")] <- 1
  
  
  ###Make records factors
  ion.charges$RECORD_NO <- as.factor(ion.charges$RECORD_NO)
  ###Calculate charge
  ion.charges$meqCharge <- ion.charges$RESULT_VA * ion.charges$ION_BAL_MEQ_FC
  
  ###Calculate pH charge
  ion.charges$meqCharge[which(ion.charges$ION_BAL_CD == "H_ion")] <- 10^(-ion.charges$RESULT_VA[which(ion.charges$ION_BAL_CD == "H_ion")])*1000*0.99212

  ###Function to use in ddply that does hte charge calculatoin for each record number

  chargeCalc <- function(ion.charges)
  {
  
  #Order rows by element and then preference
  ion.charges <- ion.charges[order(ion.charges$ION_BAL_CD, ion.charges$ION_BAL_PREF_NU),]
  
  ###Subset Alk and NO3 data out for sub ion calculatoin
  alkData <- subset(ion.charges,ION_BAL_CD == "Alk")
  NO2NO3Data <- subset(ion.charges,ION_BAL_CD == "NO2NO3")
  otherData <- subset(ion.charges,ION_BAL_CD %in% c("Ca","Cl","Fe","H_ion","K","Mg","Mn","Na","SO4")) 
  
  #Retain only first row, i.e. the top preference, of elements which do not have a subion
  otherData <- otherData[!duplicated(otherData$ION_BAL_CD),]
  
  ##Calculate Alk charge
    #subset to alk data
    
    #check for total alk value, which is indicated by a blank ION_BAL_SUBION_CD and use it
    if("" %in% alkData$ION_BAL_SUBION_CD) {
      
      alkData <- alkData[!duplicated(alkData$ION_BAL_CD),]
      alkCharge <- alkData$meqCharge
      
      #check for sub ions and use them if there is no total alk
    } else if (c("Bicarb","Carb","Hydrox") %in% alkData$ION_BAL_SUBION_CD && 
                 !("" %in% alkData$ION_BAL_SUBION_CD))
    {
      #Order rows by subion and then preference
      subionData <- alkData[order(alkData$ION_BAL_SUBION_CD, alkData$ION_BAL_SUBION_PREF_NU),]
      
      #Retain only first row, i.e. the top preference, of subions 
      subionData <- alkData[!duplicated(alkData$ION_BAL_SUBION_CD),]
      
      #sum subion charges
      alkCharge <- sum(subionData$meqCharge)
    } else (alkCharge <- 0)
    
    
    ##Calculate NO2NO3 charge
    #subset to NO2NO3 data
    
    #check for total NO2NO3 value, which is indicated by a blank ION_BAL_SUBION_CD and use it
    if("" %in% NO2NO3Data$ION_BAL_SUBION_CD) {
      
      NO2NO3Data <- NO2NO3Data[!duplicated(NO2NO3Data$ION_BAL_CD),]
      NO2NO3Charge <- NO2NO3Data$meqCharge
      
      #check for sub ions and use them if there is no total NO2NO3
    } else if (c("NO2","NO3") %in% NO2NO3Data$ION_BAL_SUBION_CD && 
                 !("" %in% NO2NO3Data$ION_BAL_SUBION_CD))
    {
      #Order rows by subion and then preference
      subionData <- NO2NO3Data[order(NO2NO3Data$ION_BAL_SUBION_CD, NO2NO3Data$ION_BAL_SUBION_PREF_NU),]
      
      #Retain only first row, i.e. the top preference, of subions 
      subionData <- NO2NO3Data[!duplicated(NO2NO3Data$ION_BAL_SUBION_CD),]
      
      #sum subion charges
      NO2NO3Charge <- sum(subionData$meqCharge)
    } else(NO2NO3Charge <- 0)
    
    #Calculate sum of cations
  ion.charges$sum_cat <- sum(otherData$meqCharge[which(otherData$ION_BAL_SIGN_CD == "c")])
    
    ###Calculate sum of anions
    
    ##Sum of anions 
    sumAn <- sum(otherData$meqCharge[which(otherData$ION_BAL_SIGN_CD == "a" &
                                             otherData$ION_BAL_CD != "Alk" &
                                             otherData$ION_BAL_CD != "NO2NO3")])
  ion.charges$sum_an <- sumAn + alkCharge + NO2NO3Charge
    
    ###Note whether the chemistry is complete or not
    if(all(c("Ca","Mg","Na","Cl","SO4","Alk") %in% ion.charges$ION_BAL_CD) == TRUE)
    {
      ion.charges$complete.chem = "Complete"
    }else(ion.charges$complete.chem="Incomplete")
  
  ion.charges <- ion.charges[!duplicated(ion.charges$ION_BAL_CD),]
  
    return(ion.charges)

}


###End of funciton###

###Use ddply to apply function to data-frame by record number

ion.charges <- subset(ion.charges, PARM_CD %in% ion.balance.data$PARM_CD)

if(nrow(ion.charges) > 0)
{
chargebalance.table  <- ddply(ion.charges,"RECORD_NO",chargeCalc)
chargebalance.table$perc.diff <- (chargebalance.table$sum_cat-chargebalance.table$sum_an)/(chargebalance.table$sum_cat+chargebalance.table$sum_an)*100

###Make element column for dcast table
chargebalance.table$element <-  chargebalance.table$ION_BAL_SUBION_CD
chargebalance.table$element[which(chargebalance.table$ION_BAL_SUBION_CD=="")] <-  chargebalance.table$ION_BAL_CD[which(chargebalance.table$ION_BAL_SUBION_CD=="")]


####Dcast the charge balance table and make a balance data table
BalanceDataTable <- dcast(chargebalance.table, RECORD_NO + sum_cat + sum_an +perc.diff + complete.chem ~ element,value.var = "meqCharge",fun.aggregate=median)
BalanceDataTable <- join(unique(qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD")]),
                         BalanceDataTable,by="RECORD_NO",type="right")
###Flag CB > 10% or 5%
BalanceDataTable$flags <- ""
BalanceDataTable$flags[which(abs(BalanceDataTable$perc.diff) > 5 & abs(BalanceDataTable$perc.diff) <= 10)] <- "Charge imbalance > 5%"
BalanceDataTable$flags[which(abs(BalanceDataTable$perc.diff) > 10)] <- "Charge imbalance > 10%"
###Format times to character
#BalanceDataTable$SAMPLE_START_DT <- as.POSIXTct(BalanceDataTable$SAMPLE_START_DT)

if(wide == FALSE)
{
return(chargebalance.table)
} else{return(list(chargebalance.table=chargebalance.table,BalanceDataTable=BalanceDataTable))}

}else{}

}