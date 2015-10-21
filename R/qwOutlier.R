#' Multivariate outlier detection
#' Takes output data object from readNWISodbc and runs multivariate outlier tests
#' using the robust mahalanobis distance.
#' @details Samples ar evaluated for the presence of multivariate outliers using the robust mahalnobis distance.
#' Parameters are grouped by expected relationships. For example, discharge major cations, SO4, and alkalinity are expected 
#' to be coorlleated with each other due to similar geochemical behavoir of ions and relationships with conductance and discharge.
#' Similarly, nutrient samples are grouped because of common sources. Pesticide and trace metal data are not evaluated.
#' The robust mahalanobis distance is calculated using the covMcd function from the robustbase package.
#' Samples that have mahalnobis distances greater than the specified quantile are flagged as possible outliers.
#' If the sample has been reviewed and approved based on DQI code, the outlier flag is removed.
#' WORK IN PROGRESS
#' @param qw.data A qw.data data object produced from readNWISodbc
#' @param quantile The quantile to use for outlier cutoff

qwOutlier <- function(qw.data,quantile)
{
        #Subset data to major ions
        majorIons <- subset(qw.data$PlotTable, 
                                  #major ions
                                  PARM_CD %in% ion.balance.data$PARM_CD[which(ion.balance.data$ION_BAL_CD %in% c("Ca","Mg","Na","SO4","Alk"))] |
                                  #Q pcode
                                  PARM_CD == "00061" |
                                  #SC pcodes
                                  PARM_CD %in% c("00095","90095") &
                                    MEDIUM_CD %in% c("WS ","WSQ","WG","WGQ")
                            )
        
        ##Melt data
        majorIons <- dcast(majorIons, RECORD_NO ~ PARM_NM,value.var = "RESULT_VA",fun.aggregate=median)
        majorIons <- na.omit(majorIons)

        #covMcd(na.omit(pp.plot.data[c("RESULT_VA_X","RESULT_VA_Y")]))
}