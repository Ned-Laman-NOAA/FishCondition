#' A function to weight length-weight residuals by catch
#'
#' This function weights length-weight residuals by a catch column. This
#' catch can be CPUE from the tow where the fish was caught (most common) or
#' stratum CPUE or biomass. 
#' @param year Year of sample must be the same length as the residuals
#' @param residual Residual that will be weighted by catch
#' @param catch Catch for weighting residual (default = 1) must be the same length as residuals
#' @keywords length, weight, groundfish condition
#' @export
#' @examples
#' weighted_resids()

weighted_resids <- function(year, residuals, catch = 1){
     wtlw.res <- residuals
     if(length(catch) == 1){catch <- rep(1, length(residuals))}
     years1 <- unique(year)
     for(i in 1:length(years1)){
          # subsetting data by year
          d0 <- which(year == years1[i])
          d1 <- residuals[d0]
          d2 <- catch[d0]
          # residuals * catch_weight
          var1 <- d1*d2
          # sum catch_weight
          var2 <- sum(d2)
          # average weighted residual times the length of the weighting factor
          # order of operations dictates that the math is exercised as (var1/var2)*length(d2)
          var3 <- var1/var2*length(d2)
          
          wtlw.res[d0] <- var3}
     return(wtlw.res)}

wt.res <- weighted_resids(year = floor(tempdata$CRUISE/100), residuals = tempdata$residuals, catch = tempdata$CATCH_WEIGHT)
