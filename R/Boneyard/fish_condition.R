#' A function to intake length-weight data from RACEBase
#'
#' This function queries RACEBase HAUL, CATCH, & SPECIMEN tables for length-weight data.
#' In addition, it queries Region-specific strata tables for grouping variables.
#' The function is hard coded for which Region-specific species to query and has additional constraints. 
#' @param Vessel vessel id
#' @param Cruise cruise id
#' @param Haul haul number
#' @param Species Code RACE species code
#' @param Region GOA, AI, or EBS
#' @param Start Time on bottom time
#' @param INPFC Area [AI, GOA] / Stratum [EBS]
#' @param StationID
#' @param Bottom Depth
#' @param Start Latitude
#' @param Start Longitude
#' @param SpecimenID
#' @param Common Name
#' @param Sex gender
#' @param Wt_CPUE_km2 cpue in kg/km2 calculated on the fly
#' @param Year translated from Cruise ID
#' @keywords length, weight, groundfish condition

get.lwdata <- function(channel = NA, region = "GOA"){

     if(is.na(channel)){
          require(RODBC)
          racebase <- odbcConnect(dsn = "AFSC", uid = "lamane", pwd = "wilsoni_30550", believeNRows = FALSE)
          # This code will work in RStudio but NOT when knitting and RMarkdown document
          #racebase <- odbcConnect(dsn = "AFSC", uid = rstudioapi::showPrompt(title = "Username", message = "Oracle Username", default = ""), 
		#	pwd = rstudioapi::askForPassword("enter password"), believeNRows = FALSE)
          close.channel = TRUE
     }else{
          close.channel <- FALSE
          }

		# note that as of 10/2019 and migration to Windows 10, != in SQL appears to test NULL against Not Null values
		# which is a major change in ORACLE Behavior
		# note that removed "and abundance_haul = Y from constraints because it eliminated 2018 NBS data and it's probably
		# OK and few instances to boot of length-weight data originating with non-abundance hauls in other years and regions
     if(region == "GOA"){
          sqry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time, c.inpfc_area,
          a.stratum, a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
          e.common_name, d.sex, d.length, d.weight, b.weight/(a.distance_fished*(a.net_width/1000)) wt_cpue_km2, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d , racebase.species e
          where a.region = 'GOA' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,30152)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0
		  and a.abundance_haul = 'Y'"
     }else if(region == "AI"){
          sqry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time, c.inpfc_area,
          a.stratum, a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
          e.common_name, d.sex, d.length, d.weight, b.weight/(a.distance_fished*(a.net_width/1000)) wt_cpue_km2, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d, racebase.species e
          where a.region = 'AI' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,21921)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0
		  and a.abundance_haul = 'Y'"
     }else if(region == "BS"){
          sqry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time,
		decode(a.stratum, 31, 30, 32, 30, 61, 60, 62, 60, 41, 40, 42, 40, 43, 40, a.stratum) stratum,
		a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
		e.common_name, d.sex, d.length, d.weight, b.weight/(a.distance_fished*(a.net_width/1000)) wt_cpue_km2, floor(a.cruise/100) year
		from racebase.haul a, racebase.catch b, racebase.specimen d , racebase.species e, race_data.v_cruises c
		where a.region = 'BS' and a.cruise >= 198201 and a.stationid like '%-%'
		and b.species_code in (21740, 21741, 21720, 10110, 10210, 10130, 10261, 10285)
		and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
		and b.species_code = e.species_code and b.species_code = d.species_code and d.length != 0 and d.weight != 0
		and a.cruisejoin = c.cruisejoin and c.survey_definition_id != 78 and a.stratum not in (82, 90)
		and a.stratum between 10 and 99
		and a.performance >= 0"
		# like '%-%' eliminates corner stations which don't have a dash in the stationid
		# survey_definition_id != 78 eliminates Bering Slope stations
		# decode to combine strata  and stratum range between 10 and 99 per original intent
		# stratum not in (82, 90) because these are non-standard strata and were excluded a priori
          }

     lwdata <- sqlQuery(channel = racebase, query = sqry, rows_at_time = 1)

     if(close.channel)close(racebase)

     lwdata

     }



#' A function to weight length-weight residuals by catch
#'
#' This function weights length-weight residuals by a catch column. This
#' catch can be CPUE from the tow where the fish was caught (most common) or
#' stratum CPUE or biomass. 
#' @param residual Residual that will be weighted by catch
#' @param year Year of sample must be the same length as the residuals
#' @param catch Catch for weighting residual (default = 1) must be the same length as residuals
#' @param stratum Vector of strata for weighting
#' @keywords length, weight, groundfish condition
#' @export
#' @examples

weight_lw_residuals <- function(residuals, year, stratum = NA, catch=1) {
  
  wtlw.res <- residuals
  
  if(length(catch) == 1){
    catch <- rep(1, length(residuals))
  }
  
  if(is.na(stratum)[1]) {
    
    unique_years <- unique(year)
    
    # Calculate residuals by year
    for(i in 1:length(unique_years)){
      year_ind <- which(year == unique_years[i])
      sel_resid <- residuals[year_ind]
      sel_catch <- catch[year_ind]
      var1 <- sel_resid * sel_catch
      var2 <- sum(sel_catch)
      var3 <- var1/var2*length(sel_catch)
      wtlw.res[year_ind] <- var3
    }
  } else {
    unique_years_stratum <- expand.grid(year = unique(year), stratum = unique(stratum))
    
    for(i in 1:nrow(unique_years_stratum)) {
      ind <- which(year == unique_years_stratum['year'] & stratum == unique_years_stratum['stratum'])
      sel_resid <- residuals[ind]
      sel_catch <- catch[ind]
      var1 <- sel_resid * sel_catch
      var2 <- sum(sel_catch)
      var3 <- var1/var2*length(sel_catch)
      wtlw.res[ind] <- var3
    }
  }
  
  return(wtlw.res)
}

#' A function to calculate length-weight residuals
#'
#' This function makes a log-log regression of length and weight for individual fish and then calculates a residual. A Bonferroni-corrected outlier correction can be applied to remove outliers. Option to use separate covariates for different strata.
#' 
#' @param length Set of individual fish lengths.
#' @param weight Corresponding set of individual fish weights.
#' @param stratum Stratum code for length-weight regression by stratum.
#' @param bias.correct Bias corrected residuals following Brodziak (2012)
#' @param outlier.rm Should outliers be removed using Bonferoni test (cutoff = 0.7)
#' @keywords length, weight, groundfish condition
#' @references Brodziak, J.2012. Fitting length-weight relationships with linear regression using the log-transformed allometric model with bias-correction. Pacific Islands Fish. Sci. Cent., Natl. Mar. Fish. Serv., NOAA, Honolulu, HI 96822-2396. Pacific Islands Fish. Sci. Cent. Admin. Rep. H-12-03, 4 p.

# Would like to have access to the adjusted R-squared values for all of the length weight relationships to assess fit
# summary(lw.mod)$adj.r.squared

calc_lw_residuals <- function(len, wt, stratum = NA, bias.correction = TRUE, outlier.rm=FALSE) {
  
  loglen <- log(len)
  logwt <- log(wt)
  
  run_lw_reg <- function(logwt, loglen, stratum) {
    if(is.na(stratum)[1]) {
      lw.mod <-lm(logwt~loglen, na.action = na.exclude)
      fitted_wt <- predict(lw.mod, newdata = data.frame(len = len)) 
    } else {
      stratum <- factor(stratum)
      lw.mod <- lm(logwt~loglen:stratum, na.action = na.exclude)
      fitted_wt <- predict(lw.mod, newdata = data.frame(len = len, stratum = stratum)) 
    }
    
    return(list(mod = lw.mod, fitted_wt  = fitted_wt))
  }
  
  # Run length-weight regression
  lw.reg <- run_lw_reg(logwt = logwt, loglen = loglen, stratum = stratum)
  
  length(lw.reg$fitted_wt)
  
  #Assessing Outliers using Bonferroni Outlier Test
  #Identify if there are any outliers in your data that exceed cutoff = 0.05 (default)
  if(outlier.rm) {
    #Produce a Bonferroni value for each point in your data
    bonf_p <- car::outlierTest(lw.reg$mod,n.max=Inf,cutoff=Inf,order=FALSE)$bonf.p 
    removed <- which(bonf_p < 0.7)
    print("Outliers rows removed")
    print(unname(removed))
    logwt[removed] <- NA
    loglen[removed] <- NA
    
    # Rerun without outliers
    lw.reg <- run_lw_reg(logwt = logwt, loglen = loglen, stratum = stratum)
  } 
  
  # Apply bias correction factor
  if(bias.correction) {
    syx <- summary(lw.reg$mod)$sigma
    cf <- exp((syx^2)/2) 
    lw.reg$fitted_wt <- log(cf *(exp(lw.reg$fitted_wt)))
  }
  
  lw.res <- (logwt - lw.reg$fitted_wt)
  
  return(lw.res)
}

