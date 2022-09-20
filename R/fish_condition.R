#' A function to intake length-weight data and biomass and stratum area from RACEBase
#'
#' This function queries RACEBase HAUL, CATCH, & SPECIMEN tables for length-weight data.
#' In addition, it queries Region-specific strata tables for grouping variables.
#' The function is hard coded for which Region-specific species to query and has additional constraints. 
#' @param Vessel vessel id
#' @param Cruise cruise id
#' @param Haul haul number
#' @param Species Code RACE species code
#' @param Region GOA, AI, BS, or NBS
#' @param Start Time on bottom time
#' @param INPFC Area [AI, GOA] / Strata [BS, NBS]
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

	# August 12 2020
	# updated get.lwdata to query biomass table products from the different regions to provide for biomass weighting of condition
	# updated output object to be a list of 2 data frames rather than a single data frame.
	
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
		# note that removed from BS/NBS "and abundance_haul = Y from constraints because it eliminated 2018 NBS data and it's probably
		# OK and few instances to boot of length-weight data originating with non-abundance hauls in other years and regions
		# August 12 2020
		# Added a query to get stratum biomass from stock products in all regions so that we can do biomass weighting of residuals
		# but not have to recreate the biomass expansion
		# Output will now be a list of two data frames and in the case of the Bering Sea chapter the data call would need to be run 
		# twice, once for BS and once for NBS
     if(region == "GOA"){
          lw.qry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time, c.inpfc_area inpfc_stratum,
          a.stratum, a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
          e.common_name, d.sex, d.length, d.weight, b.weight catch_weight, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d , racebase.species e
          where a.region = 'GOA' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,30152)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0
		  and a.abundance_haul = 'Y'"
		  biomass.area.qry <- "select a.year, a.species_code, a.haul_count, a.catch_count, a.area_biomass, a.biomass_var, b.inpfc_area inpfc_stratum, b.inpfc_stratum_area 
			from goa.biomass_inpfc a, (select inpfc_area, summary_area, sum(area) inpfc_Stratum_area from goa.goa_strata where nvl(stratum,1) != 0
			and survey = 'GOA' group by inpfc_area, summary_area) b where (a.year >= 1984 and a.year != 1989) and a.species_code in 
			(21740,21741,21720,30420,10262,10110,30060,30152) and a.summary_Area = b.summary_area order by a.year, a.species_code, b.inpfc_Area"
			# returns 511 records 08/12/20
     }else if(region == "AI"){
          lw.qry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time, c.inpfc_area inpfc_stratum,
          a.stratum, a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
          e.common_name, d.sex, d.length, d.weight, b.weight catch_weight, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d, racebase.species e
          where a.region = 'AI' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,21921)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0
		  and a.abundance_haul = 'Y'"
		  biomass.area.qry <- "select a.year, a.species_code, a.haul_count, a.catch_count, a.area_biomass, a.biomass_var, b.inpfc_area inpfc_stratum, b.inpfc_stratum_area 
			from ai.biomass_inpfc a, (select inpfc_area, summary_area, sum(area) inpfc_Stratum_area from goa.goa_strata where nvl(stratum,1) != 0
			and survey = 'AI' group by inpfc_area, summary_area) b where (a.year >= 1984 and a.year != 1989) and a.species_code in 
			(21740,21741,21720,30420,10262,10110,30060,21921) and a.summary_Area = b.summary_area order by a.year, a.species_code, b.inpfc_Area"
			# returns 352 records 08/12/20
     }else if(region == "BS"){
          # note that the lw.qry for BS is identical to the lw.qry for NBS
		  lw.qry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time,
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
		biomass.area.qry <- "select species_code,common_name,year,stratum,biomass,varbio,lowerb,upperb
		from haehnr.biomass_ebs_plusnw where species_code in (21740, 21741, 21720, 10110, 10210, 10130, 10261, 10285) 
		and stratum between 1 and 6"
          }else if(region == "NBS"){
          # note that the lw.qry for BS is identical to the lw.qry for NBS
		  lw.qry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time,
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
		biomass.area.qry <- "select species_code,common_name,year,stratum,biomass,varbio,lowerb,upperb
		from haehnr.biomass_nbs_safe where species_code in (21740, 21741, 21720, 10110, 10210, 10130, 10261, 10285)
		and stratum = 999"
		}

     print("Querying database...")
	 lwdata <- sqlQuery(channel = racebase, query = lw.qry, rows_at_time = 1)
	 biomassdata <- sqlQuery(channel = racebase, query = biomass.area.qry, rows_at_time = 1)

     if(close.channel)close(racebase)

     out.list <- list(lwdata, biomassdata)
	 
	 return(out.list)

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
#' @param make_diagnostics Output diagnostic plots and summaries? Default FALSE produces no diagnostics and summaries. If true, species_code and region should be specified.
#' @param species_code Species code (provide if make_diagnostics = TRUE)
#' @param region Region (provide if make_diagnostics = TRUE)
#' @param year Year (provide if make_diagnostics = TRUE)
#' @keywords length, weight, groundfish condition
#' @references Brodziak, J.2012. Fitting length-weight relationships with linear regression using the log-transformed allometric model with bias-correction. Pacific Islands Fish. Sci. Cent., Natl. Mar. Fish. Serv., NOAA, Honolulu, HI 96822-2396. Pacific Islands Fish. Sci. Cent. Admin. Rep. H-12-03, 4 p.
#' @export

calc_lw_residuals <- function(len, wt, stratum = NA, bias.correction = TRUE, outlier.rm=FALSE, make_diagnostics = FALSE, species_code = NA, year = NA) {
  
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
    
    return(list(mod = lw.mod, 
                fitted_wt  = fitted_wt))
  }
  
  # Run length-weight regression
  lw.reg <- run_lw_reg(logwt = logwt, loglen = loglen, stratum = stratum)
  
  length(lw.reg$fitted_wt)
  
  #Assessing Outliers using Bonferroni Outlier Test
  #Identify if there are any outliers in your data that exceed cutoff = 0.05 (default)
  if(outlier.rm) {
    #Produce a Bonferroni value for each point in your data
    bonf_p <- car::outlierTest(lw.reg$mod,n.max=Inf,cutoff=Inf,order=FALSE)$bonf.p 
    remove <- which(bonf_p < .7)
    print("Outlier rows removed")
    logwt[remove] <- NA
    loglen[remove] <- NA
    
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
  
  # Make diagnostic plots ----
  if(make_diagnostics) {

    # Create output directory if it doesn't exist ----
    out_path <- paste0("./output/", region[1], "/")
    
    if(!dir.exists(paste0("./output/"))) {
      dir.create(paste0("./output/"))
    }
    
    
    if(!dir.exists(out_path)) {
      dir.create(out_path)
    }

    # Sample sizes
    sample_size_df <- data.frame(year, stratum) %>%
      dplyr::group_by(year, stratum) %>%
      dplyr::summarise(n = n())
    
    sample_size_plot <- ggplot(data = sample_size_df, 
                               aes(x = year, y = factor(stratum), fill = n, label = n)) + 
      geom_tile() + 
      geom_text() +
      scale_x_continuous(name = "Year") +
      scale_y_discrete(name = "Stratum") +
      scale_fill_distiller(palette = "Purples", direction = 1) +
      theme_bw()
    
    # RMSE by year and stratum
    rmse <- function(x) {
      return(mean(sqrt(x^2)))
    }
    
    rmse_df <- data.frame(lw.res, stratum) %>%
      dplyr::group_by(stratum) %>%
      dplyr::summarise(RMSE = rmse(lw.res))
    
    write.csv(rmse_df, file = paste0(out_path, region[1], "_", species_code[1], "_rmse.csv"), row.names = FALSE)
    
    rmse_plot <- ggplot() + 
      geom_point(data = rmse_df,
                 aes(x = stratum, y = RMSE), 
                 size = rel(1.3)) +
      scale_x_continuous(name = "Year") +
      scale_color_discrete(name = "Stratum") + 
      theme_bw()
    
    size_dist_plot <- ggplot(data = data.frame(len, stratum, year)) +
      geom_density(aes(x = len, color = factor(stratum))) +
      scale_color_discrete(name = "Stratrum") +
      scale_x_continuous(name = "Length (mm)") +
      scale_y_continuous(name = "Density") +
      facet_wrap(~year, ncol = 5) + 
      theme_bw()
    
    pdf(paste0(out_path, region[1], "_", species_code[1], "_diagnostic_plots.pdf"), 
        onefile = TRUE, width = 16, height = 8)
    print(sample_size_plot)
    print(rmse_plot)
    print(size_dist_plot) 
    dev.off()
    
    pdf(paste0(out_path, region[1], "_", species_code[1], "_regression_diagnostics.pdf"), onefile = TRUE)
    par(mfrow = c(2,2))
    plot(lw.reg$mod)
    dev.off()
    
    sink(file = paste0(out_path,region[1], "_", species_code[1], "_model_summary.txt"))
    print(summary(lw.reg$mod))
    sink()

  }
  
  return(lw.res)
}



#' A function to weight length-weight residuals by catch
#'
#' This function weights length-weight residuals by a catch column. This
#' catch can be CPUE from the tow where the fish was caught (most common) or
#' stratum CPUE or biomass. 
#' @param residuals Residual that will be weighted by catch
#' @param year Year of sample must be the same length as the residuals
#' @param catch Catch for weighting residual (default = 1) must be the same length as residuals
#' @param stratum Vector of strata for weighting
#' @param stratum_biomass Biomass in kg for the stratum.
#' @keywords length, weight, groundfish condition
#' @export

weight_lw_residuals <- function(residuals, year, stratum = NA, stratum_biomass = NA, catch = 1) {
  
  wtlw.res <- residuals
  
  if(length(catch) == 1){
    catch <- rep(1, length(residuals))
  }
  
  if(is.na(stratum)[1]) {
    
    unique_years <- unique(year)
    
    # Calculate residuals by year with stratum weighting by catch (CNR's code)
    for(i in 1:length(unique_years)){
      year_ind <- which(year == unique_years[i])
      sel_resid <- residuals[year_ind]
      sel_catch <- catch[year_ind]
      var1 <- sel_resid * sel_catch
      var2 <- sum(sel_catch)
      var3 <- var1/var2*length(sel_catch)
      wtlw.res[year_ind] <- var3
    }
  } else if(!is.na(stratum)[1] & is.na(stratum_biomass)) {
    # Calculate residuals by stratum without biomass expansion by stratum area 
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
  } else {
    # 2020 ESR: Calculate residuals by stratum with biomass-weighted expansion (2020 ESR)
    # stratum_biomass <- c(rep(0.5,4), c(0.25, 0.25, 1.5, 1.5))
    # residuals <- c(c(0.05, 0.05, 0.5, 0.5), c(0.05, 0.05, 0.5, 0.5))
    # year <- c(rep(2018,4), rep(2019,4))
    # stratum <- c(c(1,1,2,2), c(1,1,2,2))
    
    biomass_df <- data.frame(stratum_biomass, stratum, year) %>% 
      unique()
    
    biomass_proportion_df <- biomass_df %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(year_biomass = sum(stratum_biomass)) %>%
      dplyr::inner_join(biomass_df) %>%
      dplyr::mutate(stratum_weight = stratum_biomass/year_biomass) %>%
      dplyr::select(year, stratum, stratum_weight)
    
    residuals_df <- data.frame(residuals, 
                               year, 
                               stratum) %>% 
      dplyr::inner_join(biomass_proportion_df) %>%
      dplyr::mutate(weighted_residuals = residuals * stratum_weight)
    
    wtlw.res <- residuals_df$weighted_residuals
      
  }
  
  return(wtlw.res)
}