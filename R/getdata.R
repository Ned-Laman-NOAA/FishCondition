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
          racebase <- odbcConnect(dsn = "AFSC", uid = "lamane", pwd = "macrochir_30025", believeNRows = FALSE)
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

