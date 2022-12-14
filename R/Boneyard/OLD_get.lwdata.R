get.lwdata <- function(channel = NA, region = "GOA", user = "lamane", password = "wilsoni_30550"){

     if(is.na(channel)){
          require(RODBC)
          racebase <- odbcConnect(dsn = "AFSC", uid = user, pwd = password, believeNRows = FALSE)
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
          e.common_name, d.sex, d.length, d.weight, b.weight catch_weight, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d , racebase.species e
          where a.region = 'GOA' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,30152)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0"
     }else if(region == "AI"){
          sqry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time, c.inpfc_area,
          a.stratum, a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
          e.common_name, d.sex, d.length, d.weight, b.weight catch_weight, floor(a.cruise/100) year
          from racebase.haul a, racebase.catch b, goa.goa_strata c, racebase.specimen d , racebase.species e
          where a.region = 'AI' and (a.cruise >= 198401 and a.cruise != 198901) and b.species_code in
          (21740,21741,21720,30420,10262,10110,30060,21921)
          and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
          and b.species_code = e.species_code and b.species_code = d.species_code and a.stratum = c.stratum
          and a.region = c.survey and d.length != 0 and d.weight != 0"
     }else if(region == "BS"){
          sqry <- "select a.haul, a.vessel, a.cruise, b.species_code, a.region, a.start_time,
			decode(a.stratum, 31, 30, 32, 30, 61, 60, 62, 60, 41, 40, 42, 40, 43, 40, a.stratum) stratum,
			a.stationid, a.bottom_depth, a.start_latitude, a.start_longitude, d.specimenid,
			e.common_name, d.sex, d.length, d.weight, b.weight catch_weight, floor(a.cruise/100) year
			from racebase.haul a, racebase.catch b, racebase.specimen d , racebase.species e, race_data.v_cruises c
			where a.region = 'BS' and a.cruise >= 198201 and a.stationid like '%-%'
			and b.species_code in (21740, 21741, 21720, 10110, 10210, 10130, 10261, 10285)
			and a.hauljoin = b.hauljoin and b.hauljoin = d.hauljoin
			and b.species_code = e.species_code and b.species_code = d.species_code and d.length != 0 and d.weight != 0
			and a.cruisejoin = c.cruisejoin and c.survey_definition_id != 78 and a.stratum not in (82, 90)
			and a.stratum between 10 and 99"
			# like '%-%' eliminates corner stations which don't have a dash in the stationid
			# survey_definition_id != 78 eliminates Bering Slope stations
			# decode to combine strata  and stratum range between 10 and 99 per original intent
			# stratum not in (82, 90) because these are non-standard strata and were excluded a priori
          }

     lwdata <- sqlQuery(channel = racebase, query = sqry, rows_at_time = 1)

     if(close.channel)close(racebase)

     lwdata

     }
