# GOA groundfish condition factor
# Sean Rohan <sean.rohan@noaa.gov>
# Last update: August 13, 2020

library(RODBC)
library(getPass)
library(tidyverse)
library(akfishcondition)

biomass.area.qry <- "select a.year, a.species_code, a.haul_count, a.catch_count, a.area_biomass, a.biomass_var, b.inpfc_area inpfc_stratum, b.inpfc_stratum_area
from goa.biomass_inpfc a, (select inpfc_area, summary_area, sum(area) inpfc_Stratum_area from goa.goa_strata where nvl(stratum,1) != 0
and survey = 'GOA' group by inpfc_area, summary_area) b where (a.year >= 1984 and a.year != 1989) and a.species_code in
(21740,21741,21720,30420,10262,10110,30060,30152) and a.summary_Area = b.summary_area order by a.year, a.species_code, b.inpfc_Area"

source("./R/get.lwdata.R")

get.connected <- function(schema='AFSC'){(echo=FALSE)
  username <- getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(paste(schema),paste(username),paste(password), believeNRows=FALSE)
}
# Execute the connection
channel <- get.connected()

stratum_biomass_df <- RODBC::sqlQuery(channel, 
                                      biomass.area.qry)

# Get length-weight data ----
lenwt_df <- get.lwdata(region = "GOA")

# Combine with stratum biomass data
lenwt_df <- lenwt_df %>% 
  dplyr::inner_join(stratum_biomass_df)

# Check that biomass is unique for INPFC_AREAS
# dplyr::select(lenwt_df, INPFC_STRATUM, YEAR, AREA_BIOMASS, COMMON_NAME) %>% unique()

# Get unique species codes
unique_spp <- unique(lenwt_df$SPECIES_CODE)


# Calculate length weight residuals
for(i in 1:length(unique_spp)) {
  lenwt_df$resid[lenwt_df$SPECIES_CODE == unique_spp[i]] <- 
    akfishcondition::calc_lw_residuals(len = lenwt_df$LENGTH[lenwt_df$SPECIES_CODE == unique_spp[i]], 
                                       wt = lenwt_df$WEIGHT[lenwt_df$SPECIES_CODE == unique_spp[i]], 
                                       year = lenwt_df$YEAR[lenwt_df$SPECIES_CODE == unique_spp[i]],
                                       stratum = lenwt_df$INPFC_STRATUM[lenwt_df$SPECIES_CODE == unique_spp[i]],
                                       make_diagnostics = TRUE,
                                       region = "GOA",
                                       species_code = lenwt_df$SPECIES_CODE[lenwt_df$SPECIES_CODE == unique_spp[i]])
  
  lenwt_df$weighted_resid[lenwt_df$SPECIES_CODE == unique_spp[i]] <- 
    akfishcondition::weight_lw_residuals(residuals = lenwt_df$resid[lenwt_df$SPECIES_CODE == unique_spp[i]], 
                                         year = lenwt_df$YEAR[lenwt_df$SPECIES_CODE == unique_spp[i]], 
                                         stratum = lenwt_df$INPFC_STRATUM[lenwt_df$SPECIES_CODE == unique_spp[i]], 
                                         stratum_biomass = lenwt_df$AREA_BIOMASS[lenwt_df$SPECIES_CODE == unique_spp[i]])
}

ann_mean_resid_df <- lenwt_df %>% 
  dplyr::group_by(SPECIES_CODE, COMMON_NAME, YEAR) %>%
  dplyr::summarise(mean_wt_resid = mean(weighted_resid))

ggplot(data = ann_mean_resid_df, 
       aes(x = YEAR, y = mean_wt_resid, color = COMMON_NAME)) + 
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~COMMON_NAME, ncol = 2, scales = "free_y")
