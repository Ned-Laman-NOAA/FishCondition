#' Length Weight Data

#' This is a data set of extracted specimen data from RACEBASE 
#' The example data set included in this package is called lwdata 
#' and is specimen data (length and weight specimens) for all 
#' species where this data was collected on RACE bottom trawl 
#' surveys. The data was initially extracted from RACEBASE 
#' tables (including HAUL, SPECIMEN, SPECIES and STRATA tables). 
#' 
#' @format A data frame with 4475 rows and 12 variables:
#' \describe{
#'   \item{REGION}{Trawl survey region; GOA, BS, AI}
#'   \item{CRUISE}{Cruise identifier}
#'   \item{VESSEL}{Fishing vessel identifier}
#'   \item{HAUL}{Haul number}
#'   \item{START_TIME}{Date and time of haul}
#'   \item{INPFC_AREA}{INPFC area identifier}
#'   \item{STRATUM}{Strata where haul was conducted}
#'   \item{STATIONID}{Unique station idenfier}
#'   \item{BOTTOM_DEPTH}{Mean depth of tow}
#'   \item{START_LATITUDE}{Latitude of haul start}
#'   \item{START_LONGITUDE}{Longitude of haul start}
#'   \item{SPECIMENID}{Specimen number}
#'   \item{SPECIES_CODE}{Species identifier}
#'   \item{SPECIES_NAME}{Latin name of species}
#'   \item{COMMON_NAME}{Common name of species}
#'   \item{SEX}{Sex of specimen; male = 1, female = 2, unknown = 3}
#'   \item{LENGTH}{Specimen length in mm}
#'   \item{WEIGHT}{Specimen weigth in g}
#'   \item{CATCH}{Catch-per-unit-effort in kg/ha}
#'   ...
#' }
#' @source {RACEBASE}
"lwdata"