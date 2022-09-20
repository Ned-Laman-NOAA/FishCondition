meanLength <- function(Rg = "GOA", sp.code = 21720){

     # libraries
     library(RODBC)
     library(xlsx)
     # get data
     channel <- odbcConnect(dsn = "afsc", uid = "lamane", pwd = "saxicola_30490", believeNRows = F)
     sqry <- paste0("select floor(a.cruise/100) year, sex, length, frequency, inpfc_area from racebase.length a, racebase.haul b,
                    goa.goa_strata c where a.region = '", Rg, "' and
                    a.cruise >= 199701 and a.species_code = ", sp.code, " and a.hauljoin = b.hauljoin
                    and b.stratum = c.stratum and b.region = c.survey")
     lfdata <- sqlQuery(channel = channel, query = sqry, rows_at_time = 1)
     names(lfdata) <- casefold(names(lfdata))
     close(channel)
     # expand data to raw lengths for total mean and variance by year
     survey.year <- rep(lfdata$year, lfdata$frequency)
     raw.length <- rep(lfdata$length, lfdata$frequency)
     mean.length.yr <- aggregate(raw.length, by = list(survey.year), mean)
     var.length.yr <- aggregate(raw.length, by = list(survey.year), var)
     # get INPFC area means and variances
     inpfc.area.vec <- rep(lfdata$inpfc_area, lfdata$frequency)
     area.mean.yr <- aggregate(raw.length, by = list(survey.year, inpfc.area.vec), mean)
     area.var.yr <- aggregate(raw.length, by = list(survey.year, inpfc.area.vec), var)
     # create outgoing data frames for writing
     total.mean.yr <- data.frame(cbind(mean.length.yr, sqrt(var.length.yr$x)))
     names(total.mean.yr) <- c("Mean Fork Length (mm)", "Standard Deviation")
     mean.area.yr <- data.frame(cbind(area.mean.yr, sqrt(area.var.yr$x)))
     names(mean.area.yr) <- c("Cruise Year", "INPFC Area", "Mean Fork Length (mm)", "Standard Deviation")
     write.xlsx(total.mean.yr, paste0(getwd(), "/SPP", sp.code, "meanlength.xlsx"), sheetName = "LongTermMean",
          row.names = FALSE)
     write.xlsx(mean.area.yr, paste0(getwd(), "/SPP", sp.code, "meanlength.xlsx"), sheetName = "AreaYearMean",
                                      row.names = FALSE, append = TRUE)
     }
