
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#                       ,
#           Julien Ruffault (julien.ruff@gmail.com)
#                       &
#           Francois Pimont (francois.pimont@inrae.fr)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# V2 (21/12/2020) // JR modified the following :
#  - fix a bug and clean the code for WBclim_hour / (!deep changes in code!)
# V3 (13/01/2021) //JR modified the following
#   - change WBclim_hour to use 'timestep' for EVApo lower than 1 hour (// should theoretically use very small time steps too)
# V4 (25/01/2021) // JR and NM add options for constant climate and PAR instead of RG (under progress)
# // Note faire une fonction pour calculer gBL a ajouter leafandSoilFunctions (JR, 25/01/2021)


readClimateData <- function(general_params) {

  #  read input climate data and put it in right format for SUREAU
  # # Also check data consistensity and input variables according to model options in general params
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
  # variables and format of the climate data file :
  #  - DATE      : 'dd/mm/YY' (only if generalparams$constantClimate==F)
  #  -Tair_min   : minimum air temperature of the day (degC)
  # - Tair_max   : maximum air temperature of the day (degC)
  # - Tair_mean  : mean air temperature of the day (degC)
  # - RG_sum     : global radiation (MJ/m2)
  # - PPT_sum    : precipitation (mm)
  # - RHair_min  : minimum relative humidity of the day (%)
  # - RHAir_max  : maixmum relative humidity of the day (%)
  # - RHair_mean : mean relative humidity of the day (%)
  # - WS_mean    : mean wind speed of the day (m/s)


  # NOTES (JR, 19/12/2020): # et qui peut revnoyer fonctions d'erreur si input pas bon/ corriger ce que ne va pas (eg. RH>100)
  # ou alors Tmin > Tmax (ce genre de  truc) et qui renvoie des warnings
  # si des varaibles manquent ca serai bien de pouvoir les recalculer a partir d'autres
  # vVoir pour chargerr les donnes sites direct ou les donnes SAFRAN
  # Attention dans SAFRAN, y' pas la Rhmin et la RHmax, il faut les recalculer à partir de
  # Voir pour mettre une routine en place pour cela.



  # Read file if it exists, error otherwise
  filePath <- general_params$climate_path

  if (file.exists(paste0("../Clim_data/", filePath))) {
    climate_data <- read.csv(paste0("../Clim_data/", filePath), dec = ",", sep = ";", header = T, stringsAsFactors = F)
    if (general_params$constantClimate == T) {
      climate_data <- as.data.frame(lapply(climate_data, rep, length(general_params$time_mod)))
      climate_data$DATE <- general_params$time_mod
    }
  } else {
    stop(paste0("file : ", paste0("../", filePath), "' does not exist, check presence or spelling"))
  }
  # check that the climate_data contains all necessary columns  and that theyr are nuemeric
  # note : needccol will be adujested according to  modeling options in general params (JR :18/12/2020)




  # Pas besoin du vent si T == Tleaf | emin==F
  if (general_params$calcul_Tleaf == F | general_params$calcul_Emin == F) {
    needcol <- c("Tair_min", "Tair_max", "Tair_mean", "RG_sum", "PPT_sum", "RHair_min", "RHair_max", "RHair_mean")
  }
  else {
    needcol <- c("Tair_min", "Tair_max", "Tair_mean", "RG_sum", "PPT_sum", "RHair_min", "RHair_max", "RHair_mean", "WS_mean")
  }



  for (i in needcol) {
    if (any(names(climate_data) == i)) {
      if (!is.numeric(climate_data[, which(names(climate_data) == i)])) {
        stop(paste0("oups, variable : -", i, "- not numeric in input climate file, please check data or input format"))
      }
    }
    else {
      stop(paste0("varaible : -", i, "- not found in input climate file, please check presence/spelling"))
    }
  }

  # checks NA's
  if (sum(is.na(climate_data)) > 1) {
    stop(paste0("remove NAs in file  : ", paste0("../", filePath), "' "))
  }


  # check and format  DATES
  if (any(names(climate_data) == "DATE")) {
    if (sum((is.na(as.Date(climate_data$DATE, "%d/%m/%Y")))) > 1) {
      stop("variable DATE is not in the right format in the input climate file, should be as 'dd/mm/yyyy' : e.g. '01/01/1999'")
    }
    else {
      climate_data$Doy <- yday(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Day <- day(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Month <- month(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
      climate_data$Year <- year(as.Date(climate_data$DATE, format = "%d/%m/%Y"))
    }
  } else {
    stop(paste("no variable 'DATE' in the input file, that is a problem !"))
  }


  # check the consistency of climate variables
  # PPT < 0
  if (any(climate_data$PPT_sum < 0)) {
    stop(paste("Precipitation <0 mm in input data file, that is a problem !"))
  }

  # RG < 0
  if (any(climate_data$RG_sum < 0)) {
    stop(paste("Global radiation <0 mm in input data file, that is a problem !"))
  }


  # RH > 100
  if (any(climate_data$RHair_max > 100)) {
    climate_data[climate_data$RHair_max > 100, "RHair_max"] <- 100
  }


  if (any(climate_data$RHair_mean > 100)) {
    climate_data[climate_data$RHair_mean > 100, "RHair_mean"] <- 100
  }

  if (any(climate_data$RHair_min > 100)) {
    climate_data[climate_data$RHair_min > 100, "RHair_min"] <- 100
  }


  # others....


  # select the studied period in climate data / only entire years ! from 01/01/XX to 31/12/XX
  warning("JR (19/12/2020)  : add code to check time periods ")



  climate_data <- climate_data[climate_data$Year >= general_params$start_year & climate_data$Year <= general_params$end_year, ]




  return(climate_data)
}

new_WBclim <- function(climate_data, YEAR, DOY, general_params) {
  WBclim <- list()

  WBclim$DOY <- DOY
  WBclim$YEAR <- YEAR

  index <- which(climate_data$Doy == DOY & climate_data$Year == YEAR)

  WBclim$Date <- climate_data$DATE[index]

  WBclim$Tair_mean <- climate_data$Tair_mean[index]
  WBclim$Tair_max <- climate_data$Tair_max[index]
  WBclim$Tair_min <- climate_data$Tair_min[index]

  WBclim$RHair_mean <- climate_data$RHair_mean[index]
  WBclim$RHair_max <- climate_data$RHair_max[index]
  WBclim$RHair_min <- climate_data$RHair_min[index]

  WBclim$PPT <- climate_data$PPT_sum[index]
  WBclim$RG <- climate_data$RG_sum[index]

  WBclim$net_radiation <- NA
  WBclim$ETP <- NA

  WBclim$WS_mean <- climate_data$WS_mean[index]
  WBclim$VPD <- computeVPDfromRHandT(
    relative_humidity = WBclim$RHair_mean,
    temperature = WBclim$Tair_mean
  )

  if (!length(general_params$TIME) == 1) {
    if (index != 1 & index != nrow(climate_data)) { # cas normal
      WBclim$Tair_min_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) - 1, "Tair_min"]
      WBclim$Tair_max_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) - 1, "Tair_max"]
      WBclim$Tair_min_next <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) + 1, "Tair_min"]
    } else if (index == 1) { # si premier jour de le la simu
      WBclim$Tair_min_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY), "Tair_min"]
      WBclim$Tair_max_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY), "Tair_max"]
      WBclim$Tair_min_next <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) + 1, "Tair_min"]
    } else if (index == nrow(climate_data)) { # si dernier jour de la simu
      WBclim$Tair_min_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) - 1, "Tair_min"]
      WBclim$Tair_max_prev <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY) - 1, "Tair_max"]
      WBclim$Tair_min_next <- climate_data[which(climate_data$Year == YEAR & climate_data$Doy == DOY), "Tair_min"]
    }
  }


  return(WBclim)
}

new_WBclimHour <- function(WBclim, WBveg, general_params, TIME, lat, lon, PTcoeff) {

  
  WBclimHour <- list()

  if (general_params$constantClimate == F) {
    # calculate sunrise, sunset and daylength (in seconds from midgnight) depends of DAY, latt and lon
    sunrise_sunset_daylen <- as.numeric(daylength(lat = lat, long = lon, jd = WBclim$DOY, 0)) * 3600 #
  } else {
    sunrise_sunset_daylen <- as.numeric(daylength(lat = 0, long = 0, jd = 116, 0)) * 3600
  } # 6h-18h

  # desaggregation at the hourly time step whatever the 'final resolution'timeStep' and then converted by summing/averaging at the final timestep
  TIME_HOUR <- seq(1, 24, 1)
  timeRelativeToSunset <- TIME_HOUR * 3600 - sunrise_sunset_daylen[1] # time relative to sunset (in seconds)

  # Radiation  --------------------------------------------------------------
  io <- sapply(timeRelativeToSunset, FUN = function(x) radiationDiurnalPattern(timeoftheday = x, daylength = sunrise_sunset_daylen[3]))
  io [timeRelativeToSunset < 0 | TIME_HOUR * 3600 > sunrise_sunset_daylen[2]] <- 0

  WBclimHour$RG <- WBclim$RG * io * 3600
  WBclimHour$Rn <- WBclim$net_radiation * io * 3600
  PAR_cumul <- 24 * Rg_Watt.to.PPFD_umol(Rg_MJday.to.RgWatt(WBclim$RG))
  WBclimHour$PAR <- PAR_cumul * io * 3600

  # Air temperature ---------------------------------------------------------
  WBclimHour$Tair_mean <- sapply(timeRelativeToSunset, FUN = function(x) {
    temperatureDiurnalPattern(
      timeoftheday = x,
      tmin = WBclim$Tair_min,
      tmax = WBclim$Tair_max,
      tminPrev = WBclim$Tair_min_prev,
      tmaxPrev = WBclim$Tair_max_prev,
      tminNext = WBclim$Tair_min_next,
      daylength = sunrise_sunset_daylen[3]
    )
  })
  # air relative humidity ---------------------------------------------------
  WBclimHour$RHair_mean <- sapply(WBclimHour$Tair_mean, FUN = function(x) {
    rhDiurnalPattern(
      temperature = x,
      tmin = WBclim$Tair_min,
      tmax = WBclim$Tair_max,
      rhmin = WBclim$RHair_min,
      rhmax = WBclim$RHair_max
    )
  })
  # VPD -----------------------------------------------------------------------
  WBclimHour$VPD <- computeVPDfromRHandT(relative_humidity = WBclimHour$RHair_mean, temperature = WBclimHour$Tair_mean)
  # ETP -----------------------------------------------------------------------
  WBclimHour$ETP <- ETP.PT(Tmoy = WBclimHour$Tair_mean, NetRadiation = WBclimHour$Rn, PTcoeff = PTcoeff)
  # Wind speed -----------------------------------------------------------------
  WBclimHour$WS <- rep(WBclim$WS_mean, each = 24) # no time interpolation for now

  WBclimHour$TIME <- TIME
  WBclimHour$nHours <- round(as.numeric(base::diff(c(0, as.numeric(WBclimHour$TIME)))), digit = 2)

  if (length(TIME) < 24) {
    WBclimHour$RG <- WBclimHour$RG[TIME]
    WBclimHour$Rn <- WBclimHour$Rn[TIME]
    WBclimHour$PAR <- WBclimHour$PAR[TIME]
    WBclimHour$ETP <- WBclimHour$ETP[TIME]
    WBclimHour$Tair_mean <- WBclimHour$Tair_mean[TIME]
    WBclimHour$RHair_mean <-WBclimHour$RHair_mean[TIME]
    WBclimHour$VPD <- WBclimHour$VPD[TIME]
    WBclimHour$WS <- WBclimHour$WS[TIME]
  }
  WBclimHour$TIME[which(WBclimHour$TIME == 24)] <- 23.99
  return(WBclimHour)
}

computeRnAndETP.WBclim <- function(WBclim, WBveg, Rn.formulation = "Linacre", ETP.formulation = "PT") {

  # calculatate net radiation
  if (general_params$Rn.formulation == "Linacre") {
    if (is.null(WBclim$nN)) # si sunshine duration is not provided ,
      {
        nN <- numeric(1)
        if (WBclim$PPT > 0) {
          nN <- 0.25
        } #  N=0.25 is pluie, nN=0.75 si pas de pluie
        else {
          nN <- 0.75
        }
      }

    WBclim$net_radiation <- pmax(0, 1e-6 * ((1 - 0.17) * 1e6 * WBclim$RG - 1927.987 * (1 + 4 * nN) * (100 - WBclim$Tair_mean)))
  }
  else if (general_params$Rn.formulation == "Linear") {
    WBclim$net_radiation <- WBveg$arad * WBclim$RG + WBveg$brad
  }
  # calculate ETP
  if (general_params$ETP.formulation == "PT") {
    WBclim$ETP <- ETP.PT(
      Tmoy = WBclim$Tair_mean,
      NetRadiation = WBclim$net_radiation,
      PTcoeff = WBveg$params$PTcoeff
    )
  }

  if (general_params$ETP.formulation == "PM") {
    stop("Pennman formulation for ETP not implemented yet", call. = T)
  }

  return(WBclim)
}
