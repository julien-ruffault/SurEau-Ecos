
# create a list with all necessary daily climate values to run SureauR from climate_data 
new.WBclim <- function(climate_data, YEAR, DOY) {
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
  WBclim$RG  <- climate_data$RG_sum[index]

  WBclim$net_radiation <- NA
  WBclim$ETP <- NA

  WBclim$WS_mean <- climate_data$WS_mean[index]
  WBclim$VPD <- compute.VPDfromRHandT(
    relative_humidity = WBclim$RHair_mean,
    temperature = WBclim$Tair_mean
  )

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
  return(WBclim)
}

# create a list with interpolated climate data at the required time step 
new.WBclimHour <- function(WBclim, WBveg, modeling_options, lat, lon, PTcoeff) {

  WBclimHour <- list()

  if (modeling_options$constantClimate == F) {
    # calculate sunrise, sunset and daylength (in seconds from midgnight) depends of DAY, latt and lon
    sunrise_sunset_daylen <- as.numeric(daylength(lat = lat, long = lon, jd = WBclim$DOY, 0)) * 3600 #
  } else {
    sunrise_sunset_daylen <- as.numeric(daylength(lat = 0, long = 0, jd = 116, 0)) * 3600
  } # 6h-18h

  # desegregation at the hourly time step
  TIME_HOUR <- seq(0, 23, 1)
  timeRelativeToSunset <- TIME_HOUR * 3600 - sunrise_sunset_daylen[1] # time relative to sunset (in seconds)

  # Radiation  --------------------------------------------------------------
  io <- sapply(timeRelativeToSunset, FUN = function(x) calculate.radiationDiurnalPattern(timeoftheday = x, daylength = sunrise_sunset_daylen[3]))
  io [timeRelativeToSunset < 0 | TIME_HOUR * 3600 > sunrise_sunset_daylen[2]] <- 0

  WBclimHour$RG <- WBclim$RG * io * 3600
  WBclimHour$Rn <- WBclim$net_radiation * io * 3600
  WBclimHour$PAR = Rg_Watt.to.PPFD_umol(Rg_MJ.to.RgWatt(WBclimHour$RG,Nhours=1))
  WBclimHour$Potential_PAR = Potential_PAR(timeOfDay=TIME_HOUR, Lat=lat, DOY=WBclim$DOY) 
  

  #PAR_cumul <- 24 * Rg_Watt.to.PPFD_umol(Rg_MJday.to.RgWatt(WBclim$RG))
  #WBclimHour$PAR <- 10*PAR_cumul * io * 3600 # TODO voir conversion du PAR 

  
  # Air temperature ---------------------------------------------------------
  WBclimHour$Tair_mean <- sapply(timeRelativeToSunset, FUN = function(x) {
    calculate.temperatureDiurnalPattern(
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
    calculate.rhDiurnalPattern(
      temperature = x,
      tmin = WBclim$Tair_min,
      tmax = WBclim$Tair_max+0.0000001, # to prevent crash when tmin = tmax
      rhmin = WBclim$RHair_min,
      rhmax = WBclim$RHair_max+0.000001 # to prevent crash when RHair_min = RHair_max
    )
  })
  
  WBclimHour$RHair_mean[WBclimHour$RHair_mean<0] = 0.5
  # Wind speed -----------------------------------------------------------------
  WBclimHour$WS <- rep(WBclim$WS_mean, each = 24) # no time interpolation for now
  # VPD -----------------------------------------------------------------------
  WBclimHour$VPD <- compute.VPDfromRHandT(relative_humidity = WBclimHour$RHair_mean, temperature = WBclimHour$Tair_mean)
  # ETP -----------------------------------------------------------------------
  if(modeling_options$ETPFormulation=='PT'){
  WBclimHour$ETP <- compute.ETP.PT(Tmoy = WBclimHour$Tair_mean, NetRadiation = WBclimHour$Rn, PTcoeff = PTcoeff)
  } else if (modeling_options$ETPFormulation=='Penman'){
    WBclimHour$ETP <- compute.ETP.PM(Tmoy = WBclimHour$Tair_mean, NetRadiation = WBclimHour$Rn,u =WBclim$WS,vpd=WBclim$VPD)}
  
    

  WBclimHour$TIME <- modeling_options$TIME
  WBclimHour$nHours = c(base::diff(c(WBclimHour$TIME[length(WBclimHour$TIME)],24)),base::diff(as.numeric(WBclimHour$TIME)))
  # nhours for the first time period is equal to midnight minus the last hour of the previous day 

  index= which(TIME_HOUR %in% WBclimHour$TIME )


  if (length(modeling_options$TIME) < 24) {
    WBclimHour$RG <- WBclimHour$RG[index]
    WBclimHour$Rn <- WBclimHour$Rn[index]
    WBclimHour$PAR <- WBclimHour$PAR[index]
    WBclimHour$ETP <- WBclimHour$ETP[index]
    WBclimHour$Tair_mean <- WBclimHour$Tair_mean[index]
    WBclimHour$RHair_mean <-WBclimHour$RHair_mean[index]
    WBclimHour$VPD <- WBclimHour$VPD[index]
    WBclimHour$WS <- WBclimHour$WS[index]
  }
 
  
  # All ptt of the day fall at midgnitht 
  WBclimHour$PPT <-  numeric(length(modeling_options$TIME))
  WBclimHour$PPT[1] <- WBclim$PPT
   #WBclimHour$TIME[which(WBclimHour$TIME == 24)] <- 23.99
  return(WBclimHour)
}

# compute daily PET and Net radiation
compute.RnAndETP.WBclim <- function(WBclim, WBveg, RnFormulation = "Linacre", ETPFormulation = "PT") {

  # calculate net radiation
  if (RnFormulation == "Linacre") {
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
  else if (RnFormulation == "Linear") {
    WBclim$net_radiation <- WBveg$arad * WBclim$RG + WBveg$brad
  }
  # calculate ETP
  if (ETPFormulation == "PT") {
    WBclim$ETP <- compute.ETP.PT(
      Tmoy = WBclim$Tair_mean,
      NetRadiation = WBclim$net_radiation,
      PTcoeff = WBveg$params$PTcoeff
    )
  }

  if (ETPFormulation == "PM") {
    stop("Pennman formulation for ETP not implemented yet", call. = T)
  }

  return(WBclim)
}

#interpolate climate values between two WBclim objects 
interp.WBclim=function(clim1,clim2,p=0.5) {
  res =  clim1
  res$Tair_mean  = (1-p)*clim1$Tair_mean  + p*clim2$Tair_mean
  res$RG         = (1-p)*clim1$RG         + p*clim2$RG
  res$WS         = (1-p)*clim1$WS         + p*clim2$WS
  res$VPD        = (1-p)*clim1$VPD        + p*clim2$VPD
  res$RHair_mean = (1-p)*clim1$RHair_mean + p*clim2$RHair_mean
  res$ETP        = (1-p)*clim1$ETP + p*clim2$ETP
  return(res)
}

