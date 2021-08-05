

#' compute Vapor pressure deficit from air relative humidity and air temperature
#'
#' @param relative_humidity relative humidity of the air (%)
#' @param temperature air temperature (degC)
#' @param air_pressure air pressure (Pa)
#'
#' @return
#' return the vapor pressure deficit in kPa
#' @export
#' @examples
#' compute.VPDfromRHandT(relative_humidity = 80 , temperature = 25)
compute.VPDfromRHandT <- function(relative_humidity, temperature, air_pressure = 101325) {
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  # AUTHORS     :  Julien Ruffault (julien.ruff@gmail.com)
  #                Nicolas Martin-StPaul (nicolas.martin@inrae.fr)

  # Constants
  Mas <- 28.966    # molar weight dry air (g/mol)
  Mh2o <- 18       # molar weight H20 H2O(g/mol)
  Rgz <- 8.314472  # pPerfect gaz constant %J/mol/K

  Tk <- temperature + 273.15 # conversion of temperature in K
  Dair <- ((air_pressure) / (Rgz * (Tk))) * Mas
  es <- 6.108 * exp(17.27 * temperature / (237.2 + temperature)) * 100
  ea <- relative_humidity * es / 100
  vpd <- (es - ea) / 1000
  vpd[vpd < 0] <- 0
  return(vpd)
}

compute.ETP.PT <- function(Tmoy, NetRadiation, PTcoeff = 1.26, G = 0) {
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  # AUTHORS :  Julien Ruffault (julien.ruff@gmail.com)
  #            Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
  # DATE    : 10/06/2020
  # DESCRIPTIOIN :  calcule PET with Pristeley Taylor Formulation
  # INPUTS
  #     - Tmoy          : Mean temperature over the considered time step (degC)
  #     - NetRadiation  : Cumulative Net radiation over the considered  time sep  (MJ.m2)
  # OUTPUTS
  #     - PET = Potential evapotranspiration (mm)
  #   ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  SBconstant <- 4.903 * 10^9 # Stefan-Boltzman constant [MJ.K^-4.m^-2.day^-1]
  gamma <- 0.0666 # Psychometer constant
  lambda <- 2.45 # Latent heat of vaporisation

  #  s: slope of the saturation vapour pressure function (AO 1998)
  s <- 4098 * 0.6108 * exp((17.27 * Tmoy) / (Tmoy + 237.3)) / ((Tmoy + 237.3)^2)

  ETP <- PTcoeff * (s / (s + gamma)) * ((NetRadiation - G) / lambda)

  return(ETP)
}



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#' # Calculated diurnal pattern of temperature assuming a sinusoidal pattern with T = tmin at sunrise
#  and T = (tmin+tmax)/2 at sunset. From sunset to sunrise follows a linear trend 
#'#  Adapted from the code of M de Caceres, MEDFATE
# (https://github.com/cran/medfate/blob/master/src/biophysicsutils.cpp)
# #  B. Y. H. Liu and R. C. Jordan, “The interrelationship and characteristic distribution of direct, diffuse and total solar radiation,” 
# * Solar Energy, vol. 4, no. 3, pp. 1–19, 1960.
#' @param timeoftheday a numeric value of vector indicating the time of the day (in seconds)
#' @param daylength a numeric value indicating the duration of the day (in seconds)
#' @return
#' @export
#' @examples
calculate.radiationDiurnalPattern <- function(timeoftheday, daylength) {
  ws <- (daylength / 3600.0) * (pi / 24.0) # sunrise
  w <- ws - (timeoftheday / daylength) * (ws * 2.0)
  prop <- ((pi / 24.0) * (cos(w) - cos(ws))) / (sin(ws) - ws * cos(ws))
  return(prop / 3600.0)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# #  Adapted from the code of M de Caceres, package medfate
# --> (https://github.com/cran/medfate/blob/master/src/biophysicsutils.cpp)
#  
# Calculated diurnal pattern of temperature assuming a sinusoidal pattern with T = tmin at sunrise
#  and T = (tmin+tmax)/2 at sunset. From sunset to sunrise follows a linear trend 
#  
#  timeofday - time of the day (in seconds from sunrise)
#  daylength - duration of the day (in seconds)
#
#   McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990. 
#   Modelling the yield of Pinus radiata on a site limited by water and nitrogen. 
#   Forest Ecology and Management 30:381–413.
calculate.temperatureDiurnalPattern <- function(timeoftheday, tmin, tmax, tminPrev, tmaxPrev, tminNext, daylength) {
  if ((timeoftheday < 0.0) | (timeoftheday > daylength)) {
    tfin <- 86400.0 - daylength
    if (timeoftheday < 0.0) {
      timeoftheday <- timeoftheday + 86400.0 - daylength
      temp <- (0.5 * (tmaxPrev + tminPrev) * (1.0 - (timeoftheday / tfin)) + tmin * (timeoftheday / tfin))
    } else {
      timeoftheday <- timeoftheday - daylength
      temp <- (0.5 * (tmax + tmin) * (1.0 - (timeoftheday / tfin)) + tminNext * (timeoftheday / tfin))
    }
  } else {
    ct <- cos(1.5 * pi * timeoftheday / daylength)
    temp <- 0.5 * (tmin + tmax - (tmax - tmin) * ct)
  }
  return(temp)
}

# Calculate diurnal pattern of relative humidity from temperature 
calculate.rhDiurnalPattern <- function(temperature, rhmin, rhmax, tmin, tmax) {
  rh <- rhmax + ((temperature - tmin) / (tmax - tmin)) * (rhmin - rhmax)
  return(rh)
}


compute.Tleaf <- function(Tair, SWR, WS, VPD, RH, gs, Einst, leaf_size=50, leaf_angle=45) 
  {
  # SWR  // short-wave radiation    (W/m2)
  # WS   // windspeed    (m/s)
  # Tair // air temperature (degC)
  # VPD  // Vapor pressure deficit (kPa)
  # leaf_angle // # leaf angle (depuis le plan horizontal : 0-90 deg)
  # leaf_size  // characteristic dimension from vegetation params in mm (1 - 3000 : pine needle - banana leaf)
  WS = max(WS,0.1)  # Force minimum wind speed to avoid excessive heating
  
  aSWR <- 0.5 #  //  absorptance to SWR %
  
  gflat <- 0.00662
  gcyl <- 0.00403 # //  coefficient in rbl equation    m
  jflat <- 0.5 #
  jcyl <- 0.6 # //  coefficient in rbl equation  none
  
  
  em_leaf <- 0.97 #    //emissivity    none
  SB <- 5.6704e-8 #    //  Stefan-Boltzman constant    W m-2 K-4
  p <- 1.292      #   // density of dry air    kg/m3
  Cp <- 1010      #  // heat capacity of dry air    J kg-1 K-1
  y <- 0.066      # //psychrometric constant    kPa K-1
  a <- 0.61121    # //coefficient in esat equation    kPa
  b <- 17.502     # //coefficient in esat equation    none
  z <- 240.97     # //coefficient in esat equation    °C
  
  # VARAIBLE CALCULEES
  # rst  #   // stomatal resistance s m-1 (not needed)
  # esat # //// saturation vapor pressure    kPa
  # ea   #//water vapor pressure of the air    kPa
  # em_air #//air emissivity
  # s   #// slope of esat/T curve    kPa oC-1
  # SWRabs#  // absorbed short-wave radiation    W m-2
  # LWRin  #// incoming long-wave radiation    W m-2
  # LWRouti #  // isothermal outgoing long-wave radiation    W m-2
  # Rni # //  isothermal net radiation    W m-2
  # rr # // radiative resistance    s m-1
  # rblr #  // boundary-layer + radiative resistance    s m-1
  # ym #//  modified psychrometric constant    kPa K-1
  # rbl # // leaf boundary-layer resistance    s m-1
  # Delta_T  #// leaf-to-air temperature difference    degC
  # Tleaf, Tleaf_NonLinear#  //leaf temperature    degC
  
  cloud_cover=1
  
  esat <- a * exp(b * Tair / (Tair + z)) # ; #kPa
  ea <- esat * (RH / 100) # ;
  s <- esat * b * z / ((Tair + z)^2) # ;
  em_air <- ((1 - 0.84 * cloud_cover) * 1.31 * ((10 * ea / (Tair + 273.15))^0.14285714) + 0.84 * cloud_cover) # ;
  
  # Bilan radiatif
  SWRabs <- aSWR * cos(leaf_angle * 3.1416 / 180) * SWR # Radiation absorbed by leaves
  LWRin <- em_air * SB * (Tair + 273.15)^4 # Incoming long-wave radiation (W m-2) for clear and cloudy sky
  LWRouti <- em_leaf * SB * (Tair + 273.15)^4 # Outcoming long-wave radiation (W m-2) for clear and cloudy sky
  Rni <- SWRabs + LWRin - LWRouti # isothermal net radiation
  rr <- p * Cp / (4 * em_leaf * SB * (Tair + 273.15)^3) # Radiative resistance
  
  # Boundary layer resistance
  if (leaf_size > 3) {
    rbl <- 1 / (1.5 * gflat * ((WS^jflat) / ((leaf_size / 1000)^(1 - jflat))))
  } else {
    rbl <- 1 / (1.5 * gcyl * ((WS^jcyl) / ((leaf_size / 1000)^(1 - jcyl)))) # A flat leaf if > 3mm
  } #                    # a needle, formula for a cylinder
  
  g_bl <- 1 / rbl * 1000 * 40 #      #leaf boundary layer conductance in mmol/s/m2
  rblr <- 1 / (1 / rbl + 1 / rr) # 
  
  # If gs is directly computed
  # if (g_s) {
  #   rst <- 1 / g_s * 1000 * 40
  # } # conversion fromm mmol/s/m2 to s/m
  # # Else we used transpiration
  
  if(gs) {rst=1/gs*1000*40} else {rst=9999.99 }

  #gs <- (Einst / VPD * 101.3) 
  
  ym <- y * (rst / rblr) # 
  
  # compute Tleaf with linear approximation
  Delta_T <- (ym * Rni * rblr / (p * Cp) - VPD) / (s + ym) # 
  Tleaf <- Tair + Delta_T # 
  T_Leaf <- Tleaf # 

  return(c(T_Leaf, g_bl))
}


PPFD_umol.to.Rg_Watt <- function (PPFD, J_to_mol = 4.6, frac_PAR = 0.5) 

  {
  Rg <- PPFD/frac_PAR/J_to_mol
  return(Rg)
}



#' converts radiation from W.m-2 to umol/m-2/s-1 .
#'
#' @param Rg Global radiation (W/m2)
#' @param J_to_mol Conversion factor 
#' @param frac_PAR Fanction of solar rdiation that is photosynthetically active radiation (PAR) 
#'
#' @return  	
#'Photosynthetic photon flux density (umol.m-2.s-1)
#' @export
#'
#' @examples
Rg_Watt.to.PPFD_umol <- function (Rg, J_to_mol = 4.6, frac_PAR = 0.5) 
{
  PPFD <- Rg * frac_PAR * J_to_mol
  return(PPFD)
}


# Convert  instantaneous radiation in watt to dialy cumulative radiation in MJ (MJ.day-1)
Rg_Watt.to.Rg_MJday <-  function(Rg)
{
return(RgMJ = Rg*0.0864)
}

  
Rg_MJday.to.RgWatt <-  function(Rg)
{
  return(RgWatt = Rg*(1/0.0864))
}

Rg_MJ.to.RgWatt <- function(Rg,Nhours)
{
  
  return(RgWatt = Rg*(10^6/(Nhours*3600)))
  
}



# determine potential for a given place and date /used to determine cloud cover 
# return potential par in W.m2
Potential_PAR <- function(timeOfDay, Lat, DOY) 
{
  diffuseFraction = 0.1;
  solarConstant = 2084;
  attenuationCoef  = -0.174353387144778;
  
  decl = Declination(DOY);
  pn = -cos(Lat * 3.1416 / 180);
  pz = sin(Lat * 3.1416 / 180);
  hRad = (timeOfDay - 6) * 3.1416/12;
  se = cos(hRad) * cos(decl);
  sn = -pz * sin(hRad) * cos(decl) - pn * sin(decl)
  sz = -pn * sin(hRad) * cos(decl) + pz * sin(decl)
  alt = atan(sz / ((se*se + sn*sn)^0.5))
  azi = 3.1416 + atan(se / sn)
  azi[sn > 0] = azi[sn > 0] + 3.1416
  pfd = solarConstant * exp(attenuationCoef / sin(alt))
  pfd[alt < 0] = 0
  dpfd = diffuseFraction * pfd
  dpfd[alt<0] = 0
  
  return (dpfd + pfd * sin(alt))
}


# calculate declination of sun (radians ? ) for a given julian day (DOY)
Declination <- function(DOY) {
  #Hervé's formula for solar declination
  c1 = 0.398749068925246 #; // =Sin(23.5*pi/180), 23.5 = Earth declination
  c2  = 2 * 3.1416 / 365 #;
  c3  = 80 #; // date of spring
  x = c1 * sin((DOY - c3) * c2) #;
  return(atan(x / ((1 - x*x)^0.5) ))
  
}


