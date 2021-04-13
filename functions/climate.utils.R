# 
# create.clim.files.test <- function(general_params) {
#   # create a test file from Puechabon study site datasets in SUREAU_ECOS
#   data <- read.csv(file = paste0(general_params$model_path, "/tests/Climat_Puechabon_1999-2019.csv"), sep = ";")
#   selec <- data$YEAR %in% seq(general_params$start_year, general_params$end_year)
#   climate_list <- list(
#     Tmoy = data[selec, "MeanTair"],
#     Tmax = data[selec, "MaxTAir"],
#     Tmin = data[selec, "MinTAir"],
#     relative_humidity = data[selec, "HRmean"],
#     RHmin <- data[selec,"
#     
#     
#     ppt = data[selec, "SumPPT"],
#     global_radiation = data[selec, "SumRg"]
#   )
#   climate_list$vpd <- compute.VPDfromRHandT(climate_list$relative_humidity, climate_list$temperature)
# 
#   return(climate_list)
# }
# 

compute.dailyclim.WBclim <- function(climate_list, general_params, year, DOY) {
  # select climate data for the year and doy in SUREAU_ECOS
  output_list <- lapply(climate_list, FUN = function(x) x[year(general_params$time_mod) == year & yday(general_params$time_mod) == DOY])

  output_list$net_radiation <- NA
  output_list$ETP <- NA

  return(output_list)
}
DFMC_dedios <- function(VPD, FM0 = 5.43, FM1 = 52.91, m = 0.64) {

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  # Authors : Julien Ruffault (julien.ruff@gmail.com)
  #                       &
  #           Nicolas MartinStPaul (nicolas.martin@inrae.fr)
  # Date    : 30/03/2020
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # compute FMC for dead fuel following Resco De Dios et al. (2015), AFM ,https://doi.org/10.1016/j.agrformet.2015.01.002
  # INPUTS
  #   -VPD : vapor pressure deficit (kPA)
  #   -FM0 = 5.43 #(minimim FM, %dry weight) -           --> see Resco De Dios et al. (2015)
  #   -FM1 = 52.91 # (FM0+FM1 : maximum FM, % dryweight) --> see Resco De Dios et al. (2015)
  #   -m   = 0.64 #(rate of decay)
  # OUTPUTS
  #   - FM : fuel moisture (% dry weight)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  FM <- FM0 + FM1 * exp(-m * VPD)

  return(FM)
}
computeVPDfromRHandT <- function(relative_humidity, temperature, air_pressure = 101325) {
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  # AUTHORS     :  Julien Ruffault (julien.ruff@gmail.com)
  #                Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
  # DATE        : 10/06/2020
  # DESCRIPTION :  calculate VPD from Relative humidity and Temperature s
  # INPUTS
  #   -relative_humidity  :  relative humidity (%)
  #   -temperature        :  air temperature (degC)
  #   -air_pressure       :  surface pressure (Pa)
  # OUPUTS
  #   - VPD               :  vapor pressure deficit (kPa)
  #   ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  # Constants
  Mas <- 28.966 # molar weight dry air (g/mol)
  Mh2o <- 18 # molar weight H20 H2O(g/mol)
  Rgz <- 8.314472 # pPerfect gaz constant %J/mol/K

  Tk <- temperature + 273.15 # conversion of temperature in K
  Dair <- ((air_pressure) / (Rgz * (Tk))) * Mas
  es <- 6.108 * exp(17.27 * temperature / (237.2 + temperature)) * 100
  ea <- relative_humidity * es / 100
  vpd <- (es - ea) / 1000
  vpd[vpd < 0] <- 0
  return(vpd)
}

ETP.PT <- function(Tmoy, NetRadiation, PTcoeff = 1.26, G = 0) {
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

#  Returns the proportion of daily radiation corresponding to the input time 
#  Adapted from the code of M de Caceres, MEDFATE
# (https://github.com/cran/medfate/blob/master/src/biophysicsutils.cpp)
#  
#  t : time of the day (in seconds from sunrise)
#  daylength : duration of the day (in seconds)
#  
#  B. Y. H. Liu and R. C. Jordan, “The interrelationship and characteristic distribution of direct, diffuse and total solar radiation,” 
# * Solar Energy, vol. 4, no. 3, pp. 1–19, 1960. 

radiationDiurnalPattern <- function(timeoftheday, daylength) {
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

temperatureDiurnalPattern <- function(timeoftheday, tmin, tmax, tminPrev, tmaxPrev, tminNext, daylength) {
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
rhDiurnalPattern <- function(temperature, rhmin, rhmax, tmin, tmax) {
  rh <- rhmax + ((temperature - tmin) / (tmax - tmin)) * (rhmin - rhmax)
  return(rh)
}


Tleaf <- function(Tair, SWR, WS, VPD, RH, gs, Einst, leaf_size=50, leaf_angle=45) 
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
# 
#  #Pour test de la fonction Tleaf
#   #computeVPDfromRHandT(30,35)
# count=0
# A=NULL
# for (PP in  seq(0,0.5,0.01))
# {
# count=count+1
# A[count]   = Tleaf(Tair=35,SWR=800,WS=1,VPD=3.94,RH=30,leaf_size=36,leaf_angle=20,Einst=PP)
# }
# plot(seq(0,0.5,0.01),A-35,xlab ='E (mol/m2/s)',ylab='deltaT (degC)')
# mtext(side=3,'Tair=35,SWR=800,WS=1,VPD=3.94,RH=30,leaf_size=36,leaf_angle=20',font=2)
# 
# 
# 


#Calculates leaf temperature
# Campbell & Norman 1998 (eqns. 14.1 & 14.3)
#airTemperature - Air temperature (in ºC)
# absRad - Absorbed long- and short-wave radiation (in W*m^-2)
# E - Transpiration flow (in mmol H20 * m^-2 * s^-1) one sided leaf area basis
# leafWidth - Leaf width (here in cm)
# u - wind speed above the leaf boundary layer (in m/s)
#/
#  
# Tleafbis <- function(absRad,airTemperature,u,E,leafWidth=1) # Caceres et al/ Medfate
# {
#  u=max(u,0.1) # force minimum wind speed to avoid excessive heating 
#  gHa = 0.189*pow(u/(leafWidth*0.0072), 0.5)
#  
#  
#  }
# 
# double leafTemperature(double absRad, double airTemperatur,e, double u, double E, double leafWidth = 1.0) {
#   double lambda = meteoland::utils_latentHeatVaporisationMol(airTemperature);
#   u = std::max(u, 0.1);//Force minimum wind speed to avoid excessive heating
#   double gHa = 0.189*pow(u/(leafWidth*0.0072), 0.5);
#   double gr = 4.0*0.97*SIGMA_W*pow(273.16+airTemperature,3.0)/Cp_Jmol;
#   double deltaTemp = (absRad- (0.97*SIGMA_W*pow(273.16+airTemperature,4.0)) - (lambda*(E/2000.0)))/(Cp_Jmol*(gr+gHa));
#   return(airTemperature+deltaTemp);
# }
# 
# 
# 


PPFD_umol.to.Rg_Watt <- function (PPFD, J_to_mol = 4.6, frac_PAR = 0.5) 
{
  Rg <- PPFD/frac_PAR/J_to_mol
  return(Rg)
}


Rg_Watt.to.PPFD_umol <- function (Rg, J_to_mol = 4.6, frac_PAR = 0.5) 
{
  PPFD <- Rg * frac_PAR * J_to_mol
  return(PPFD)
}

Rg_Watt.to.Rg_MJday <-  function(Rg)
{
RgMJ = Rg*0.0864
}
  
Rg_MJday.to.RgWatt <-  function(Rg)
{
  RgWatt = Rg*(1/0.0864)
}







# 
# 
