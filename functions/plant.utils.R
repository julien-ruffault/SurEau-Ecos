

# compute Rs from pmin (resolution from Bartlet et al 2012 EcolLett and email Herve Cochard 19/06/2015)
Rs.Comp <- function(PiFT, Esymp, Pmin) {
  A <- max((-1 * (Pmin + PiFT - Esymp) - sqrt((Pmin + PiFT - Esymp)^2 + 4 * (Pmin * Esymp))) / (2 * Esymp), 1 - PiFT / Pmin)
  return(A)
}

# Total Conductance = Soil Conductance & Plant COnductance
PLC.comp <- function(Pmin, slope , P50) {
  PLC  = 100 / (1 + exp(slope / 25 * (Pmin - P50)))
  return(PLC)
}

# This function computes PLCPrime from PLC current value
PLCPrime.comp <- function(PLC , slope) {
  return(- slope/25 * PLC/100 * (1 - PLC/100))
}


calcul.gmin <- function(temperatureLeaf, gmin_20,TPhase, Q10_1, Q10_2) {
  
  # Martin-StPaul N, Ruffault J, Pimont F
  # calculate minimum conductance from formulae by  Cochard (2019)
  # ATTENTION : VOIR POUR MUTLIPER gminn par 2 en fonction de la signifcationn de gmin_20 (Par LAI (surface projeté), ou par surface totale (les deux faces des feuilles)
  #warning("Indiquer les unités")
  #warning("VOIR POUR MUTLIPER gmin par 2 en fonction de la signifcationn de gmin_20")
  # Tleaf_A  / Tleaf tmeparature (in degC)
  # TP       / Temperature for phase transition of gmin
  # Q10_1    /  Q10 values for gcuti = f(T) below Tphase
  # Q10_2    / Q10 values for gcuti = f(T) below  Tphase
  
  if (temperatureLeaf<= TPhase) {
    gmin <- gmin_20 *Q10_1^((temperatureLeaf - 20) / 10)
  } else if (temperatureLeaf> TPhase) {
    gmin <- gmin_20 *Q10_1^((TPhase - 20) / 10) * Q10_2^((temperatureLeaf- TPhase) / 10)
  }
  return(gmin)
}

calcul.Emin <- function(gmin,vpd, air_temperature, relative_humidity, air_pressure) {
  # calculate minimal 
  # if vpd is not provided it is calculated from Air_Temperature and Relative humidity
  # Air_Temperature              : Air temperature (degC)
  # Relative_Humidity            : Relative humidity of the air (%)
  # Air_Pressure (optionnal)     : Air_pressure (Pa)
  if (is.missing(vpd)) {
    if (missing(air_pressure)) {
      air_presssure <- 101325
    }
    vpd <- compute.VPDfromRHandT(relative_humidity = relative_humidity, temperature = Air_temperature, air_pressure = air_pressure)
  }
  # calculate E_min
  E_min <- g_min * vpd / 100 # mmol/m2/s
  
  return(E_min)
}

compute.DFMC <- function(VPD, FM0 = 5.43, FM1 = 52.91, m = 0.64) {
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  # Authors : Julien Ruffault (julien.ruff@gmail.co&
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