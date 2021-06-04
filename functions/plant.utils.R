# Several plant functions used for SurEau-Ecos




# compute Rs from pmin (resolution from Bartlet et al 2012 EcolLett and email Herve Cochard 19/06/2015)
Rs.Comp <- function(PiFT, Esymp, Pmin) {
  A <- max((-1 * (Pmin + PiFT - Esymp) - sqrt((Pmin + PiFT - Esymp)^2 + 4 * (Pmin * Esymp))) / (2 * Esymp), 1 - PiFT / Pmin)
  return(A)
}



# Turgor pressure
turgor.comp <- function(PiFT, Esymp, Rstemp) {
  A <- -PiFT - Esymp * Rstemp
  return(A)
}

# Osmotic potential
osmo.comp <- function(PiFT, Rstemp) {
  A <- PiFT / (1 - Rstemp)
  return(A)
}

# Total potential
psiTotalSymp.comp <- function(PiFT, Esymp, Rstemp) {
  A <- Comp.Pturg(PiFT = PiFT, Esymp = Esymp, Rstemp = Rstemp) + Comp.Osmo(PiFT = PiFT, Rstemp = Rstemp)
  return(A)
}

regulfactTurgor.comp <- function(Tpress = Tpress, TpRESSUREMax = TpRESSUREMax) {
    # This function computes stomatal regulation if stomatal closure is limited (linearly) by turgor pressure
    # It returns regulfact a dimensionless factor that 

    # Parameters:
    # Emax
    # Tpress: turgor pressure
    # TpRESSUREMax: : Maximum turgor pressure
    # Ecuti: residual transpiration
    Tr <- min(Emax * Tpress / TpRESSUREMax, Emax)
    return(c(max(Tr, 0), min(max(max(Tr, 0) + Ecuti, 0 + Ecuti), Emax)))
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



#' calculate minimum conductance (gmin) following Cochard et al. (2019)
#'
#' @param leafTemperature Temperature of the leaf (degC)
#' @param gmin_20   leaf conductance at 20 degC
#' @param TPhase  Temperature for phase transition of gmin
#' @param Q10_1 Q10 values for gmin= f(T) below Tphase
#' @param Q10_2 Q10 values for gcuti = f(T) above Tphase
#'
#' @return
#' @export
#'
#' @examples
calcul.gmin <- function(leafTemperature, gmin_20,TPhase, Q10_1, Q10_2) {
  if (leafTemperature<= TPhase) {
    gmin <- gmin_20 *Q10_1^((leafTemperature - 20) / 10)
  } else if (leafTemperature> TPhase) {
    gmin <- gmin_20 *Q10_1^((TPhase - 20) / 10) * Q10_2^((leafTemperature- TPhase) / 10)
  }
  return(gmin)
}

#' calculate minimum transpiration (mmol/m2/s from gmin and vod 
#'
#' @param gmin  minimum conductance 
#' @param VPD vapor pressure deficit  (kPa)
#' @param airPressure surface air pressure (kPa) (default = 101.3) 
#' @export
#'
#' @examples
calcul.Emin <- function(gmin,VPD,airPressure =101.3) {
  return(gmin * VPD /airPressure) 
}

#' compute dead fuel moisture content from VPD following De Dios et al. (2015)
#' <https://doi.org/10.1016/j.agrformet.2015.01.002>
#'
#' @param VPD Vapor pressure deficit (kPA) 
#' @param FM0 minimum fuel moisture content (% dry weight)
#' @param FM1 maximum fuel moisture content (% dry weight)
#' @param m rate of decay 
#' @return
#' fuel moisture content (% dry weight)
#' @examples
compute.DFMC <- function(VPD, FM0 = 5.43, FM1 = 52.91, m = 0.64) {
  FM <- FM0 + FM1 * exp(-m * VPD)
  return(FM)
}



#' calcultate hydraulic conductances in the different portions of the plant 
#' (trunk, leaf and root) according to predetermined rules 
#' @param kPlantInit conductance of the plant from root to leaf 
#' @param ri root distribution within the soil layers.
#'
#' @return
#' @export
#'
#' @examples
distribute.conductances <- function(kPlantInit,ri)
{
  k_TLInit = 1/(0.2/kPlantInit)
  k_LSymInit = 1/(0.4/kPlantInit)
  k_RTInit   = 1 /( 0.4 / kPlantInit) *ri
  return(list( k_TLInit =  k_TLInit, k_LSymInit = k_LSymInit ,  k_RTInit=k_RTInit))
}


compute.gCrown <- function(gCrown0, windSpeed){
  windSpeed=  max(0.1, windSpeed) # to avoid very high conductance values 
  return(gCrown0*windSpeed^0.6)
  }





calculate.Ebound_mm.Granier <- function(ETP, LAI, a = -0.006, b = 0.134, c = 0) {
  ET_Granier <- pmax(0, ETP * (a * LAI^2 + b * LAI + c))
  return(ET_Granier)
}







#'convert flux in L.m-2soil to an instantaneous flux in mmol/m-2leaf.s-1 
#'over a defined time period 
#'
#' @param x the amount of water in mm (L.m-2soil)
#' @param timeStep time step (in hours)
#' @param LAI Leaf area index of the stand (m2leaf.m-2soil)
#'
#' @return
#' @export
#'
#' @examples
ConvertFluxFrom_mm_To_mmolm2s <- function(x, timeStep, LAI) {
  if (LAI > 0) {
    y <- 10^6 * x / (LAI * timeStep * 3600 * 18)
  } else if (LAI == 0) {
    y <- 0
  }
  return(y)
}

#' Convert an instantaneous flux in mmol.m-2Leaf.s-1 to a amount in 
#' mm (L.m2soil) over a defined time period 
#'
#' @param x The instaneous flux (mmol.m-2leaf.s-1) to be converted. Expressed per
#' m2 of leaf 
#' @param timeStep timeStep (in hours) 
#' @param LAI leaf area index of the stand 
#'
#' @return the flux over the considered time period in mm (L.m-2 Soil
#' @export
#'
#' @examples
convertFluxFrom_mmolm2s_To_mm <- function(x, timeStep, LAI=1) {
  y <- x * (LAI * timeStep * 3600 * 18) / 10^6
  return(y)
}

# Function to sum 2 conductances in series
kseriesum<-function(k1,k2) {return(1/(1/k1+1/k2))}


# not used for now
calculate.Ebound.Granier <- function(ETP, LAI, a = -0.006, b = 0.134, c = 0) {
  return(pmax(0, ETP * (a * LAI^2 + b * LAI + c)))
}



#' Convert soil parameter from from cm3.cm-3 to mm according to thickness and
#' rock fragment content 
#' @param x the soil value to be converted (in m3.m-3)
#' @param rock_fragment_content rock fragment content of the soil layer (%)
#' @param layer_thickness thickness of the soil layer (in m)
#'
#' @return y soil parameter in mm 
#' @export
#'
#' @examples
convert.FtoV <- function(x, rock_fragment_content = 0, layer_thickness) {
  y <- x * (1 - (rock_fragment_content / 100)) * layer_thickness * 1000
  return(y)
}



