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


#This Function computes Turgor pressure from PV curves parameters and water potential
computeTurgorFromPsi <- function(PiFT, Esymp, Psi) {
  #This Function computes Turgor pressure from PV curves parameters and water potential
  # PiFT = Osmotic potential at full turgor (MPa)
  # Esymp = Modulus of elastoicoty of the Symplasm (MPa/%)
  # Psi = Water potential of the organ (MPa)
  
  #Compute symplasm relative water deficit (Rs)
  Rs = rep(NA, length(Psi))
  Rs1 = (-1 * (Psi + PiFT - Esymp) - sqrt((Psi + PiFT - Esymp)^2 + 4 * (Psi * Esymp))) / (2 * Esymp)
  Rs2 =  (1 - PiFT / Psi)
  
  Rs[Rs1>Rs2] = Rs1[Rs1>Rs2]
  Rs[Rs1<Rs2] = Rs2[Rs1<Rs2]
  
  #Compute Turgor 
  Turgor <- ( -PiFT - Esymp * Rs)
  Turgor[Turgor<0]<-0
  #NB: read Tyree paper about negative turgor pressure the old and the recent one
  
  return(Turgor)
}


# Osmotic potential
osmo.comp <- function(PiFT, Rstemp) {
  A <- PiFT / (1 - Rstemp)
  return(A)
}

# Total potential
psiTotalSymp.comp <- function(PiFT, Esymp, Rstemp) {
  A <- turgor.comp(PiFT = PiFT, Esymp = Esymp, Rstemp = Rstemp) + osmo.comp(PiFT = PiFT, Rstemp = Rstemp)
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


#VCCURVE Same function as above for plotting some diagnostic
VCCurve <- function(x, slope , P50) {
  PLC  = 100 / (1 + exp(slope / 25 * (x - P50)))
  return(PLC)
}

GsCurve <- function(x, slope_gs, P50_gs, PsiStartClosing, PsiClose, PiFT, Esymp, turgorPressureAtGsMax, gsmax, stomatalRegFormulation=NULL) {
#To obtain plots of the gs regulation curve 
  
  if(stomatalRegFormulation=="Sigmoid") {
    PL_gs <- 1 / (1 + exp(slope_gs / 25 * (x - P50_gs)))
    regulFact <- (1 - PL_gs)
    }
  
   
  if(stomatalRegFormulation=="PiecewiseLinear") {
    regulFact <- (x - PsiClose) / (PsiStartClosing - PsiClose)
    regulFact[regulFact<0] <- 0
    regulFact[regulFact>1] <- 1
  
  }
   
   if(stomatalRegFormulation=="Turgor") {
     #Only Rs1 is needed above TLP.
     rs1 <- (-1 * (x + PiFT - Esymp) - sqrt((x + PiFT - Esymp)^2 + 4 * (x * Esymp))) / (2 * Esymp)
     tlp = (PiFT * Esymp)/(PiFT + Esymp)
     # rs2 <- 1 - PiFT / x
     # rs1[rs1<rs2] = rs2
     # turgor <- -PiFT - Esymp * rs1
     # rs <- Rs.Comp(PiFT, Esymp , Pmin =x)
     # 
     turgor <- -PiFT - Esymp * rs1
     regulFact <- turgor/turgorPressureAtGsMax
     regulFact[regulFact<0] <- 0
     regulFact[regulFact>1] <- 1
   }
   
  Gs <- regulFact*gsmax
   
  return(Gs)
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
#' 
compute.gmin <- function(leafTemperature, gmin_20, TPhase, Q10_1, Q10_2, gminTempOff=F) {
  if(!gminTempOff){
    if (leafTemperature<= TPhase) {
      gmin <- gmin_20 *Q10_1^((leafTemperature - 20) / 10)
    } else if (leafTemperature> TPhase) {
      gmin <- gmin_20 *Q10_1^((TPhase - 20) / 10) * Q10_2^((leafTemperature- TPhase) / 10)
    }
  } else if(gminTempOff) {
    gmin=gmin_20
  }
  return(gmin)
}


#' calculate minimum transpiration (mmol/m2/s from gmin and vod 
#' #'
#' #' @param gmin  minimum conductance 
#' #' @param VPD vapor pressure deficit  (kPa)
#' #' @param airPressure surface air pressure (kPa) (default = 101.3) 
#' #' @export
#' #'
#' #' @examples
#' calcul.Emin <- function(gmin,VPD,airPressure = 101.3) {
#'   return(gmin * VPD /airPressure) 
#' }



compute.Emin <- function(gmin, gBL, gCrown, VPD, airPressure =101.3) {
  gmintot = 1/(1/gmin+ 1/gBL + 1/gCrown)
  return(gmintot * VPD /airPressure) 
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
#' @param k_PlantInit conductance of the plant from root to leaf 
#' @param ri root distribution within the soil layers.
#' @param fractLeafSym proportion of k_PlantInit assigned to the leaf (apoplasm to symplasm pathway)
#' @return
#' @export
#'
#' @examples
distribute.conductances <- function(k_PlantInit,ri,fracLeafSym = 0.4)
{
  
  fracRT   = (2/3)*(1-fracLeafSym)
  fractTL  =  (1/3)*(1-fracLeafSym)
  
  k_RSApoInit   = 1 /(fracRT / k_PlantInit) *ri
  k_SLApoInit = 1/(fractTL/k_PlantInit)
  k_LSymInit = 1/(fracLeafSym/k_PlantInit)
  
  #TODO: AJOUTE UN CALCUL DES CONDUCTANCE ICI POUR CHECK DU CALCUL? e.g.:
  #k_PlantInit <-  1/ (1 /sum(k_RSApoInit) + 1/k_SLApoInit + 1/k_LSymInit)
  return(list( k_SLApoInit =  k_SLApoInit, k_LSymInit = k_LSymInit ,  k_RSApoInit=k_RSApoInit, k_PlantInit=k_PlantInit))
}


compute.gCrown <- function(gCrown0, windSpeed){
  windSpeed=  max(0.1, windSpeed) # to avoid very high conductance values 
  return(gCrown0*windSpeed^0.6)
  }



# time step : hours
# LAI : m2.m^-2
# ETP : mm
# 
calculate.Ebound.Granier <- function(ETP,LAI,timeStep){
  ebound_mm = calculate.Ebound_mm.Granier(ETP = ETP, LAI=LAI)
  Ebound = ConvertFluxFrom_mm_To_mmolm2s(x = ebound_mm, timeStep = timeStep, LAI = LAI)
  return(Ebound)
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


# # New verion of compute Tleaf by Nicolas Martin (04/08/2021) : corrected cloud cover calculation  / Changed input parameters also
compute.Tleaf <- function(Tair, PAR, POTENTIAL_PAR, WS, RH, gs, g_cuti, Einst ,PsiLeaf,  leaf_size=50, leaf_angle=45, TurnOffEB=F, transpirationModel = "Jarvis")
{
  #Compute Tleaf and VPDLeaf
  # SWR  // short-wave radiation    (W/m2)
  # WS   // windspeed    (m/s)
  # Tair // air temperature (degC)
  # leaf_angle // # leaf angle (depuis le plan horizontal : 0-90 deg)
  # leaf_size  // characteristic dimension from vegetation params in mm (1 - 3000 : pine needle - banana leaf)

  #if(PsiLeaf==0) {PsiLeaf = -0.01}

  WS = max(WS, 0.1)  # Force minimum wind speed to avoid excessive heating
  SWR=  PAR*0.5495 # from µmol/m²/s to Watts/m²

  aSWR <- 0.5 #  //  absorptance to SWR %

  gflat <- 0.00662
  gcyl  <- 0.00403 # //  coefficient in rbl equation    m
  jflat <- 0.5 #
  jcyl  <- 0.6 # //  coefficient in rbl equation  none


  em_leaf <- 0.97  #    // emissivity    none
  SB <- 5.6704e-8  #    //  Stefan-Boltzman constant    W m-2 K-4
  p  <- 1.292      #   // density of dry air    kg/m3
  Cp <- 1010       #  // heat capacity of dry air    J kg-1 K-1
  y  <- 0.066      # // psychrometric constant    kPa K-1

  a  <- 0.61121    # // coefficient in esat equation    kPa
  b  <- 17.502     # // coefficient in esat equation    none
  z  <- 240.97     # // coefficient in esat equation    °C

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
  if(POTENTIAL_PAR>0) {cloud_cover = PAR/POTENTIAL_PAR} else {cloud_cover=0}
  if (cloud_cover>1) {cloud_cover=1}



  esat <- a * exp(b * Tair / (Tair + z)) # ; #kPa
  ea <- esat * (RH / 100) # ;
  s <- esat * b * z / ((Tair + z)^2) # ;
  em_air <- ((1 - 0.84 * cloud_cover) * 1.31 * ((10 * ea / (Tair + 273.15))^0.14285714) + 0.84 * cloud_cover) # ;
  VPDx =   esat - ea #Update VPD with esat and ea (why?)

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
  } # a needle, formula for a cylinder

  g_bl <- 1 / rbl * 1000 * 40 #      #leaf boundary layer conductance in mmol/s/m2
  rblr <- 1 / (1 / rbl + 1 / rr) #
  
  #Include the gs term into the energy balance
  if (transpirationModel == "Jarvis"){
  if ((gs+g_cuti)>0) {
    rst = 1/(gs+g_cuti)*1000*40} 
    else {
      rst = 9999.99}
    }

  if (transpirationModel == "Granier"){
    g = Einst/VPDx*101.3
    if (g>0) {rst = 1/(g)*1000*40} 
    else {
      rst = 9999.99}
    }


  ym <- y * (rst / rblr) #


  # compute Tleaf with linear approximation
  Delta_T <- (ym * Rni * rblr / (p * Cp) - VPDx) / (s + ym) #
  Tleaf <- Tair + Delta_T #
  T_Leaf <- Tleaf


  e_sat_air = 611.21*exp((18.678-Tair/234.5)*Tair/(257.14+Tair)) #;    // saturation vapour water pressure at Tair in Pa from Buck's equation
  e_air = e_sat_air*RH/100                                #// vapour water pressure at Tair and RHair
  VPD_Air =    (e_sat_air - e_air)/1000

  e_sat = 611.21*exp((18.678-Tleaf/234.5)*Tleaf/(257.14+Tleaf))   #;    // saturation vapour water pressure at Tair in Pa from Buck's equation
  e = e_sat*exp(PsiLeaf*2.16947115/(Tleaf+273.15))
  #// effect of leaf water potential on e
  VPD_Leaf = max(0, (e-e_air)/1000) #//vpd between leaf and air in kPa



  if(TurnOffEB == F) {vecres = c(T_Leaf, g_bl, VPD_Leaf,  VPD_Air, Delta_T)}
  #If turn off energy balance Tleaf = Tair
  if(TurnOffEB == T) {vecres = c(Tair, g_bl, VPD_Leaf, VPD_Air)}

  return(vecres)
}





