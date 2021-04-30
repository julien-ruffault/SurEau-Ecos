
#----------------------------------------------------------------------------
#------------------         TURGOR COMPUTATION & PV-CURVES         ----------
#----------------------------------------------------------------------------

#-----
# Functions for leaf turgor :
# The set of functions allows to compute :
# Leaf turgor and osmotic and total potetnial based on leaf water content and parameter from P-V curves
# Leaf water content vased on minimum water potential and parameter from P-V curves
# From Bartlett et al 2012 Ecology letters and resolution Rs from Cochard 19/06/2015 (voir email to Sylvain Delzon and Nicolas Martin)

# RWCs symplastic relative water content:=(RWC ??? af)/(100-af)

# # Turgor pressure
# Pturg.Comp <- function(PiFT, Esymp, Rstemp) {
#   A <- -PiFT - Esymp * Rstemp
#   return(A)
# }
# 
# # Osmotic potential
# Osmo.Comp <- function(PiFT, Rstemp) {
#   A <- PiFT / (1 - Rstemp)
#   return(A)
# }
# 
# # Total potential
# PsiTotal.Comp <- function(PiFT, Esymp, Rstemp) {
#   A <- Comp.Pturg(PiFT = PiFT, Esymp = Esymp, Rstemp = Rstemp) + Comp.Osmo(PiFT = PiFT, Rstemp = Rstemp)
#   return(A)
# }


#------
# P.volume.comp <- function(G, ApFrac, Emax, Nhours = 10, LA, LMA = F, LFMC = F) {
#   # xylem.volume.comp computes the volume of xylem in the plant.
#   # Occupied by water this volume contributes to the stream flow when water potential decrease cause cavitation
#   # G is ratio of daily transpired over storage water
#   # We use the apoplasmic fraction to compute xylem volume
# 
#   Emaxday <- Emax * LA * Nhours * 3600 * 18 / 1000 / 1000
# 
# 
#   if (LMA & LFMC) {
#     LWS <- LMA * LA * LFMC / 1000 # Leaf water storage (g)
# 
#     G <- Emaxday / LWS # G for the Leaf compartment
#   }
# 
#   return(Emaxday * G * ApFrac)
# }
#

# compute Rs from pmin (resolution from Bartlet et al 2012 EcolLett and email Herve Cochard 19/06/2015)
Rs.Comp <- function(PiFT, Esymp, Pmin) {
  A <- max((-1 * (Pmin + PiFT - Esymp) - sqrt((Pmin + PiFT - Esymp)^2 + 4 * (Pmin * Esymp))) / (2 * Esymp), 1 - PiFT / Pmin)
  return(A)
}




#----------------------------------------------------------------------------
#------------------         TRANSPIRATION & PLANT CONDUCTANCE         -------
#----------------------------------------------------------------------------
# Functions and parameter for plant conductance and stomatal control
# Include function and paramter of PV-curve module
#----------------------------------------------------------------------------

#---FUNCTIONS
# Ktotal computes dynamically the total hydraulic conductance of the soil to leaves pathway
# It accounts for the effect of drought on plant conductance (cavitation) and soil resistivity
# So far there is only 2 resistances consider : soil and plant. In the future several resitances can be implemented.
#---

# updateKwithCavitation<- function(Kinit, PLC){
#   if(PLC<0 | PLC>1){warning("PLC non compris entre 0 et 1");print(PLC)}
#   # Plant Conductance
#   K=Kinit * (1 - PLC)
#   return(K)
# }

# Ktotal.comp <- function(...) {  # A Recoder si utile
#   if (Kplant > 0) {
#     Ktotal <- 1 / ((1 / Ksoil) + (1 / Kplant))
#   } else {
#     Ktotal <- 0
#   }
#   return(Ktotal)
# }
# 
# # Total Conductance = Soil Conductance & Plant COnductance
# PLC.comp <- function(Pmin, slope , P50) {
#   PLC  = 100 / (1 + exp(slope / 25 * (Pmin - P50)))
#   return(PLC)
# }
# 
# # This function computes PLCPrime from PLC current value
# PLCPrime.comp <- function(PLC , slope) {
#   return(- slope/25 * PLC/100 * (1 - PLC/100))
# }

# Percent loss of conductance based on vulnerability curevs
# PsiCompVC <- function(plcset, slope, b) {
#   # return the Psi for a given PLC and VC parameters
#   # a parameter of the VC
#   # b abs(P50)
#   a <- slope / 25
# 
#   (log((1 / plcset) - 1) - a * b) / a
# }

# #Pmin.Comp <- function(Psoil = Psoil, Ktotal = Ktotal, E = E) {
#   if (Ktotal > 0) {
#     Pmin <- Psoil - E / (Ktotal)
#   } else {
#     Pmin <- (-999)
#     # print("Warning conductance is 0 or negative and has been set to 0.0000001")
#   }
#   return(Pmin)
# }

# Minimum water potential

# Ecrit.Turg.comp <- function(Emax = Emax, Tpress = Tpress, TpRESSUREMax = TpRESSUREMax, Ecuti = Ecuti) {
#   # EcritT is a function that computes transpiration if stomatal closure is limited (linearly) by turgor pressure
#   # It returns the amount of transpiration in mmol/s/m?? assuming
# 
#   # Parameters:
#   # Emax
#   # Tpress: turgor pressure
#   # TpRESSUREMax: : Maximum turgor pressure
#   # Ecuti: residual transpiration
#   Tr <- min(Emax * Tpress / TpRESSUREMax, Emax)
#   return(c(max(Tr, 0), min(max(max(Tr, 0) + Ecuti, 0 + Ecuti), Emax)))
# }
# EcritT is a function that computes transpiration if stomatal closure is limited (linearly) by turgor pressure
# It returns the amount of transpiration in mmol/s/m?? assuming

# Ecrit.Cav.comp <- function(Ktotal = Ktotal, Psoil = Psoil, Pset = Pset, Ecrit.saf = Ecrit.saf, Emax = Emax, Ecuti = Ecuti) {
#   # EcritCav is a function that computes transpiration if stomatal closure adjusts in order to avoid Pmin<pcav
#   # it returns the amount a transpiration in mmol/s/m?? assuming
# 
# 
#   # Parameters:
#   # Ktotal
#   # Psoil
#   # Pset
#   # Ecrit.saf
#   # Emax
#   # Ecuti
#   Tr <- min((Ktotal) * (Psoil - Pset) * (1 - Ecrit.saf), Emax)
#   return(c(max(Tr, 0), min(max(max(Tr, 0) + Ecuti, 0 + Ecuti), Emax), Tr))
# }
# # EcritCav is a function that computes transpiration if stomatal closure adjusts in order to avoid Pmin<pcav
# # it returns the amount a transpiration in mmol/s/m?? assuming
# 
# Ecrit.Cav.comp2 <- function(Ktotal = Ktotal, Psoil = Psoil, Pset = Pset, Ecrit.saf = Ecrit.saf, Emax = Emax, Ecuti = Ecuti) {
#   # EcritCav is a function that computes transpiration if stomatal closure adjusts in order to avoid Pmin<pcav
#   # It returns the amount a transpiration in mmol/s/m?? assuming
# 
#   # Parameters:
#   # Ktotal
#   # Psoil
#   # Pset
#   # Ecrit.saf
#   # Emax
#   # Ecuti
#   Tr <- min((Ktotal) * (Psoil - Pmin) * (1 - Ecrit.saf), Emax)
#   return(c(max(Tr, 0), min(max(max(Tr, 0) + Ecuti, 0 + Ecuti), Emax), Tr))
# }


# E.Empiric.comp <- function(Emax = Emax, Tpress = Tpress, TpRESSUREMax = TpRESSUREMax, Ecuti = Ecuti) {
#   # Ajout d'une r?gulation empirique
# }

# #----------------------------------------------------------------------------
# #-------------------------         SOIL         ----------------------------
# #----------------------------------------------------------------------------
# # Functions and parameter for Soil water content and conductance
# # Also computes soil parameters based on soil texture, depth, stone content & according to different models
# #----------------------------------------------------------------------------
# 
# # K at saturation Ksat (mol/m/s/Mpa)
# # REW
# # REW.Comp <- function(WR = WR, SV = SV, TetaWilt = TetaWilt, TetaSat = TetaSat) {
# # 
# #   # TetaAct (????, unitless): Actual relative water content or -- according to Van Genutchen formulaiton-- water content at matric potential ??,
# #   # TetaWilt (??r, unitless): relative water content at wilting point or -- residual water content -- usually fitted to measured data.
# #   # TetaSat (??s, unitless): relative water content at saturation -- usually taken as the measured total porosity (i.e. the water content at a matric potential of 0)
# #   # WR (kg/m?? or mm) :  Water Reserve ( in Kg or mm, from water budget module)
# #   # SV (m3): Soil Volume cvomputable from soil paramters such as depth, stone fraction...
# # 
# #   # Soil water holding capacity
# #   RelSWHC <- TetaSat - TetaWilt
# # 
# #   # Compute the relative water content (m3 water/m3 soil) based on Water Reserve and soil volume
# #   TetaAct <- WR / SV / 1000
# # 
# #   return(max(0, (TetaAct - TetaWilt) / RelSWHC))
# # }
# 
# 
# 
# # Psoil
# Psoil.Comp <- function(REW = REW, alfa = alfa, n = n, psie, SWStemp, tsc, b_camp, method = "VG") {
# 
#   #---------------------------------------------------------------------------------------------------------------------------
#   # DESCRIPTION
#   # Psoil.comp() permit to compute soil matrix water potential from soil water content and a set of soil parameters according
#   # to the formulation from Van Genuchten 1980
#   # (later I'll include another formalation from Campbell et al 1985)
#   #---------------------------------------------------------------------------------------------------------------------------
# 
#   #---------------------------------------------------------------------------------------------------------------------------
#   # PARAMETERS
#   # alfa (unitless): shape paramter (0.015 in Sur_eau)
#   # n (unitless): shape paramter (1.253 in Sur_eau)
#   # m (unitless): shape parameter usually computed from n as m=(1-1/n)
#   #---------------------------------------------------------------------------------------------------------------------------
# 
#   #---------------------------------------------------------------------------------------------------------------------------
#   # Computes basics required parameters
#   #---------------------------------------------------------------------------------------------------------------------------
# 
#   #---------------------------------------------------------------------------------------------------------------------------
#   # Computation of soil water potential, from Van Genuchten 1980
#   if (method == "VG") {
#     m <- (1 - 1 / n)
#     return((
#       -1 * ((((1 / REW)^(1 / m)) - 1)^(1 / n)) / alfa / 10000
#     ))
#   }
#   #---------------------------------------------------------------------------------------------------------------------------
# 
#   #-----------------------------------------  
#   # Campbell
#   if (method == "Camp") {
#     return(psie * ((SWStemp / tsc)^-b_camp))
#   } # A ajouter...
#   #-----------------------------------------  
# }
# 
# 
# # Teta
# TetaPsi.comp    <- function(TetaWilt = TetaWilt, TetaSat = TetaSat, Psoil = Psoil, alfa = alfa, n = n) {
#   m <- (1 - 1 / n)
#   teta <- TetaWilt + ((TetaSat - TetaWilt) / ((1 + abs(alfa * Psoil)^n)^m))
#   return(teta)
# }

# computePsi.WBsoil <- function(method) {
#   # compute soil matrix water potential from soil water contents
#   # formulation from Van Genuchten 1980 or Campebell
# 
#   # PARAMETERS Van Genuchten
#   # alfa (unitless): shape paramter (0.015 in Sur_eau)
#   # n (unitless): shape paramter (1.253 in Sur_eau)
#   # m (unitless): shape parameter usually computed from n as m=(1-1/n)
#   # thethaSC :  Soil watr content at staturation
#   # thetaRes :  Residual soil Water content
#   # PARAMETERS Campbell
#   #  pise    : air entry potential
#   # thethaSC : Soil watr content at saaturation
#   # b_camp   : shape parameter of the campbel equation
# 
#   # Computation of soil water potential, from Van Genuchten 1980
#   if (method == "VG") {
#     TotalAvailWater <- (tsc - twp)
#     ActualAvailWater <- (SWS - twp)
#     REW <- max(0.000000001, ActualAvailWater / TotalAvailWater)
#     m <- (1 - 1 / n)
#     Psoil <- -1 * ((((1 / REW)^(1 / m)) - 1)^(1 / n)) / alfa / 10000
#   }
#   #-----------------------------------------  
#   # Campbell
#   if (method == "Camp") {
#     Psoil <- -1 * (psie * ((SWS / tsc)^-b_camp))
#   }
# return(Psoil)  
# }# computePsi.WBsoil <- function(method) {
#   # compute soil matrix water potential from soil water contents
#   # formulation from Van Genuchten 1980 or Campebell
# 
#   # PARAMETERS Van Genuchten
#   # alfa (unitless): shape paramter (0.015 in Sur_eau)
#   # n (unitless): shape paramter (1.253 in Sur_eau)
#   # m (unitless): shape parameter usually computed from n as m=(1-1/n)
#   # thethaSC :  Soil watr content at staturation
#   # thetaRes :  Residual soil Water content
#   # PARAMETERS Campbell
#   #  pise    : air entry potential
#   # thethaSC : Soil watr content at saaturation
#   # b_camp   : shape parameter of the campbel equation
# 
#   # Computation of soil water potential, from Van Genuchten 1980
#   if (method == "VG") {
#     TotalAvailWater <- (tsc - twp)
#     ActualAvailWater <- (SWS - twp)
#     REW <- max(0.000000001, ActualAvailWater / TotalAvailWater)
#     m <- (1 - 1 / n)
#     Psoil <- -1 * ((((1 / REW)^(1 / m)) - 1)^(1 / n)) / alfa / 10000
#   }
#   #-----------------------------------------  
#   # Campbell
#   if (method == "Camp") {
#     Psoil <- -1 * (psie * ((SWS / tsc)^-b_camp))
#   }
# return(Psoil)  
# }
  
# compute.KSoil   <- function(method)  {
#   # compute soil conductance from soil water content 
# 
#   # Compute soil conductivity (mmol/m/s/MPa)
#   # Soil Ks (mmol/m/s/MPa) computed from Ksat and the conductance factor (cond fac)
#   # CondFac upscaling factors at the rhizophere with roots dimensions
#   # CondFac depends on b (root radius) r (distance between roots) and La (root length per area)
#   # Generic conductance factor for upsacling conductivity to rhizophere (gardner cowan 1960 model)
# 
#   CondFac <- 1000 * La * 2 * 3.14 / log(b / r) # Soil Ks (mmol/m/s/MPa)
#   Ks <- Ksat * CondFac
#   # Compute soil hydraulic conductivity with Van Genuchten
#   if (method == "VG") {
#     TotalAvailWater <- (tsc - twp)
#     ActualAvailWater <- (SWS - twp)
#     REW <- max(0.000000001, ActualAvailWater / TotalAvailWater)
#     m <- (1 - 1 / n)
#     ksoil <- REW^(I) * (1 - (1 - REW^(1 / m))^m)^2
#   }
#   # Compute soil hydraulic conductivity with campbell
#   if (method == "Camp") {
#     ksoil <- (SWS / tsc)^(b_camp * 2 + 2)
#   }
#   # Compute Rhizosphere conductance
#   Ksoil <- Ks * ksoil
# 
# return(Ksoil)
# }

# calculate_ET_Granier <- function(ETP, LAI, a = -0.006, b = 0.134, c = 0) {
#   ET_Granier <- pmax(0, ETP * (a * LAI^2 + b * LAI + c))
#   return(ET_Granier)
# }










#// convert flux in mm to mol/m2/s : x(mm) , timestep(hours) ,LAI(m2/m2)
ConvertFluxFrom_mm_To_mmolm2s <- function(x, timeStep, LAI) {
  if (LAI > 0) {
    y <- 10^6 * x / (LAI * timeStep * 3600 * 18)
  } else if (LAI == 0) {
    y <- 0
  }
  return(y)
}


#' Convert a water instantaneous water flux in mmol.m-2.s-1 to a amount in mm 
#' over a defined time period 
#'
#' @param x The instaneous flux (mmol.m-2.s-1) to be converted. expressed per
#' m2 of leaf 
#' @param timeStep timeStep (in hours) 
#' @param LAI leaf area index of the stand 
#'
#' @return
#' @export
#'
#' @examples
ConvertFluxFrom_mmolm2s_To_mm <- function(x, timeStep, LAI) {
  y <- x * (LAI * timeStep * 3600 * 18) / 10^6
  return(y)
}




calculate_gs_Jarvis <- function(PAR, leafTemperature, option=1, gsMax = 200, Ca = 400, gsNight = 20, JarvisPAR = 0.006, Tgs_sens=17, Tgs_optim=25)
{
  
  if (option==1) # temeperature effect on gs 
  {
    gsMax2    = max(0,gsMax/(1+((leafTemperature-Tgs_optim)/Tgs_sens)^2))
    gsNight2  = max(0,gsNight/(1+((leafTemperature-Tgs_optim)/Tgs_sens)^2))
  }
  
  
  #if (option==7){
  # gsMax2 = (gsMax*(1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((leafTemperature-Tgs_optim)/Tgs_sens,2))  # 
  # gsNight2 = (gs_night* (1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((T_Leaf-Tgs_optim)/Tgs_sens,2));
  #}
  
  gs_Jarvis=   gsNight2 + (gsMax2-gsNight2)*(1-exp(-JarvisPAR*PAR))
  #print(gs_Jarvis)
  return(gs_Jarvis)
}

# calculate_ET_Gs <- function(VPD, windSpeed, gmin, gs, gCrown0=45, gBL, PsiStartClosing, PsiClose, Psi)
# {
#   
#   # Il faudrait remplacer :  PsiStartClosing, PsiClose & Psi Par le type de régulation de Psi
#   
#   if(windSpeed<0.1) {windSeed <- 0.1}
#   
#   gs_Bound = gs
#   gs_lim = gs * (Psi - PsiClose)/(PsiStartClosing - PsiClose)
#   gCcrown = gCrown0*windSpeed^0.6
#   
#   gcanopy_bound = 1/((1/g_crown)+(1/gs_Bound) + (1/gBL))
#   gcanopy_lim = 1/((1/g_crown)+(1/gs_lim) + (1/gBL))
#   
#   if(Psi > PsiStartClosing){
#     Eprime =  0
#     E0 =   gmin*VPD/101.3  + gcanopy_bound*VPD/101.3
#     
#   } else if(Psi < PsiClose)
#   {
#     Eprime =  0
#     E0 =   gmin*VPD/101.3 
#     
#   } else if(Psi > PsiClose & Psi<PsiStartClosing)
#   {
#     
#     Eprime =  gcanopy_bound*VPD/101.3/(PsiStartClosing - PsiClose)
#     
#     E0 =  gmin*VPD/101.3 + gcanopy_lim*VPD/101.3
#     
#   }  
#   
#   return(c(E0, Eprime, gs_Bound, gs_lim, g_crown, gcanopy_bound, gcanopy_lim))
# }

# utilities 
convert.FtoV <- function(x, .rock_fragment_content, .layer_thickness) {
  # . rockfragmentcontent (%)
  # .layer_thickness (m)
  # x :  (cm3.cm3.).
  # y : (mm)
  
  y <- x * (1 - (.rock_fragment_content / 100)) * .layer_thickness * 1000
  
  return(y)
}

#Function to sum in series 2 k
kseriesum<-function(k1,k2) {return(1/(1/k1+1/k2))}









