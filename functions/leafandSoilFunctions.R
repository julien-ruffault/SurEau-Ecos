
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

# # compute Rs from pmin (resolution from Bartlet et al 2012 EcolLett and email Herve Cochard 19/06/2015)
# Rs.Comp <- function(PiFT, Esymp, Pmin) {
#   A <- max((-1 * (Pmin + PiFT - Esymp) - sqrt((Pmin + PiFT - Esymp)^2 + 4 * (Pmin * Esymp))) / (2 * Esymp), 1 - PiFT / Pmin)
#   return(A)
# }
# 



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

# 



 # Function medfate pour calcule de la distribution des racines dans le sol 
# NumericVector ldrRS_one(double Z50, double Z95, NumericVector d){
#   int nlayer = d.size();
#   NumericVector dCum = clone(d);
#   NumericVector Vd(nlayer);
#   double c = 2.94/log(Z50/Z95);
#   double Vtot = 0.0;
#   Vd[0] = 1.0/(1.0+pow(d[0]/Z50,c));
#   Vtot = Vd[0];
#   for(int i=1;i<nlayer;i++) dCum[i] = dCum[i]+dCum[i-1];
#   for(int i=1;i<nlayer;i++){
#     Vd[i] = 1.0/(1.0+pow(dCum[i]/Z50,c)) -1.0/(1.0+pow(dCum[i-1]/Z50,c));
#     Vtot +=Vd[i];
#   }
#   //Rescale proportions so that they sum 1
#   for(int i=0;i<nlayer; i++) {
#     Vd[i] = Vd[i]/Vtot;
#   }
#   return(Vd);
# }








