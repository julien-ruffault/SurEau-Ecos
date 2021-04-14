 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#                       &
#           Julien Ruffault (julien.ruff@gmail.com)
#                       &
#           Francois Pimont (francois.pimont@inrae.fr)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##


# V2 (18/12/2020) // JR modified the following : 
#  - Ajouter un kAbovegroubnd et Kbelowground variable suivant la PLCAbove et PLCBelow (pour l'instant ne bouge pas)
# -  Remettre le initialze year dans le function pheno (si DOY = 1, on reinitialise le LAI, la PLC et le k)
# -  Ajout d'un k qui depent du LAI selon kplant  = kPlantInit*LAI/LAImax
# -  Mettre une seuil de 10 % de PLC pour commencer la defoliation --> OK 

# V3 (21/01/2021) // JR added the new formulation for transpiration

# To be done : TESTER Penmann au lieu de Priestly taylor  dans Granier  
#              puis Se debrasser de granier avec Gs, Gbl et Gcrown

# (25/01/2021) // NM addded new formamulation to derive stomatal conductances 
# note  = see to transfer Lv, La, R and kroot in WVveg rather than in WBsoil 

# V10 (6/03/2021) // Nico and Francois did lots of modification and started to get promissing runs with Leaf cavitation

# V11-12 (11-12/03/2021) // Starting cleaning the code
# TODO UpdateSoilWater.WBsoil should be double checked (use the good psi and "waterrelease from cavitation"!) but can be called in the main as we don't update soil over 1 large time step.


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create an "object" WBveg from  veg_params
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
new_WBveg <- function(veg_params) {
  
  WBveg <- list() # initialisation
  
  WBveg$params <- veg_params # assign parameters 
  WBveg$params$PsiTlp <- WBveg$params$PiFullTurgor*WBveg$params$EpsilonSymp/(WBveg$params$PiFullTurgor+WBveg$params$EpsilonSymp)
  
  # INITIALISE 
  WBveg$Psi_LApo = 0
  WBveg$Psi_TApo = 0
  WBveg$Psi_LSym = 0
  WBveg$Psi_TSym = 0 
  WBveg$Psi_LApo_cav = 0 # FP replaced "mem" by "cav" (when cavitation starts)
  WBveg$Psi_TApo_cav = 0
  WBveg$Psi_AllSoil <- 0 
  #----Conductance & capacitance
  # (mmol/m2/s/MPa) Here on leaf area basis but they are to be updated as a function symplasm conductance and leaf area
  WBveg$K_LSym = 1.8 #to be changed as an absolute conductance and to divide by leaf area
  WBveg$K_TSym = 0.26
  
  #Symplasm capacitance updated as a function of water content and Leaf area (mmol/m2/MPa)
  WBveg$C_LSym = 0 # Needs to be updated as a function of pressure volume curves
  WBveg$C_TSym = 0 # Needs to be updated as a function of pressure volume curves
  
  WBveg$C_LApo <- 1.45 #Capacitance apoplasmique Leaf (mmol/m2 of leaf/s/MPA) ref: Sureau-param-Cochard
  WBveg$C_TApo <- 7 #Capacitance apoplasmique Trunk (mmol/m2 of leaf/s/MPA) ref: Sureau-param-Cochard
  
  # hydraulic conductances
  WBveg$kSoilToCollar <- numeric(3) # / conductance rhisophere for each soil layer
  WBveg$kAllSoilToCollar <- 0
  WBveg$KAllSoilToCanopy <- 0
  WBveg$kAboveGround <- 0
  WBveg$kBelowGround <- 0
  WBveg$kRoot <- numeric(3)
  
  # Leaf and canopy conductance
  WBveg$gmin <- 0 #Gmin for leaves
  WBveg$gminT <- 3 #Gmin for trunk and branches
  WBveg$TBA = 0.8 #Trunk and branch area # actual gminT is gminT*TBA TODO : gminT could be initialized directly to 3*0.8?
  
  WBveg$gs <- 0
  WBveg$gs_bound <- 0
  WBveg$gs_lim <- 0
  WBveg$gcanopy_Bound <- 0
  WBveg$gcanopy_lim <- 0
  WBveg$gBL <- 0
  WBveg$g_crown <- 0
  WBveg$RegulFact <- 1
  
  # Fluxes
  WBveg$E0 = 0
  WBveg$Eprime = 0
  WBveg$Emin <- 0
  WBveg$EminT <- 0
  WBveg$Ebound <- 0
  WBveg$Elim <- 0
  WBveg$AET.C <- numeric(1)
  WBveg$Emin.C <- 0
  WBveg$fluxCollarToCanopy.C <- numeric(1)
  WBveg$fluxSoilToCollar.C <- numeric(3)
  
  # LAI and LAI-dependent variables
  WBveg$LAIpheno <- numeric(1)
  WBveg$LAI      <- numeric(1)    #  LAI
  WBveg$CSC      <- numeric(1)    #  Canopy Storage Capacity
  
  # interception 
  WBveg$pptsoil  <- 0 # ppt that reach the soil
  WBveg$QR       <- 0 # QR /quantite d'eau dans la canopee
  
  WBveg$evaporationIntercepted <- 0
  WBveg$ETPr <- 0
  
  #WBveg$defoliation <- 0 # defoliation // no defoliation (add an option to set defoliation due to cavitation of the Plant Above)
  WBveg$deadLAI <- 0
  
  # Cavitation
  WBveg$PLCAbove     <- 0  # percent loss of conductivity [%]/ Above part of the 
  WBveg$PLCBelow     <- 0  # percent loss of conductivity [%] / 
  
  # leaf temp
  WBveg$Tleaf <- NA
  
  #--------------- pheno-----
  # parameters if deciduous
  if (veg_params$Foliage == "Deciduous" & general_params$LAImod == T) {
    WBveg$LAIpheno       <- 0
    WBveg$sumTemperature <- 0 # temmpeerature sum to determine budburst
    WBveg$budburstDate   <- NA # budburst date
  }
  
  # water storage in canopy
  WBveg$DFMC     <- NA # Fuel moisture content of the dead compartment (gH20/gMS)
  WBveg$LFMCApo  <- NA # Live fuel moisture content of the apoplasmic compartment (gH20/gMS)
  WBveg$LFMCSymp <- NA # Live fuel moisture content of the apoplasmic compartment (gH20/gMS)
  WBveg$LFMC     <- NA # live fuel moisture content (gH20/gMS)
  
  WBveg$WCCanopySymp    <- NA # water content in the sympoplasm compartment at saturation (l/m2 (i.e. mm)
  WBveg$WCCanopySympSat <- NA # water content in the sympoplasm compartment (l/m2 (i.e. mm)
  WBveg$WCCanopyApo     <- NA # water content in the apoplasm compartment  (l/m2 (i.e. mm)
  WBveg$WCDeadCanopy    <- NA # water content in the dead compartment  (l/m2 (i.e. mm) , initialized at zero
  
  WBveg$waterRelease    <- NA # water release in the soil because of Cavitation (Unit)
  WBveg$DMLiveCanopy    <- NA # Live Canopy dry matter [gMS/m2 soil]
  WBveg$DMDeadCanopy    <- 0 # Dead Canopy dry matter [gMS/m2 soil]
  
  #---------------------#
  WBveg$params$Q_LApo_sat_mmol  <- 0
  WBveg$Q_LApo_mmol  <- 0
  WBveg$params$Q_TApo_sat_mmol <- 0
  WBveg$Q_TApo_mmol  <- 0
  
  WBveg$params$Q_LSym_sat_mmol   <- 0
  WBveg$Q_LSym_mmol       <- 0
  WBveg$params$Q_TSym_sat_mmol   <- 0
  WBveg$Q_TSym_mmol       <- 0
  
  WBveg$F_L_Cav <- 0
  WBveg$F_T_Cav <- 0
  WBveg$Delta_Q_LApo_mmol_diag <- 0
  #---------------------#
  
  # TODO Warning('P50 and slope for the above and below compartment are set equal')
  WBveg$params$P50_VC_Below <- WBveg$params$P50_VC 
  WBveg$params$P50_VC_Above <- WBveg$params$P50_VC
  WBveg$params$Slope_VC_Below <- WBveg$params$Slope_VC
  WBveg$params$Slope_VC_Above <- WBveg$params$Slope_VC
  WBveg$params$Regul <-"Sigmoid"
  #WBveg <- computeCavitation.WBveg(WBveg)
  WBveg$PLCAbove = PLC.comp(Pmin = WBveg$Psi_LApo, slope = WBveg$params$Slope_VC_Above, P50 = WBveg$params$P50_VC_Above)
  WBveg$PLCBelow = PLC.comp(Pmin = WBveg$Psi_TApo, slope = WBveg$params$Slope_VC_Below, P50 = WBveg$params$P50_VC_Below)
  
  # initialize capacitance
  WBveg <- updateCapaSym.WBveg(WBveg) 
  # NB conductance are computed after when soil, pheno, etc are done
  
  return(WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute pheno and LAI for an object WBveg in SUREAU_ECOS
### ### ### ### ### ### ### ### ### ### ## ### ### ### ### ### ### ### ### ##
computePheno.WBveg <- function(.WBveg, temperature, DOY, LAImod = T, LAIpres=F) {
  
  # INPUTS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  #   - .WBveg
  #   - temperature    :  daily tempeature  (degC)
  #   - DOY            :  day of year
  #  -  LAImod=T
  ### ### ### ### ### ### ### ### ###
  # major changed 26/01/2021 JR/ simplified to give just LAIpheno as output (not final LAI that combines both LAI    
  
  # set to initial parameters at the beginning of the year 
  if (DOY==1){
    if (.WBveg$params$Foliage == "Evergreen") {
      .WBveg$LAIpheno <- .WBveg$params$LAImax  # note  : for evergreen this values will remain the same untill the end of the year ! 
    }
    else if (.WBveg$params$Foliage == "Deciduous") {
      .WBveg$LAIpheno <- 0
      .WBveg$sumTemperature <- 0
      .WBveg$budburstDate   <- NA
    }
    
    #.WBveg$CanopyWCdead <- 0 <-- mettre ailleurs  (26/01/2021)
    #.WBveg$LAIdead <- 0      <-- mettre ailleurs  (26/01/2021)
    
    #.WBveg$PLCAbove <- 0   mettre ailleurs  (26/01/2021)
  }
  if (.WBveg$params$Foliage == "Deciduous" & LAImod == T) # si decidus LAI =fonction(pheno)-defol
  {
    if (is.na(.WBveg$budburstDate) == T) # si pas de debourrement
    {
      if (temperature > .WBveg$params$Tbase & DOY >= .WBveg$params$DayStart) {
        .WBveg$sumTemperature <- .WBveg$sumTemperature + temperature # update SumTemp si T>tbase
      }
      
      if (.WBveg$sumTemperature > .WBveg$params$Fcrit) {
        .WBveg$budburstDate <- DOY
      } # budburstday
    }
    
    else if (DOY >= 280) # perte des feuilles au jour 270
    {
      .WBveg$LAIpheno= max(0,.WBveg$LAI-max(0, .WBveg$params$LAImax / .WBveg$params$nbdayLAI))
    }
    
    else if (is.na(.WBveg$budburstDate) == F) {
      if (DOY < .WBveg$budburstDate + .WBveg$params$nbdayLAI) # si debourrement et que periode de construction des feuilles
      {
        .WBveg$LAIpheno <- .WBveg$LAIpheno + max(0, .WBveg$params$LAImax / .WBveg$params$nbdayLAI) #
      }
    }
    
  }
  
  # temporary for now /// 
  .WBveg$LAI <- .WBveg$LAIpheno
  
  # update LAI-dependent variables 
  .WBveg$FCC <- (1 - exp(-.WBveg$params$K * .WBveg$LAI))
  .WBveg$CSC <- 1.5 * .WBveg$LAI
  
  # update water storing capacities of the dead and living canopy
  .WBveg$DMLiveCanopy <- .WBveg$LAI * .WBveg$params$LMA
  .WBveg$DMDeadCanopy <- .WBveg$deadLAI * .WBveg$params$LMA
  
  if (DOY==1)
  {
    Q_LSym_sat_L <- (1 / (.WBveg$params$LDMC / 1000) - 1) * .WBveg$DMLiveCanopy * (1 - .WBveg$params$ApoplasmicFrac) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
    .WBveg$params$Q_LSym_sat_mmol <- Q_LSym_sat_L*1000000/18
    .WBveg$Q_LSym_mmol <- .WBveg$params$Q_LSym_sat_mmol
    
    Q_LApo_sat_L <- (1 / (.WBveg$params$LDMC / 1000) - 1) * .WBveg$DMLiveCanopy * (.WBveg$params$ApoplasmicFrac) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
    .WBveg$params$Q_LApo_sat_mmol <- Q_LApo_sat_L*1000000/18
    .WBveg$Q_LApo_mmol <- .WBveg$params$Q_LApo_sat_mmol
    
    Q_TSym_sat_L <- 5 * (1 - 0.7) # Leaf symplastic water content in l/m2 (i.e. mm)
    .WBveg$params$Q_TSym_sat_mmol <- Q_TSym_sat_L*1000000/18
    .WBveg$Q_TSym_mmol <- .WBveg$params$Q_TSym_sat_mmol
    
    Q_TApo_sat_L <- 5 * (0.7) # Leaf symplastic water content in l/m2 (i.e. mm)
    .WBveg$params$Q_TApo_sat_mmol <- Q_TApo_sat_L* 1000000/18
    .WBveg$Q_TApo_mmol <- .WBveg$params$Q_TApo_sat_mmol
    
    
  }
  
  .WBveg$WCCanopySympSat <- (1 / (.WBveg$params$LDMC / 1000) - 1) * .WBveg$DMLiveCanopy * (1 - .WBveg$params$ApoplasmicFrac) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
  .WBveg$WCCanopyApo <- (1 - .WBveg$PLCAbove / 100) * .WBveg$DMLiveCanopy * (1 / (.WBveg$params$LDMC / 1000) - 1) * (.WBveg$params$ApoplasmicFrac) / 1000
  
  
  #print(paste0("LAIpheno = " ,.WBveg$LAI))
  return(.WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Rain interception by canopy/ stock of water in the canopy reservoir (one vegetation layer only)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
computeInterception.WBveg <- function(.WBveg, ppt) {
  
  # inputs : 
  #   - WB.veg         : object WBveg 
  #   -  ppt          :  precipitation (mm)
  
  if (ppt * .WBveg$FCC <= (.WBveg$CSC - .WBveg$QR)) # pas de débordement
  {
    .WBveg$pptsoil <- ppt * (1 - .WBveg$FCC)
    .WBveg$QR <- .WBveg$QR + (ppt * .WBveg$FCC)
  }
  
  else if (ppt * .WBveg$FCC > (.WBveg$CSC - .WBveg$QR)) #  debordement
  {
    .WBveg$pptsoil <- ppt - (.WBveg$CSC - .WBveg$QR)
    .WBveg$QR <- .WBveg$CSC
  }
  
  return(.WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute water stocks in leaves/wood in SUREAU_ECOS (one vegetation layer only)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
computeWaterStorage.WBveg <- function(WBveg, VPD) {
  
  # Dead FMC [%]
  WBveg$DFMC <- DFMC_dedios(VPD)
  
  # Water content of dead foliage (l/m2 sol ou mm)
  WBveg$WCCanopyDead <- (WBveg$DFMC / 100) * WBveg$DMDeadCanopy / 1000
  
  #----Symplasmic canopy water content----
  RWCs <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor, Esymp = WBveg$params$EpsilonSymp, Pmin = WBveg$Psi_LSym) # Relative water content (unitless)
  WBveg$WCCanopySymp <- max(0, RWCs) * WBveg$WCCanopySympSat
  WBveg$LFMCSymp <- 100 * (WBveg$WCCanopySymp / (WBveg$DMLiveCanopy * (1 - WBveg$params$ApoplasmicFrac) / 1000))
  
  #---Apoplasmic water content----
  WBveg$waterRelease <- max(0, WBveg$WCCanopyApo * max(0, WBveg$dPLCAbove / 100)) #  Water release from xylem cavitation, [mm]
  WBveg$WCCanopyApo <- max(0, WBveg$WCCanopyApo - WBveg$waterRelease) #  Canopy apoplasmic water content  [mm]
  WBveg$LFMCApo <- 100 * (WBveg$WCCanopyApo / (WBveg$DMLiveCanopy * WBveg$params$ApoplasmicFrac / 1000)) #  LFMC of Apo (relative moisture content to dry mass), gH20/gMS
  
  #------LFMC total---- (Apo+Symp)
  WBveg$LFMC <- 100 * (WBveg$WCCanopySymp + WBveg$WCCanopyApo) / (WBveg$DMLiveCanopy / 1000)
  #- FMCcanopy 
  WBveg$FMCCanopy <- 100 * (WBveg$WCCanopySymp + WBveg$WCCanopyApo + WBveg$WCCanopyDead) / (WBveg$DMLiveCanopy / 1000 + WBveg$DMDeadCanopy / 1000)
  
  return(WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Compute evaporation of the water intercepted by the canopy in the SUREAU_ECOS model
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
computeEvapIntercepted.WBveg <- function(WBveg, ETP) {
  if (ETP >= 0) {
    if (ETP >= WBveg$QR) { #  all the water intercepted by the canopy is tranpired
      WBveg$ETPr <- ETP - WBveg$QR
      WBveg$evaporationIntercepted <- WBveg$QR
    }
    WBveg$QR <- 0 # empty the reservoir
  } else if (ETP < WBveg$QR) { # Energie pas suffisante pour evaporer l'eau interceptée
    WBveg$QR <- WBveg$QR - ETP
    WBveg$ETPr <- 0
    WBveg$evaporationIntercepted <- WBveg$QR - ETP
  }
  return(WBveg)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# updateKplant - checked FP 11/03/2021
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
updateKplant.WBveg <- function(WBveg,WBsoil) {
  # calculate kabove and kbelow from "kplant", assuming an equal ksat above and below
  WBveg$kBelowGround <- (2 * WBveg$params$kPlantInit) * (1-WBveg$PLCBelow/100)
  WBveg$kAboveGround <- (2 * WBveg$params$kPlantInit) *  (1-WBveg$PLCAbove/100)
  
  # Root from root length
  WBveg$kRoot        <- WBveg$kBelowGround * (WBsoil$params$Lv/(sum(WBsoil$params$Lv))) 
  
  WBveg$kSoilToCollar    <- kseriesum(WBsoil$kSoil, WBveg$kRoot) # conductance from soil to collar (two resitances in series Rsoil and Rroot)
  WBveg$kAllSoilToCollar <- sum(WBveg$kSoilToCollar) # conductance in parallel
  
  WBveg$KAllSoilToCanopy <- kseriesum(WBveg$kAboveGround, WBveg$kAllSoilToCollar)# overall K of the systemm
  
  return(WBveg) 
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute an up date of plant capacitances (19/02/2021) - checked FP 11/03/2021
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
updateCapaSym.WBveg <- function(WBveg){
  PsiTlp <- WBveg$params$PsiTlp
  PiFullTurgor <- WBveg$params$PiFullTurgor
  Esymp <- WBveg$params$EpsilonSymp
  dbxmin = 1e-100 # NM minimal double to avoid-INF
  
  #----Compute the relative water content of the symplasm----
  RWC_LSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor, Esymp = WBveg$params$EpsilonSymp, Pmin = WBveg$Psi_LSym-dbxmin) # Relative water content (unitless)
  #----Compute the derivative of the relative water content of the symplasm----
  if(WBveg$Psi_LSym > PsiTlp) { # FP derivative of -Pi0- Eps(1-RWC)+Pi0/RWC
    RWC_LSym_prime = RWC_LSym/(-PiFullTurgor-WBveg$Psi_LSym-Esymp+2*Esymp*RWC_LSym)
  } else {
    RWC_LSym_prime = -PiFullTurgor/WBveg$Psi_LSym^2 # FP derivative of Pi0/Psi
  }
  #Compute the leaf capacitance (mmol/MPa/m2_sol)
  WBveg$C_LSym <- WBveg$params$Q_LSym_sat_mmol*RWC_LSym_prime
  
  #----Trunk symplasmic canopy water content----
  #TODO Warning: same parameters for leaves and trunk
  RWC_TSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor, Esymp = WBveg$params$EpsilonSymp, Pmin = WBveg$Psi_TSym-dbxmin) # Relative water content (unitless)
  
  #----Compute the derivative of the relative water content of the symplasm----
  if(WBveg$Psi_TSym > PsiTlp) {RWC_TSym_prime = RWC_TSym/(-PiFullTurgor-WBveg$Psi_TSym-Esymp+2*Esymp*RWC_TSym)} 
  else {RWC_TSym_prime = -PiFullTurgor/WBveg$Psi_TSym^2}
  #Compute the capacitance (mmol/MPa/m2_sol)
  WBveg$C_TSym <- WBveg$params$Q_TSym_sat_mmol*RWC_TSym_prime
  
  return(WBveg)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# computeTranspiration (gmin, Emin, gcanopy, Ebound and regulation)
# #FP this section was cleaned a bit and a bug was corrected in Eprime (when regul=0)
# TODO check why we have gs and gs_lim with same value
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
computeTranspiration.WBveg <- function(WBveg, WBclim, Nhours) {
  TGbl_Leaf <- Tleaf(Tair = WBclim$Tair_mean,SWR = WBclim$RG * 1e6 / (Nhours * 3600), # conversion en W/m
                     WS = WBclim$WS,VPD = WBclim$VPD,RH = max(100, WBclim$RHair_mean),gs=WBveg$gs,Einst = max(0.00001, WBveg$SumFluxSoilToCollar))
  WBveg$Tleaf <- TGbl_Leaf [1] # Transpiration/Gs du pas de temps précédent
  # cuticular 
  WBveg$gmin <- calcul_gmin(temperatureLeaf = WBveg$Tleaf,gmin_20 = WBveg$params$gmin20,TPhase = WBveg$params$TPhase_gmin,Q10_1 = WBveg$params$Q10_1_gmin,Q10_2 = WBveg$params$Q10_2_gmin)
  WBveg$Emin <- WBveg$gmin * WBclim$VPD / 101.3 #  [mmol/m2/s] conersion vpd de kPa en Pa ##  en fait c'est VPD/Pa <-- changer si on veut la P
  #WBveg$gminTLAI <- WBveg$gminT*TBA unused
  WBveg$EminT <- WBveg$gminT * WBveg$TBA * WBclim$VPD / 101.3 #  [mmol/m2/s] conersion vpd de kPa en Pa ##  en fait c'est VPD/Pa <-- changer si on veut la P
  WBclim$PAR <- WBclim$PAR*10 #TODO check this !!!! A corriger dans climat
  
  # canopy with no regulation
  WBveg$gs_bound <- calculate_gs_Jarvis(PAR = WBclim$PAR, Tleaf = WBveg$Tleaf)
  gCrown0=45 #TODO gCrown0 hardcoded
  windSpeed = max(0.1, WBclim$WS)
  WBveg$g_crown  = gCrown0*windSpeed^0.6
  WBveg$gBL = TGbl_Leaf[2]
  WBveg$gcanopy_Bound  = 1/(1/WBveg$g_crown+1/WBveg$gs_bound+ 1/WBveg$gBL)
  WBveg$Ebound   = WBveg$gcanopy_Bound * WBclim$VPD / 101.3
  
  #compute current regulation
  regul = computeRegulFact(psi = WBveg$Psi_LSym, params= WBveg$params)
  WBveg$RegulFact = regul$regulFact
  
  # canopy with current regulation 
  WBveg$gs_lim = WBveg$gs_bound * WBveg$RegulFact
  WBveg$gs = WBveg$gs_lim #TODO check why gs and gs_lim ?
  WBveg$gcanopy_lim = 1/(1/WBveg$g_crown+1/WBveg$gs_lim + 1/WBveg$gBL) # NB: gcanopy_lim =0 when gs_lim=0 (1/(1+1/0)=0 in R)
  WBveg$Elim = WBveg$gcanopy_lim * WBclim$VPD / 101.3   
  WBveg$E0 = WBveg$Emin + WBveg$Elim
  gs_lim_prime = WBveg$gs_bound * regul$regulFactPrime
  dbxmin = 1e-100
  WBveg$Eprime = WBveg$Elim * gs_lim_prime /(WBveg$gs_lim*(1+WBveg$gs_lim*(1/WBveg$g_crown+1/WBveg$gBL))+dbxmin)
  return(WBveg) 
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# utilitary function to lineary interpolate climate between clim1 and clim2 (p==0=>res = clim1,p==1=>res = clim2)
# TODO this should be generalized to all usefull clim data...
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
interpClim=function(clim1,clim2,p) {
  res =  clim1
  res$Tair_mean  = (1-p)*clim1$Tair_mean  + p*clim2$Tair_mean
  res$RG         = (1-p)*clim1$RG         + p*clim2$RG
  res$WS         = (1-p)*clim1$WS         + p*clim2$WS
  res$VPD        = (1-p)*clim1$VPD        + p*clim2$VPD
  res$RHair_mean = (1-p)*clim1$RHair_mean + p*clim2$RHair_mean
  res$ETP        = (1-p)*clim1$ETP + p*clim2$ETP
  return(res)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute integration over time - checked FP 11/03/2021
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
computePlantNextTimeStep.WBveg <- function(WBveg, WBsoil, WBclim_current,WBclim_next, Nhours, opt = compOptions) {
  
  # A. LOOP ON THE IMPLICIT SOLVER IN PSI, trying different time steps until results are OK
  regulationWellComputed=F;cavitationWellComputed=F; nwhilecomp=0
  while ((!regulationWellComputed |!cavitationWellComputed) & (nwhilecomp<length(opt$nsmalltimesteps))) { # LOOP TO TRY DIFFERENT TIME STEPS
    WBveg_n = WBveg # initial value of WBveg
    WBsoil_n = WBsoil # initial value of WBsoil
    
    regulationWellComputed=F;cavitationWellComputed=F;nwhilecomp=nwhilecomp+1;deltaRegulMax = 1e-100;deltaPLCMax = 1e-100
    nts = opt$nsmalltimesteps[nwhilecomp] # number of small time steps
    fluxSoilToCollarLargeTimeStep = 0
    fluxEvaporationSoilLargeTimeStep = 0
    for (its in c(1:nts)) { #INTERNAL LOOP ON SMALL TIME STEPS
      p = (its-0.5)/nts;WBclim = interpClim(WBclim_current,WBclim_next,p) # climate at nph
      WBsoil_n <- computeEvaporation_withG.WBsoil(WBsoil = WBsoil_n,ETP = WBveg_n$ETPr,RHair = WBclim$RHair,K = WBveg_n$params$K,LAI = WBveg_n$LAI,Nhours = Nhours/nts)
      fluxEvaporationSoilLargeTimeStep = fluxEvaporationSoilLargeTimeStep + WBsoil_n$E_Soil3/nts
      #WBclim = lapply(seq_along(WBclim_current),function(i) unlist(0.5*(WBclim_current[i])+unlist(WBclim_next[i])))
      WBveg_tmp <- computeTranspiration.WBveg(WBveg_n, WBclim, Nhours) # transpi with climate at nph
      WBveg_np1 <- implicitTemporalIntegrationAt_np1(WBveg_tmp,  WBsoil_n, dt = Nhours * 3600 / nts, opt = compOptions)
      WBveg_np1 <- updateKplant.WBveg(WBveg_np1,WBsoil_n)
      WBveg_np1 <- updateCapaSym.WBveg(WBveg_np1)
      
      # QUANTITIES TO CHECK IF THE RESOLUTION IS OK
      # 1. delta regulation between n and np1
      regul_np1 = computeRegulFact(psi = WBveg_np1$Psi_LSym, params= WBveg_np1$params)
      regul_n = computeRegulFact(psi = WBveg_n$Psi_LSym, params= WBveg_n$params)# TODO check why recomputed? should be in WBveg_tmp
      deltaRegulMax = max(deltaRegulMax,abs(regul_np1$regulFact-regul_n$regulFact))
      # 2. PLC at n and np1
      #print(c(signif(WBveg_np1$PLCAbove, digits = 5),signif(WBveg_n$PLCAbove, digits = 5),signif(WBveg_np1$PLCBelow, digits = 5),signif(WBveg_n$PLCBelow, digits = 5)))
      deltaPLCMax = max(deltaPLCMax,WBveg_np1$PLCAbove-WBveg_n$PLCAbove,WBveg_np1$PLCBelow-WBveg_n$PLCBelow)
      # Update WBveg_n
      WBveg_n = WBveg_np1
      
      
      # 3. update of soil on small time step (done by FP in version 16)
      fluxSoilToCollar = WBveg$kSoilToCollar*(WBsoil_n$PsiSoil-WBveg_np1$Psi_TApo)
      # NB the time step for fluxSoilToCollar.C is Nhours/nts!
      WBveg_np1$fluxSoilToCollar.C = ConvertFluxFrom_mmolm2s_To_mm(fluxSoilToCollar, LAI = WBveg$LAI, timeStep = Nhours/nts) # Quantity from each soil layer to the below part 
      WBsoil_n <- UpdateSoilWater.WBsoil(WBsoil = WBsoil_n, fluxEvap = WBveg_np1$fluxSoilToCollar.C, fluxRelease  = 0) 
      fluxSoilToCollarLargeTimeStep = fluxSoilToCollarLargeTimeStep + fluxSoilToCollar/nts # mean flux over one large time step
    } # end loop small time step
    # TESTS ON RESOLUTION
    WBveg_np1$Diag_deltaRegulMax = deltaRegulMax
    regulationWellComputed = (deltaRegulMax<0.05)
    WBveg_np1$Diag_deltaPLCMax = deltaPLCMax
    cavitationWellComputed = (deltaPLCMax<1) # 1%
    WBveg_np1$Diag_timeStepInHours = Nhours/nts
    
    if (nwhilecomp==length(opt$nsmalltimesteps)&deltaRegulMax>0.05) {
      warning(paste0('regulation inacurate(deltaRegulMax=',signif(deltaRegulMax, digits = 3),'; please reduce the time step, currently=',Nhours/nts))
    }
    if (nwhilecomp==length(opt$nsmalltimesteps)&deltaPLCMax>1) {# 1%
      warning(paste0('water release from cavitation inacurate(deltaPLCMax(%)=',signif(deltaPLCMax, digits = 3),'; please reduce the time step, currently=',Nhours/nts))
    }
  } # end while
  # B. SAVING SOLUTION AT NEXT TIME STEP IN WBveg
  WBveg = WBveg_np1
  
  #  print(paste0("leaving loop, regulfactnp1,",signif(WBveg_np1$RegulFact, digits = 5)))
  WBveg <- computeTranspiration.WBveg(WBveg, WBclim_next, Nhours) # final update of transpiration at clim_next (useful for consistency in outputs, but not required for the computations)
  #  print(paste0("leaving loop2, regulfactfromtranspi,",signif(WBveg$RegulFact, digits = 5)))
  #  print('')
  # C. UPDATING FLUX FROM SOIL (WBveg$fluxSoilToCollar.C is used as input in UpdateSoilWater.WBsoil)
  #fluxSoilToCollar = WBveg$kSoilToCollar*(WBsoil$PsiSoil-WBveg$Psi_TApo)
  #WBveg$SumFluxSoilToCollar <- sum(fluxSoilToCollar) # flux total en mmol/m2/s / used for Tleaf 
  #TODO FP would suggest to move compute fluxSoilToCollar.C directly in the main loop, as it is the coupling between the two models...
  #WBveg$fluxSoilToCollar.C  <- ConvertFluxFrom_mmolm2s_To_mm(fluxSoilToCollar, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part 
  #WBveg$AET.C               <- sum(WBveg$fluxSoilToCollar.C) # total flux
  
  # mean soil quantities on large time steps
  WBveg$SumFluxSoilToCollar <- sum(fluxSoilToCollarLargeTimeStep) # flux total en mmol/m2/s / used for Tleaf 
  WBveg$fluxSoilToCollar.C  <- ConvertFluxFrom_mmolm2s_To_mm(fluxSoilToCollarLargeTimeStep, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part 
  WBveg$AET.C               <- sum(WBveg$fluxSoilToCollar.C) # total flux 
  
  return(WBveg)
}

compOptionsAccurate = list ("nsmalltimesteps"=c(180),"Lsym"=1,"Tsym"=1,"Eord"=1,"Lcav"= 1,"Tcav"= 1,"CLapo"=1,"CTapo"=1)
compOptionsSpecial = list ("nsmalltimesteps"=c(720),"Lsym"=1,"Tsym"=1,"Eord"=1,"Lcav"= 1,"Tcav"= 1,"CLapo"=1,"CTapo"=1)
compOptionsNormal = list ("nsmalltimesteps"=c(6,10,20,60),"Lsym"=1,"Tsym"=1,"Eord"=1,"Lcav"= 1,"Tcav"= 1,"CLapo"=1,"CTapo"=1)
compOptionsFast = list ("nsmalltimesteps"=c(1,6),"Lsym"=1,"Tsym"=1,"Eord"=1,"Lcav"= 1,"Tcav"= 1,"CLapo"=1,"CTapo"=1)
compOptionsFast1 = list ("nsmalltimesteps"=c(1),"Lsym"=1,"Tsym"=1,"Eord"=1,"Lcav"= 1,"Tcav"= 1,"CLapo"=1,"CTapo"=1)
#compOptions = compOptionsAccurate
#compOptions = compOptionsFast1
compOptions=compOptionsNormal
#compOptions=compOptionsSpecial

##################################################################
# Implicit time integration function on small time step dt
# Notes : np1 means "n+1", nph means "n+1/2" (for n plus half)
##################################################################

implicitTemporalIntegrationAt_np1 <- function(WBveg, WBsoil, dt, opt) {
  # 1. Initializing current time step according to computation options (FP)
  dbxmin = 1e-100 # FP minimal double to avoid 0/0
  Psi_LApo_n = WBveg$Psi_LApo
  Psi_TApo_n = WBveg$Psi_TApo
  Psi_LSym_n = WBveg$Psi_LSym
  Psi_TSym_n = WBveg$Psi_TSym
  Psi_LApo_cav = WBveg$Psi_LApo_cav
  Psi_TApo_cav = WBveg$Psi_TApo_cav
  
  K_LSym = opt$Lsym * WBveg$K_LSym 
  K_TSym = opt$Tsym * WBveg$K_TSym 
  C_LSym = opt$Lsym * WBveg$C_LSym 
  C_TSym = opt$Tsym * WBveg$C_TSym 
  
  C_LApo = opt$CLapo * WBveg$C_LApo
  C_TApo = opt$CTapo * WBveg$C_TApo
  
  K_TL = WBveg$kAboveGround
  E_nph = WBveg$Elim # COMPUTED WITH WBveg$Psi_LSym AND INTERPOLATE CLIMATE 
  Eprime_nph = opt$Eord * WBveg$Eprime
  
  Emin_L_nph = WBveg$Emin 
  Emin_T_nph = WBveg$EminT
  
  #Compute K_L_Cav et K_T_Cav
  PLC_prime_L = PLCPrime.comp(WBveg$PLCAbove,WBveg$params$Slope_VC_Above)
  K_L_Cav = -opt$Lcav * WBveg$params$Q_LApo_sat_mmol * PLC_prime_L / dt  # avec WBveg$Q_LSym_sat en l/m2 sol
  PLC_prime_T = PLCPrime.comp(WBveg$PLCBelow,WBveg$params$Slope_VC_Below)
  K_T_Cav = -opt$Tcav * WBveg$params$Q_TApo_sat_mmol * PLC_prime_T / dt  #opt$Tcav * WBveg$K_T_Cav #FP corrected a bug sign here
  
  # 2. While loop in order to decide if cavitation or not :
  # In order to account for the cavitation that occurs only when potentials go below their lowest value "cav" (formerly called "mem" in an earlier version)
  # the following computations are done trying sequentially the resolutions of LApo and TApo eventually activating
  # the appropriate cavitation events when needed (starting assuming no cavit at all...)
  # in case of computational problem, the last case assume no cavitation flux
  
  LcavitWellComputed=F #initialized to false
  TcavitWellComputed=F
  if ((opt$Lcav==0)&(opt$Tcav==0)) { # no cavitation flux computed
    delta_L_cavs=c(0)
    delta_T_cavs=c(0)
  } else if ((opt$Lcav==0)&(opt$Tcav==1)) {# Tcav only
    delta_L_cavs=c(0,0,0)
    delta_T_cavs=c(0,1,0)
  } else if ((opt$Lcav==0)&(opt$Tcav==1)) {# Lcav only
    delta_L_cavs=c(0,1,0)
    delta_T_cavs=c(0,0,0)
  } else { #Lcav=1 and Tcav=1
    delta_L_cavs=c(0,1,0,1,0) # the fifth case is here in case no solution with others...
    delta_T_cavs=c(0,0,1,1,0)
  }
  nwhilecomp = 0 # count the number of step in while loop (if more than 4 no solution and warning)
  while (((!LcavitWellComputed)|(!TcavitWellComputed))&(nwhilecomp<length(delta_L_cavs))) {
    nwhilecomp = nwhilecomp + 1
    delta_L_cav = delta_L_cavs[nwhilecomp]
    delta_T_cav = delta_T_cavs[nwhilecomp]
    # ----------------------
    # 2.1 Compute intermediates
    #print (c(Eprime_nph ,C_LApo/dt , 1/(1/K_LSym + dt/C_LSym) , delta_L_cav*K_L_Cav))
    # compute Psi_L_tilda and K_L_tilda and E_L_tilda
    klsym = kseriesum(K_LSym, C_LSym/dt+0.5 * Eprime_nph) # for Psi_LSym_n
    K_L_td =  C_LApo/dt + klsym + delta_L_cav*K_L_Cav
    Psi_L_td = (C_LApo/dt*Psi_LApo_n + klsym*Psi_LSym_n + delta_L_cav*K_L_Cav*Psi_LApo_cav)/(K_L_td + dbxmin) # dbxmin to avoid 0/0
    E_L_tilda = (E_nph + Emin_L_nph)/(1+(C_LSym/dt+0.5 * Eprime_nph+dbxmin)/K_LSym)
      
    # compute Psi_T_tilda and K_T_tilda and E_T_tilda
    ktsym = kseriesum(K_TSym, C_TSym/dt) # for Psi_TSym_n
    K_T_td = C_TApo/dt + ktsym + sum(WBveg$kSoilToCollar)  + delta_T_cav*K_T_Cav 
    Psi_T_td = (C_TApo/dt*Psi_TApo_n + ktsym*Psi_TSym_n + sum(WBveg$kSoilToCollar * WBsoil$PsiSoil) + delta_T_cav*K_T_Cav*Psi_TApo_cav) / (K_T_td + dbxmin) # dbxmin to avoid 0/0
    E_T_tilda = K_TL/(K_TL + K_T_td) * Emin_T_nph/(1+(C_TSym/dt + dbxmin)/K_TSym)  # dbxmin to avoid 0/0  
    
    # 2.2 Compute Psi_LApo_np1
    Psi_LApo_np1_Num = kseriesum(K_TL , K_T_td + dbxmin)*Psi_T_td + K_L_td*Psi_L_td - (E_L_tilda + E_T_tilda)
    Psi_LApo_np1_Denom = kseriesum(K_TL, K_T_td + dbxmin) + K_L_td + dbxmin # dbxmin to avoid 0/0
    Psi_LApo_np1 = Psi_LApo_np1_Num/Psi_LApo_np1_Denom
    
    # 2.3 Compute Psi_TApo_np1
    Psi_TApo_np1 = ((K_L_td + K_TL)*Psi_LApo_np1 - K_L_td*Psi_L_td + E_L_tilda)/(K_TL+ dbxmin) 
    
    # 2.4 check if cavitation is well computed according to delta_cav, np1 and "cav"
    LcavitWellComputed = (delta_L_cav==(Psi_LApo_np1 < Psi_LApo_cav))|(opt$Lcav==0)
    TcavitWellComputed = (delta_T_cav==(Psi_TApo_np1 < Psi_TApo_cav))|(opt$Tcav==0)
    if ((length(delta_L_cavs)>1)&(nwhilecomp==length(delta_L_cavs))) { # we tried the normal cases and the computation is still not ok so we have done a last one desactivating cavitation water source (delta_cav=0)
      warning(paste0("water flux due to Cavitation ignored with time step, no solution from the implicit solver=",dt))
    }
  } # end of the while loop with check on cavitation options
  WBveg$Diag_nwhile_cavit = nwhilecomp  # Diagnostic step to track cavit event and eventual errors (corresponding to nwhilecomp==5)
  
  # 3. Compute Psi_Symp_np1 (L and T)
  klsym = C_LSym/dt+0.5 * Eprime_nph # for Psi_LSym_n
  Psi_LSym_np1 = (K_LSym*Psi_LApo_np1 + klsym*Psi_LSym_n - (E_nph + Emin_L_nph)) / (K_LSym + klsym + dbxmin) # dbxmin to avoid 0/0
  Psi_TSym_np1 = (K_TSym*Psi_TApo_np1 + C_TSym/dt*Psi_TSym_n - Emin_T_nph) / (K_TSym + C_TSym/dt + dbxmin) # dbxmin to avoid 0/0
  
  #Step 4 : set computed values in WBveg and update Psi_cav, PLC and Psi_AllSoil
  WBveg$Psi_LApo<-Psi_LApo_np1
  WBveg$Psi_TApo<-Psi_TApo_np1
  WBveg$Psi_LSym<-Psi_LSym_np1
  WBveg$Psi_TSym<-Psi_TSym_np1
  
  # Cavitation
  if (WBveg$Psi_LApo < Psi_LApo_cav) {
    WBveg$Psi_LApo_cav = WBveg$Psi_LApo
    WBveg$PLCAbove = PLC.comp(Pmin = WBveg$Psi_LApo, slope = WBveg$params$Slope_VC_Above, P50 = WBveg$params$P50_VC_Above)
  }
  if (WBveg$Psi_TApo < Psi_TApo_cav) {
    WBveg$Psi_TApo_cav = WBveg$Psi_TApo
    WBveg$PLCBelow = PLC.comp(Pmin = WBveg$Psi_TApo, slope = WBveg$params$Slope_VC_Below, P50 = WBveg$params$P50_VC_Below)
  }
  
  WBveg$Psi_AllSoil <- sum (WBveg$kSoilToCollar * WBsoil$PsiSoil)/sum (WBveg$kSoilToCollar)
  return(WBveg)
}

################################################################
# This function computes regulation ie stomatal closure parameters (including derivative from current psi)
# stomatalclosure is 0, 1 or 2 depending on the value of psi in "creneau"
# regulFact is regul proportion between 0 and 1
# regulFactPrime is the first order derivative of regul function in psi
##################################################################
computeRegulFact <- function(psi, params) {
  if(params$Regul=="Creneau") {
    if (psi > params$PsiStartClosing) {
      stomatalClosure = 0
      regulFact = 1
      regulFactPrime = 0
    } else if (psi > params$PsiClose) {
      stomatalClosure = 1
      regulFact = (psi - params$PsiClose)/(params$PsiStartClosing - params$PsiClose)
      regulFactPrime = 1/(params$PsiStartClosing - params$PsiClose)
    } else {
      stomatalClosure = 2
      regulFact = 0
      regulFactPrime = 0
    }
  } else if (params$Regul=="Sigmoid") { 
    Pgs_12=-2.07 #TODO parameters are hard coded here!
    Pgs_88=-2.62
    stomatalClosure = NA #stomatalClosure not relevant ofr sigmoid 
    P50_gs = (Pgs_12 + Pgs_88)/2;
    slope_gs = 100/(Pgs_12-Pgs_88);
    
    PL_gs = 1/(1+exp(slope_gs/25*(psi-P50_gs)))
    regulFact = 1-PL_gs;
    al = slope_gs/25
    regulFactPrime = al * PL_gs * regulFact
    #regulFactPrime = al*exp(-al*(psi-P50_gs)) / ((1+exp(-al*(psi-P50_gs)))^2) # formulation in psi (same but slower to compute)
  }
  return(list("regulFact"=regulFact,"stomatalClosure"=stomatalClosure,"regulFactPrime"=regulFactPrime))
}


#TODO is it the right place for this IO/initialisation?
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create list with all species parameters from configuration file
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
### ////  should be changed to separate stand and vegetation parameters ///
createVegParams <- function(general_params) {
  file <- paste0("../Species_Parameters/Parameters_", general_params$species, ".csv") # hard coded to avoid
  
  if (file.exists(file)) {
    io <- read.csv(file = file, sep = ";", head = T)
  } else {
    stop(paste0("Could not find input parameter file : ", file))
  }
  
  colnames(io) <- c("Name", "Value")
  
  
  .veg_params <- list() # list that contains all the parameters
  
  .veg_params$LAImax <- general_params$LAImax
  
  # setting commomn params for WB_veg (regardless of the options)
  
  params <- c(
    "P50_VC", # [MPa] / Water potential causing 50% Cavitation in the vulnerability curve
    "Slope_VC", # [%/MPa]             / Slope of the vulnerability curve
    "EpsilonSymp", # [MPa]            / Modulus of elasticity in leaves
    "PiFullTurgor", # [MPa]          / Osmotic Potential at full turgor
    "ApoplasmicFrac", # [-]           / Apoplasmic Fraction
    "LDMC", # [mgMS/g]                / Leaf dry matter content (measured for fully watered leaves)
    "LMA", # [g/m2leaf]                   / Leaf mass per area
    "K", # [-]                        / Light extinction coefficient of the vegetation layer
    "SapwoodVolume", # [m3]           / Sapwood volume   / unused [per soil surface ? )
    "WoodDensity", # [-]                / Wood density    /  unused
    "kPlantInit", # [mmol/MPa/s/m2leaf]  / Hydaulic conductance of the plant from soil to leaves
    "gmin20", # [mmol/m2leaf/s]         / Minimum conductance (gmin) at the reference temperature
    "TPhase_gmin", # [degC]            / Temperature for phase transition of minimum conductance
    "Q10_1_gmin", # [-]                 / Q10 value for gmin = f(T) <= Tphase_gmin
    "Q10_2_gmin", # [-]                 / Q10 value for gmin = f(T)  > Tphase_gmin``
    "CanoStorageParam", # [l/m2leaf]    / Depth of water that can be retained by leaves and trunks per unit of leaf area index (used to compute the canopy water storage capacity as a function of LAI)
    "PsiClose", # [MPa]
    "PsiStartClosing" # [MPA]
  )
  
  # "Lv_root", # [m/m3soil]             / Root length per unit soil volume
  # "r_root" # [m]                      / Root radius
  
  
  
  for (i in 1:length(params)) {
    AAA <- which(io$Name == params[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
    {
      stop(paste0("'", params[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", file))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params[i], "' is not provided several times in input vegetation parameter file, correct \n", file))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0(".veg_params$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params[i], "must be numeric"))
      }
    }
  }
  # TODO is it useful to keep this commented lines ? 
  #
  #   ##### Stomatal regulation  ####
  #   AAA <- io[which(io$Name == "StomatalRegModel"), "Value"]
  #   if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
  #     {
  #       stop(paste0("'StomatalRegModel' is not provided in input vegetation parameter file, check presence or spelling"))
  #     } else if (length(AAA) > 1) {
  #     stop(paste0("'", params[i], "'StomatalRefModel' is provided several times in input vegetation parameter file, correct"))
  #   } else if (length(AAA) == 1) { # checking that parameter is numeric in input file /stop running otherwise
  #     .veg_params$StomatalRegModel <- as.character(AAA)
  #   }
  #
  #   if (!.veg_params$StomatalRegModel %in% c("Empirical")) {
  #     stop("Stomatal regulation  should be 'Empirical'. / Other formulation are not yet implemented")
  #   }
  #
  #   if (.veg_params$StomatalRegModel == "Empirical") {
  #     params_Stom <- c(
  #       "PsiClose" # / [MPa]  / Potential at which somatae are fully closed
  #     )
  #     for (i in 1:length(params_Stom))
  #     {
  #       AAA <- which(io$Name == params_Stom[i]) ## line number of the variable
  #
  #       if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
  #         {
  #           stop(paste0("'", params_Stom[i], "' is not provided in input vegetation parameter file, check presence or spelling"))
  #         } else if (length(AAA) > 1) {
  #         stop(paste0("'", params_Stom[i], "' is not provided several times in input vegetation parameter file, correctr"))
  #       } else if (length(AAA) == 1) {
  #         if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
  #           eval(parse(text = paste0(".veg_params$", params_Stom[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
  #         } else {
  #           stop(paste0(params_Stom[i], "must be numeric"))
  #         }
  #       }
  #     }
  #   }
  
  ##### Foliage Type  ####
  AAA <- io[which(io$Name == "Foliage"), "Value"]
  if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
  {
    stop(paste0("'Foliage' is not provided in input vegetation parameter file, check presence or spelling"))
  } else if (length(AAA) > 1) {
    stop(paste0("'", params[i], "'Foliage' is provided several times in input vegetation parameter file, correct"))
  } else if (length(AAA) == 1) { # checking that parameter is numeric in input file /stop running otherwise
    .veg_params$Foliage <- as.character(AAA)
  }
  
  if (!.veg_params$Foliage %in% c("Evergreen", "Deciduous")) {
    stop("'Foliage' should be 'Evergreen' or 'Deciduous' / Check Type of spelling ")
  }
  
  if (.veg_params$Foliage == "Deciduous") {
    params_Decid <- c("Tbase", "Fcrit", "DayStart", "nbdayLAI")
    for (i in 1:length(params_Decid))
    {
      AAA <- which(io$Name == params_Decid[i]) ## line number of the variable
      
      if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
      {
        stop(paste0("'", params_Decid[i], "' is not provided in input vegetation parameter file, check presence or spelling"))
      } else if (length(AAA) > 1) {
        stop(paste0("'", params_Decid[i], "' is not provided several times in input vegetation parameter file, correctr"))
      } else if (length(AAA) == 1) {
        if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
          eval(parse(text = paste0(".veg_params$", params_Decid[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_Decid[i], "must be numeric"))
        }
      }
    }
  }
  
  
  
  # ETP parameters for PT or PM
  if (general_params$ETP.formulation == "PT") {
    params_PT <- c("PTcoeff")
    for (i in 1:length(params_PT))
    {
      AAA <- which(io$Name == params_PT[i]) ## line number of the variable
      
      if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
      {
        stop(paste0("'", params_PT[i], "' is not provided in input vegetation parameter file, check presence or spelling"))
      } else if (length(AAA) > 1) {
        stop(paste0("'", params_PT[i], "' is not provided several times in input vegetation parameter file, correctr"))
      } else if (length(AAA) == 1) {
        if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
          eval(parse(text = paste0(".veg_params$", params_PT[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_PT[i], "must be numeric"))
        }
      }
    }
  }
  
  return(.veg_params)
}



