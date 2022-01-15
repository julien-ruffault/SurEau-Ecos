 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
#           Francois Pimont (francois.pimont@inrae.fr)
### ### ### ### ### ### P### ### ### ### ### ### ### ### ### ### ### ### ### ##


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create an "object" WBveg from  veg_params
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
new.WBveg <- function(veg_params) {
  
  WBveg <- list() # initialisation
  
  WBveg$params <- veg_params # assign parameters 
  
  WBveg$params$PsiTLP_Leaf  <-  WBveg$params$PiFullTurgor_Leaf*WBveg$params$EpsilonSymp_Leaf/(WBveg$params$PiFullTurgor_Leaf+WBveg$params$EpsilonSymp_Leaf)
  WBveg$params$PsiTLP_Stem  <-  WBveg$params$PiFullTurgor_Stem*WBveg$params$EpsilonSymp_Stem/(WBveg$params$PiFullTurgor_Stem+WBveg$params$EpsilonSymp_Stem)
  
  # potentials 
  WBveg$Psi_LApo = 0
  WBveg$Psi_SApo = 0
  WBveg$Psi_LSym = 0
  WBveg$Psi_SSym = 0 
  WBveg$Psi_LApo_cav = 0 # FP replaced "mem" by "cav" (when cavitation starts)
  WBveg$Psi_SApo_cav = 0
  WBveg$Psi_AllSoil = 0
  #----Conductance & capacitance (mmol/m2/s/MPa) Here on leaf area basis but they are to be updated as a function symplasm conductance and leaf area
  # hydraulic conductances
  WBveg$k_Plant <-  WBveg$params$k_PlantInit  # constant value during simulation  
  WBveg$k_LSym  <-  WBveg$params$k_LSymInit  # constant value during simulation   
  WBveg$k_SSym  <-  WBveg$params$k_SSymInit  # constant value during simulation 
  WBveg$k_RSApo    <-  NA # is updated in compute.kplant.WBveg
  WBveg$k_SLApo    <-  NA # is updated in compute.kplant.WBveg
  WBveg$k_SoilToStem <- c("NA","NA","NA") # / conductance rhisophere for each soil layer
  
  #Capacitances 
  #Symplasm capacitance updated as a function of water content and Leaf area (mmol/m2leaf/MPa) /updated in update.capacitancesSymAndApo()  (NM : 25/10/2021)
  WBveg$C_LSym = NA 
  WBveg$C_SSym = NA
  
  #Symplasm capacitance are initialised per m2sol and are updated acording to LAI for conversion in m2leaf (updated in update.capacitancesSymAndApo())  (NM : 25/10/2021)
  WBveg$C_LApo = NA 
  WBveg$C_SApo = NA
  
  # Leaf and canopy conductance
  WBveg$gmin <- 0 # initialised at 0 to compute Tleaf on first time step considering gs =0 and not NA s
  WBveg$gmin_S <- WBveg$params$gmin_S #Gmin for stem and branches
  WBveg$regulFact <- 0.01 #  TODO voir si y'a besoin d'initialiser 
  
  # TODO voir pour mettre tout en NA si TranspirationMod = 0 
  WBveg$gs_bound <- NA
  WBveg$gs_lim   <- 0 # initialised to 0 to compute Tleaf on first time step considering gs = 0 and not NA 
  WBveg$gcanopy_bound <- NA
  WBveg$gcanopy_lim <- NA
  WBveg$gBL <- NA
  WBveg$gCrown <- NA

  # Fluxes
  WBveg$Eprime = 0
  WBveg$Emin <- 0
  WBveg$Emin_S <- 0
  WBveg$Ebound <- 0
  WBveg$Elim <- 0
  WBveg$fluxSoilToStem <- numeric(3)
  WBveg$transpiration_mm<- 0
  WBveg$Emin_mm <- 0
  WBveg$Emin_S_mm <- 0
  
  # LAI and LAI-dependent variables
  WBveg$LAIpheno <- numeric(1)
  WBveg$LAI      <- numeric(1)    
  WBveg$canopyStorageCapacity <- numeric(1)
  
  # rainfall and interception 
  WBveg$pptSoil  <- 0 # ppt that reach the soil
  WBveg$interceptedWaterAmount <- 0 # interceptedWater /quantite d'eau dans la canopee
  WBveg$evaporationIntercepted <- 0
  WBveg$ETPr <- 0
  
  WBveg$defoliation <- 0 # defoliation // no defoliation (add an option to set defoliation due to cavitation of the Plant Above)
  WBveg$LAIdead <- 0
  
  # Cavitation
  WBveg$PLC_Leaf     <- 0  # percent loss of conductivity [%]/ 
  WBveg$PLC_Stem       <- 0  # percent loss of conductivity [%] /
  
  # leaf temp
  WBveg$leafTemperature <- NA
  
  #-- pheno--
  # parameters if deciduous
  if (veg_params$Foliage == "Deciduous") {
    WBveg$LAIpheno       <- 0
    WBveg$sumTemperature <- 0 # temmpeerature sum to determine budburst
    WBveg$budburstDate   <- NA # budburst date
  }
  
  # water storage in canopy
  WBveg$DFMC     <- NA # Fuel moisture content of the dead compartment (gH20/gMS)
  WBveg$LFMCApo  <- NA # Live fuel moisture content of the apoplasmic compartment (gH20/gMS)
  WBveg$LFMCSymp <- NA # Live fuel moisture content of the apoplasmic compartment (gH20/gMS)
  WBveg$LFMC     <- NA # live fuel moisture content (gH20/gMS)
  
  WBveg$DMLiveCanopy    <- NA # Live Canopy dry matter [gMS/m2 soil]
  WBveg$DMDeadCanopy    <- 0 # Dead Canopy dry matter [gMS/m2 soil]
  
  #---------------------# 
  # Q leaf apo (mol/m2leaf)
  WBveg$Q_LApo_sat_mmol  <- 0
  WBveg$Q_LApo_sat_L <- 0

  # Q stem apo  (mol/m2leaf)
  WBveg$Q_SApo_sat_mmol <- 0
  WBveg$Q_SApo_sat_L <- 0

  # Q leaf symplasm (mol/m2leaf)
  WBveg$Q_LSym_sat_mmol   <- 0
  WBveg$Q_LSym_sat_L      <- 0

  # Q Stem symplasm (mol/m2leaf)
  WBveg$Q_SSym_sat_mmol   <- 0
  WBveg$Q_SSym_sat_L      <- 0

  # Q Stem and Leaf apo and symp in liter/kg TODO 13/08/2021: better in mmol?
  WBveg$Q_LApo_L      <- 0
  WBveg$Q_SApo_L      <- 0
  WBveg$Q_LSym_L      <- 0
  WBveg$Q_SSym_L      <- 0
  
  WBveg$Delta_Q_LApo_mmol_diag <- 0
  
  WBveg$F_L_Cav <- 0
  WBveg$F_S_Cav <- 0
 
  #---------------------#

  WBveg$PLC_Leaf  =  PLC.comp(Pmin = WBveg$Psi_LApo, slope = WBveg$params$slope_VC_Leaf, P50 = WBveg$params$P50_VC_Leaf)
  WBveg$PLC_Stem = PLC.comp(Pmin = WBveg$Psi_SApo, slope = WBveg$params$slope_VC_Stem, P50 = WBveg$params$P50_VC_Stem)
  
  return(WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute pheno and LAI for an object WBveg in SUREAU_ECOS
### ### ### ### ### ### ### ### ### ### ## ### ### ### ### ### ### ### ### ##
compute.pheno.WBveg <- function(WBveg, temperature, DOY) {
  
  # INPUTS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  #   - WBveg
  #   - temperature    :  daily tempeature  (degC)
  #   - DOY            :  day of year
  #  -  LAImod=T
  ### ### ### ### ### ### ### ### ###
  # major changed 26/01/2021 JR/ simplified to give just LAIpheno as output (not final LAI that combines both LAI    
  
  # set to initial parameters at the beginning of the year 
  if (DOY==1){
    if (WBveg$params$Foliage == "Evergreen") {
      WBveg$LAIpheno <- WBveg$params$LAImax  # note  : for evergreen this values will remain the same untill the end of the year ! 
    }
    else if (WBveg$params$Foliage == "Deciduous") {
      WBveg$LAIpheno <- 0
      WBveg$sumTemperature <- 0
      WBveg$budburstDate   <- NA
    }
  }
  
  if (WBveg$params$Foliage == "Deciduous") # si decidus LAI =fonction(pheno)
  {
    if (is.na(WBveg$budburstDate) == T) # si pas de debourrement
    {
      
      
      if (temperature > WBveg$params$Tbase & DOY >= WBveg$params$dayStart) {
        WBveg$sumTemperature <- WBveg$sumTemperature + temperature # update SumTemp si T>tbase
      }
      
      if (WBveg$sumTemperature > WBveg$params$Fcrit) {
        WBveg$budburstDate <- DOY
      } # budburstday
    }
    
    else if (DOY >= 280) # perte des feuilles au jour 270
    {
      WBveg$LAIpheno = max(0, WBveg$LAI - max(0, WBveg$params$LAImax / WBveg$params$nbdayLAI))
    }
    
    else if (is.na(WBveg$budburstDate) == F) {
      if (DOY < WBveg$budburstDate + WBveg$params$nbdayLAI) # si debourrement et que periode de construction des feuilles
      {
        WBveg$LAIpheno <- WBveg$LAIpheno + max(0, WBveg$params$LAImax / WBveg$params$nbdayLAI) #
      }
    }
    
  }

  return(WBveg)
}

# update LAI as a function of LAIpheno and caviation + update LAI dependent parameters 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
updateLAIandStocks.WBveg <- function(WBveg,modeling_options) {
  
  if (modeling_options$defoliation == F) # cavitation does not affect LAI
  {WBveg$LAIdead = 0 }
  
  else if (modeling_options$defoliation == T) # cvitation affect LAI 
  {
    #  leaf shedding because of cavitation  // starts only if PLCabove > 10 % 
    if (WBveg$PLC_Leaf > 10){
      WBveg$LAIdead <- max(0, WBveg$LAIpheno * WBveg$PLC_Leaf / 100) # defoliation in LAI unit
    }else {WBveg$LAIdead = 0}
  }
  
  WBveg$LAI     <-  WBveg$LAIpheno - WBveg$LAIdead

  
  # update LAI-dependent variables 
  WBveg$FCC <- (1 - exp(-WBveg$params$K * WBveg$LAI))
  WBveg$canopyStorageCapacity  <- 1.5 * WBveg$LAI 
  
  # update water storing capacities of the dead and living canopy
  WBveg$DMLiveCanopy <- WBveg$LAI * WBveg$params$LMA      # water storing capacities of the living component
  WBveg$DMDeadCanopy <- WBveg$LAIdead * WBveg$params$LMA  # water storing capacities of the dead component
  
  WBveg$Q_LSym_sat_L <- (1 / (WBveg$params$LDMC / 1000) - 1) * WBveg$DMLiveCanopy * (1 - WBveg$params$apoplasmicFrac_Leaf) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
  WBveg$Q_LSym_sat_mmol <- WBveg$Q_LSym_sat_L*1000000/18
  if(WBveg$LAI==0) {WBveg$Q_LSym_sat_mmol_perLeafArea <- 0} else {WBveg$Q_LSym_sat_mmol_perLeafArea <- WBveg$Q_LSym_sat_mmol / max(1,WBveg$LAI)  } # modified by NM (10/12/2021)
  
  WBveg$Q_SSym_sat_L <- WBveg$params$Vol_Stem *WBveg$params$SymplasmicFrac_Stem
  WBveg$Q_SSym_sat_mmol <- WBveg$Q_SSym_sat_L*1000000/18
  WBveg$Q_SSym_sat_mmol_perLeafArea <- WBveg$Q_SSym_sat_mmol / max(1,WBveg$LAI)  #  used max(1,LAI) to avoid that Q_SSym_sat_mmol_perLeafArea--> inF when LAI --> 0 (limit imposed by computing water fluxes by m2leaf) 
  

  WBveg$Q_LApo_sat_L <- (1 / (WBveg$params$LDMC / 1000) - 1) * WBveg$DMLiveCanopy * (WBveg$params$apoplasmicFrac_Leaf) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
  WBveg$Q_LApo_sat_mmol <- WBveg$Q_LApo_sat_L*1000000/18
  if(WBveg$LAI==0) {WBveg$Q_LApo_sat_mmol_perLeafArea <- 0} else {WBveg$Q_LApo_sat_mmol_perLeafArea <- WBveg$Q_LApo_sat_mmol / max(1,WBveg$LAI)  } # modified by NM (10/12/2021)
  

  WBveg$Q_SApo_sat_L <- WBveg$params$Vol_Stem*WBveg$params$ApoplasmicFrac_Stem 
  WBveg$Q_SApo_sat_mmol <- WBveg$Q_SApo_sat_L* 1000000/18
  WBveg$Q_SApo_sat_mmol_perLeafArea = WBveg$Q_SApo_sat_mmol / max(1,WBveg$LAI) #used max(1,LAI) to avoid that Q_SApo_sat_mmol_perLeafArea--> inF when LAI --> 0 (limit imposed by computing water fluxes by m2leaf)  modified by NM (25/10/2021)
  
  
  WBveg <- update.capacitancesApoAndSym.WBveg(WBveg)
  
  return(WBveg)
  
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Rain interception by canopy/ stock of water in the canopy reservoir (one vegetation layer only)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.interception.WBveg <- function(WBveg, ppt) {
  
  # inputs : 
  #   - WB.veg         : object WBveg 
  #   -  ppt          :  precipitation (mm)
  
  if (ppt * WBveg$FCC <= (WBveg$canopyStorageCapacity  - WBveg$interceptedWaterAmount )) # pas de débordement
  {
    WBveg$pptSoil <- ppt * (1 - WBveg$FCC)
    WBveg$interceptedWaterAmount  <- WBveg$interceptedWaterAmount  + (ppt * WBveg$FCC)
  }
  
  else if (ppt * WBveg$FCC > (WBveg$canopyStorageCapacity  - WBveg$interceptedWaterAmount )) #  debordement
  {
    WBveg$pptSoil <- ppt - (WBveg$canopyStorageCapacity  - WBveg$interceptedWaterAmount )
    WBveg$interceptedWaterAmount  <- WBveg$canopyStorageCapacity 
  }
  
  return(WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute water stocks in leaves/wood in SUREAU_ECOS (one vegetation layer only)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.waterStorage.WBveg <- function(WBveg, VPD)
{
  
  
  #----Symplasmic canopy water content of the leaves----
  RWC_LSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Leaf, Esymp = WBveg$params$EpsilonSymp_Leaf, Pmin = WBveg$Psi_LSym) # Relative water content (unitless)
  Q_LSym <- max(0,  RWC_LSym) * WBveg$Q_LSym_sat_L
  WBveg$Q_LSym_L <- Q_LSym
  WBveg$LFMCSymp <- 100 * (Q_LSym / (WBveg$DMLiveCanopy * (1 - WBveg$params$apoplasmicFrac_Leaf) / 1000))
  
  #---Apoplasmic water content of the leaves-------------------------------------------------
  Q_LApo = (1-WBveg$PLC_Leaf/100) *  WBveg$Q_LApo_sat_L
  WBveg$Q_LApo_L <- Q_LApo
  WBveg$LFMCApo <- 100 * (Q_LApo / (WBveg$DMLiveCanopy * WBveg$params$apoplasmicFrac_Leaf / 1000)) #  LFMC of Apo (relative moisture content to dry mass), gH20/gMS
  
  #------LFMC leaf total---- (Apo+Symp) --------------------------------------------
  WBveg$LFMC <- 100 * (Q_LApo + Q_LSym) / (WBveg$DMLiveCanopy / 1000)
  
  #----Symplasmic canopy water content of the stem----
  RWC_SSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Stem, Esymp = WBveg$params$EpsilonSymp_Stem, Pmin = WBveg$Psi_SSym) # Relative water content (unitless)
  Q_SSym <- max(0,  RWC_SSym) * WBveg$Q_SSym_sat_L
  WBveg$Q_SSym_L <- Q_SSym
  
  #----Apoplasmic water content of the stem-----
  Q_SApo = (1-WBveg$PLC_Stem/100) *  WBveg$Q_SApo_sat_L
  WBveg$Q_SApo_L <- Q_SApo
  
  
  #- FMCcanopy
  # Dead FMC [%]
  WBveg$DFMC <- compute.DFMC(VPD)
  # Water quantity of dead foliage (l/m2 sol ou mm)
  Q_LDead <- (WBveg$DFMC / 100) * WBveg$DMDeadCanopy / 1000
  WBveg$FMCCanopy <- 100 * (Q_LApo + Q_LSym + Q_LDead) / (WBveg$DMLiveCanopy / 1000 + WBveg$DMDeadCanopy / 1000)
  
  return(WBveg)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Compute evaporation of the water intercepted by the canopy in the SUREAU_ECOS model
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.evapoIntercepted.WBveg <- function(WBveg, ETP) {
  if (ETP >= 0) {
    if (ETP >= WBveg$interceptedWaterAmount) { #  all the water intercepted by the canopy is tranpired
      WBveg$ETPr <- ETP - WBveg$interceptedWaterAmount 
      WBveg$evaporationIntercepted <- WBveg$interceptedWaterAmount 
    }
    WBveg$QR <- 0 # empty the reservoir
  } else if (ETP < WBveg$QR) { # Energie pas suffisante pour evaporer l'eau interceptée
    WBveg$interceptedWaterAmount  <- WBveg$interceptedWaterAmount  - ETP
    WBveg$ETPr <- 0
    WBveg$evaporationIntercepted <- WBveg$interceptedWaterAmount  - ETP
  }
  return(WBveg)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# updateKplant
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
update.kplant.WBveg <- function(WBveg, WBsoil) {
  
  # calculate k_RSApo and k_SLApo with cavitation
  WBveg$k_RSApo   = WBveg$params$k_RSApoInit * (1-WBveg$PLC_Stem/100)
  
  WBveg$k_SLApo   = WBveg$params$k_SLApoInit * (1-WBveg$PLC_Leaf/100)
  
  # Root from root length
  WBveg$k_SoilToStem    <- kseriesum(WBsoil$kSoil, WBveg$k_RSApo) # conductance from soil to collar (two resistances in series Rsoil and Rroot)
  
  # Compute k_plant (from root to leaf) for diagnostic only
  WBveg$k_Plant <-  1/ (1 /sum(WBveg$k_RSApo) + 1/WBveg$k_SLApo + 1/WBveg$k_LSym)
  
  return(WBveg) 
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# update symplasmic plant capacitances for Trunk and leaves 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
update.capacitancesApoAndSym.WBveg <- function(WBveg) {
  dbxmin <- 1e-100 # NM minimal double to avoid-INF

  #----Compute the relative water content of the symplasm----
  RWC_LSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Leaf, Esymp = WBveg$params$EpsilonSymp_Leaf, Pmin = WBveg$Psi_LSym - dbxmin) 
  #----Compute the derivative of the relative water content of the symplasm----
  if (WBveg$Psi_LSym > WBveg$params$PsiTLP_Leaf) { # FP derivative of -Pi0- Eps(1-RWC)+Pi0/RWC
    RWC_LSym_prime <- RWC_LSym / (-WBveg$params$PiFullTurgor_Leaf - WBveg$Psi_LSym - WBveg$params$EpsilonSymp_Leaf + 2 * WBveg$params$EpsilonSymp_Leaf * RWC_LSym)
  } else {
    RWC_LSym_prime <- -WBveg$params$PiFullTurgor_Leaf / WBveg$Psi_LSym^2 # FP derivative of Pi0/Psi
  }
  # Compute the leaf capacitance (mmol/MPa/m2_sol)
  #WBveg$C_LSym <- WBveg$Q_LSym_sat_mmol * RWC_LSym_prime
  if (WBveg$LAI==0){ WBveg$C_LSym = 0 }else{ WBveg$C_LSym <- WBveg$Q_LSym_sat_mmol_perLeafArea * RWC_LSym_prime} # changed 25/10/2021 by NM 
 
  
  #----Stem symplasmic canopy water content----
  RWC_SSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Stem, Esymp = WBveg$params$EpsilonSymp_Stem, Pmin = WBveg$Psi_SSym - dbxmin) 

  #----Compute the derivative of the relative water content of the symplasm----
  if (WBveg$Psi_SSym > WBveg$params$PsiTLP_Stem) {
    RWC_SSym_prime <- RWC_SSym / (-WBveg$params$PiFullTurgor_Stem - WBveg$Psi_SSym - WBveg$params$EpsilonSymp_Stem + 2 * WBveg$params$EpsilonSymp_Stem * RWC_SSym)
  }
  else {
    RWC_SSym_prime <- -WBveg$params$PiFullTurgor_Stem / WBveg$Psi_SSym^2
  }
  # Compute the capacitance (mmol/MPa/m2_leaf)
  #WBveg$C_SSym <- WBveg$Q_SSym_sat_mmol * RWC_SSym_prime
  WBveg$C_SSym <- WBveg$Q_SSym_sat_mmol_perLeafArea * RWC_SSym_prime #  changed 25/10/2021 by NM. --> Stem capacitance per leaf area can only decrease with LAI (cannot increase when LAI<1 )
  

  WBveg$C_SApo = WBveg$params$C_SApoInit 
  WBveg$C_LApo = WBveg$params$C_LApoInit 
  
  
  return(WBveg)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute transpiration
# needed values for the rest of the code  : 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.transpiration.WBveg <- function(WBveg, WBclim, Nhours, modeling_options) {

  if (modeling_options$transpirationModel == 'Granier') {
  
    
    Einst= WBveg$Elim + WBveg$Emin
    
    TGbl_Leaf <- compute.Tleaf(Tair=WBclim$Tair_mean, 
                               PAR=WBclim$PAR, 
                               POTENTIAL_PAR = WBclim$Potential_PAR,
                               WS = WBclim$WS, 
                               RH = WBclim$RHair_mean,
                               Einst =Einst,
                               PsiLeaf =WBveg$Psi_LSym,  
                               leaf_size=50, 
                               leaf_angle=45, 
                               TurnOffEB=F,
                               transpirationModel = modeling_options$transpirationModel) 
  
      
    WBveg$leafTemperature <-TGbl_Leaf [1]
    WBveg$gBL = TGbl_Leaf[2]
    WBveg$leafVPD = TGbl_Leaf[3]
    
    WBveg$Ebound = calculate.Ebound.Granier(ETP = WBclim$ETP,LAI = WBveg$LAI,timeStep = Nhours)
    
    # Cuticular conductances and transpiration 
    WBveg$gmin  <- compute.gmin(leafTemperature = WBveg$leafTemperature,gmin_20 = WBveg$params$gmin20,TPhase = WBveg$params$TPhase_gmin,Q10_1 = WBveg$params$Q10_1_gmin,Q10_2 = WBveg$params$Q10_2_gmin)
    #TODO : ATTENTION PAS DE CALCUL DE GCROWN ici....:
    WBveg$Emin  <- WBveg$gmin* WBveg$leafVPD /101.3
    WBveg$Emin_S <-  WBveg$params$fTRBToLeaf * WBveg$gmin_S* WBveg$leafVPD /101.3
    #compute current stomatal regulation
    regul = compute.regulFact(psi = WBveg$Psi_LSym, params= WBveg$params, regulationType = modeling_options$stomatalRegFormulation)
    WBveg$regulFact = regul$regulFact
    WBveg$Elim = WBveg$Ebound * WBveg$regulFact
    dbxmin = 1e-100
    WBveg$Eprime = WBveg$Ebound * regul$regulFactPrime + dbxmin
    
    } 
  else if (modeling_options$transpirationModel=='Jarvis')
      {
  # calculate Tleaf, leafVPD and gBL 
  Einst= WBveg$Elim + WBveg$Emin
  
  # calculate Tleaf, leafVPD and gBL 
  TGbl_Leaf <- compute.Tleaf(Tair=WBclim$Tair_mean, 
                             PAR=WBclim$PAR, 
                             POTENTIAL_PAR = WBclim$Potential_PAR,
                             WS = WBclim$WS, 
                             RH = WBclim$RHair_mean,
                             gs = WBveg$gs_lim, 
                             g_cuti = WBveg$gmin,
                             PsiLeaf =WBveg$Psi_LSym,  
                             leaf_size=50, 
                             leaf_angle=45, 
                             TurnOffEB=F,
                             transpirationModel = modeling_options$transpirationModel) 
  

  WBveg$leafTemperature <-TGbl_Leaf [1]
  WBveg$gBL = TGbl_Leaf[2]
  WBveg$leafVPD = TGbl_Leaf[3]
  
  
  
  # calculate gcrown 
  WBveg$gCrown  = compute.gCrown(gCrown0 = WBveg$params$gCrown0, windSpeed = WBclim$WS)
  
  # Cuticular conductances and transpiration 
  WBveg$gmin <- compute.gmin(leafTemperature = WBveg$leafTemperature,gmin_20 = WBveg$params$gmin20,TPhase = WBveg$params$TPhase_gmin,Q10_1 = WBveg$params$Q10_1_gmin,Q10_2 = WBveg$params$Q10_2_gmin)
  WBveg$Emin <- compute.Emin(gmin = WBveg$gmin, gBL=WBveg$gBL, gCrown = WBveg$gCrown, VPD = WBveg$leafVPD)
  WBveg$Emin_S <-  WBveg$params$fTRBToLeaf * compute.Emin(gmin = WBveg$gmin_S,gBL=WBveg$gBL, gCrown=WBveg$gCrown, VPD= WBclim$VPD)
  #compute current stomatal regulation
  regul = compute.regulFact(psi = WBveg$Psi_LSym, params= WBveg$params, regulationType = modeling_options$stomatalRegFormulation)
  WBveg$regulFact = regul$regulFact
  
  # calculate canopy Transpiration with no regulation
  #if (modeling_options$EboundFormulation == 'Jarvis'){
  WBveg <- calculate.gsJarvis.WBveg(WBveg, PAR = WBclim$PAR) # calculate gs_bound
  WBveg$gcanopy_bound  = 1/(1/WBveg$gCrown+1/WBveg$gs_bound+ 1/WBveg$gBL)
  WBveg$Ebound   = WBveg$gcanopy_bound * WBveg$leafVPD / 101.3
  #}
  
  # else if (modeling_options$EboundFormulation =='Granier1999'){
  #   WBveg$Ebound = calculate.EboundGranier.WBveg(WBveg = WBveg, ETP=WBclim$ETP ,timeStep=Nhours)
  # }
  # 

  # canopy with current regulation 
  WBveg$gs_lim = WBveg$gs_bound * WBveg$regulFact
  WBveg$gcanopy_lim = 1/(1/WBveg$gCrown+1/WBveg$gs_lim + 1/WBveg$gBL) # NB: gcanopy_lim =0 when gs_lim=0 (1/(1+1/0)=0 in R)
  WBveg$Elim = WBveg$gcanopy_lim * WBveg$leafVPD/ 101.3   
  #WBveg$E0 = WBveg$Emin + WBveg$Elim
  gs_lim_prime = WBveg$gs_bound * regul$regulFactPrime
  dbxmin = 1e-100
  WBveg$Eprime = WBveg$Elim * gs_lim_prime /(WBveg$gs_lim*(1+WBveg$gs_lim*(1/WBveg$gCrown+1/WBveg$gBL))+dbxmin)
  
  
  # update Tleaf according to new conductance to avoid large gaps (comestic)
  TGbl_Leaf <- compute.Tleaf(Tair=WBclim$Tair_mean, 
                             PAR=WBclim$PAR, 
                             POTENTIAL_PAR = WBclim$Potential_PAR,
                             WS = WBclim$WS, 
                             RH = WBclim$RHair_mean,
                             gs = WBveg$gs_lim, 
                             g_cuti = WBveg$gmin,
                             PsiLeaf =WBveg$Psi_LSym,  
                             leaf_size=50, 
                             leaf_angle=45, 
                             TurnOffEB=F,
                             transpirationModel = modeling_options$transpirationModel) 
  
  WBveg$leafTemperature <-TGbl_Leaf [1]
  WBveg$gBL = TGbl_Leaf[2]
  WBveg$leafVPD = TGbl_Leaf[3]
  

  
  }
  
  return(WBveg) 
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute integration over time - checked FP 11/03/2021
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.plantNextTimeStep.WBveg <- function(WBveg, WBsoil, WBclim_current,WBclim_next, Nhours, modeling_options, WBoutput) {
  
  opt=modeling_options$compOptions
  # A. LOOP ON THE IMPLICIT SOLVER IN PSI, trying different time steps until results are OK
  regulationWellComputed=F
  cavitationWellComputed=F 
  nwhilecomp=0
  while ((!regulationWellComputed |!cavitationWellComputed) & (nwhilecomp<length(opt$nsmalltimesteps))) { # LOOP TO TRY DIFFERENT TIME STEPS
    WBveg_n = WBveg # initial value of WBveg
    WBsoil_n = WBsoil # initial value of WBsoil
    
    regulationWellComputed=F;cavitationWellComputed=F;nwhilecomp=nwhilecomp+1;deltaRegulMax = 1e-100;deltaPLCMax = 1e-100
    nts = opt$nsmalltimesteps[nwhilecomp] # number of small time steps
    fluxSoilToStemLargeTimeStep = 0
    fluxEvaporationSoilLargeTimeStep = 0
    for (its in c(1:nts)) { #INTERNAL LOOP ON SMALL TIME STEPS
      p = (its-0.5)/nts
      WBclim = interp.WBclim(WBclim_current,WBclim_next,p) # climate at nph
      WBsoil_n <- compute.evaporationG.WBsoil(WBsoil = WBsoil_n,ETP = WBveg_n$ETPr,Tair = WBclim$Tair_mean, RHair = WBclim$RHair,K = WBveg_n$params$K,LAI = WBveg_n$LAI,Nhours = Nhours/nts)
      fluxEvaporationSoilLargeTimeStep = fluxEvaporationSoilLargeTimeStep + WBsoil_n$E_Soil3/nts
      #WBclim = lapply(seq_along(WBclim_current),function(i) unlist(0.5*(WBclim_current[i])+unlist(WBclim_next[i])))
      WBveg_tmp <- compute.transpiration.WBveg(WBveg_n, WBclim, Nhours, modeling_options) # transpi with climate at nph
      WBveg_np1 <- implicit.temporal.integration.atnp1(WBveg_tmp,  WBsoil_n, dt = Nhours * 3600 / nts, opt = opt)
      WBveg_np1 <- update.kplant.WBveg(WBveg_np1,WBsoil_n)
      WBveg_np1 <- update.capacitancesApoAndSym.WBveg(WBveg_np1)
      
      #browser()
      # QUANTITIES TO CHECK IF THE RESOLUTION IS OK
      # 1. delta regulation between n and np1
      regul_np1 = compute.regulFact(psi = WBveg_np1$Psi_LSym, params = WBveg_np1$params,regulationType=modeling_options$stomatalRegFormulation)
      regul_n   = compute.regulFact(psi = WBveg_n$Psi_LSym  , params = WBveg_n$params  ,regulationType=modeling_options$stomatalRegFormulation)# TODO check why recomputed? should be in WBveg_tmp
      deltaRegulMax = max(deltaRegulMax,abs(regul_np1$regulFact-regul_n$regulFact))
      # 2. PLC at n and np1
      deltaPLCMax = max(deltaPLCMax,WBveg_np1$PLC_Leaf-WBveg_n$PLC_Leaf,WBveg_np1$PLC_Stem-WBveg_n$PLC_Stem)
      WBveg_n = WBveg_np1 # Update WBveg_n
      
      # 3. update of soil on small time step (done by FP in version 16)
      fluxSoilToStem = WBveg$k_SoilToStem*(WBsoil_n$PsiSoil-WBveg_np1$Psi_SApo)
      # NB the time step for fluxSoilToStem is Nhours/nts!
      WBveg_np1$fluxSoilToStem = convertFluxFrom_mmolm2s_To_mm(fluxSoilToStem, LAI = WBveg$LAI, timeStep = Nhours/nts) # Quantity from each soil layer to the below part 
      WBsoil_n <- update.soilWater.WBsoil(WBsoil = WBsoil_n, fluxEvap = WBveg_np1$fluxSoilToStem)
      fluxSoilToStemLargeTimeStep = fluxSoilToStemLargeTimeStep + fluxSoilToStem/nts # mean flux over one large time step
      
      # if (opt$numericalScheme == "Explicit" ) {
      #   write.WBoutput(Date = NA, WBoutput = WBoutput, WBsoil = WBsoil, WBveg = WBveg_n, WBclim = WBclim_next)
      # }
      
      
    } # end loop small time step
    # TESTS ON RESOLUTION
    WBveg_np1$Diag_deltaRegulMax = deltaRegulMax
    regulationWellComputed = (deltaRegulMax<0.05)
    WBveg_np1$Diag_deltaPLCMax = deltaPLCMax
    cavitationWellComputed = (deltaPLCMax<1) # 1%
    WBveg_np1$Diag_timeStepInHours = Nhours/nts
    
    # if (nwhilecomp==length(opt$nsmalltimesteps)&deltaRegulMax>0.05) {
    #   warning(paste0('regulation inacurate(deltaRegulMax=',signif(deltaRegulMax, digits = 3),'; please reduce the time step, currently=',Nhours/nts))
    # }
    # if (nwhilecomp==length(opt$nsmalltimesteps)&deltaPLCMax>1) {# 1%
    #   warning(paste0('water release from cavitation inacurate(deltaPLCMax(%)=',signif(deltaPLCMax, digits = 3),'; please reduce the time step, currently=',Nhours/nts))
    # }
  } # end while
  # B. SAVING SOLUTION AT NEXT TIME STEP IN WBveg
  WBveg = WBveg_np1
  
  WBveg <- compute.transpiration.WBveg(WBveg, WBclim_next, Nhours,modeling_options=modeling_options) # final update of transpiration at clim_next (useful for consistency in outputs, but not required for the computations)
  

  # C. UPDATING FLUX FROM SOIL (WBveg$fluxSoilToStem is used as input in UpdateSoilWater.WBsoil)
  #TODO FP suggests moving the computation of  fluxSoilToStem in the main loop, as it is the coupling between the two models...

  # mean soil quantities on large time steps
  WBveg$Emin_mm  = convertFluxFrom_mmolm2s_To_mm(WBveg$Emin, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part 
  WBveg$Emin_S_mm = convertFluxFrom_mmolm2s_To_mm(WBveg$Emin_S, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part 
  
  WBveg$SumFluxSoilToStem <- sum(fluxSoilToStemLargeTimeStep) # flux total en mmol/m2/s / used for Tleaf 
  WBveg$fluxSoilToStem_mm  <- convertFluxFrom_mmolm2s_To_mm(fluxSoilToStemLargeTimeStep, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part  in mm
  WBveg$transpiration_mm     <- convertFluxFrom_mmolm2s_To_mm((WBveg$Emin + WBveg$Emin_S + WBveg$Elim),LAI = WBveg$LAI, timeStep = Nhours) # total flux in mm 

  return(WBveg)
}


# Implicit time integration function on small time step dt
# Notes : np1 means "n+1", nph means "n+1/2" (for n plus half)
implicit.temporal.integration.atnp1 <- function(WBveg, WBsoil, dt, opt) {
  # 1. Initializing current time step according to computation options (FP)
  dbxmin = 1e-100 # FP minimal double to avoid 0/0
  Psi_LApo_n = WBveg$Psi_LApo
  Psi_SApo_n = WBveg$Psi_SApo
  Psi_LSym_n = WBveg$Psi_LSym
  Psi_SSym_n = WBveg$Psi_SSym
  Psi_LApo_cav = WBveg$Psi_LApo_cav
  Psi_SApo_cav = WBveg$Psi_SApo_cav
  
  K_LSym = opt$Lsym * WBveg$k_LSym   #
  K_SSym = opt$Ssym * WBveg$k_SSym   #
  C_LSym = opt$Lsym * WBveg$C_LSym   #
  C_SSym = opt$Ssym * WBveg$C_SSym   #
  
  C_LApo = opt$CLapo * WBveg$C_LApo #
  C_SApo = opt$CTapo * WBveg$C_SApo #
  
  
  K_SL = WBveg$k_SLApo #
  E_nph = WBveg$Elim # COMPUTED WITH WBveg$Psi_LSym AND INTERPOLATE CLIMATE 
  Eprime_nph = opt$Eord * WBveg$Eprime
  
  Emin_L_nph = WBveg$Emin 
  Emin_S_nph = WBveg$Emin_S
  

  
  #Compute K_L_Cav et K_S_Cav
  
  PLC_prime_L = PLCPrime.comp(WBveg$PLC_Leaf,WBveg$params$slope_VC_Leaf)
  #K_L_Cav = -opt$Lcav * WBveg$Q_LApo_sat_mmol * PLC_prime_L / dt  # avec WBveg$Q_LSym_sat en l/m2 sol
  K_L_Cav = -opt$Lcav * WBveg$Q_LApo_sat_mmol_perLeafArea * PLC_prime_L / dt  # avec WBveg$Q_LSym_sat en l/m2 sol # changed by NM (25/10/2021)
  PLC_prime_S = PLCPrime.comp(WBveg$PLC_Stem,WBveg$params$slope_VC_Stem)
  #K_S_Cav = -opt$Scav * WBveg$Q_SApo_sat_mmol * PLC_prime_S / dt  #opt$Scav * WBveg$K_S_Cav #FP corrected a bug sign here
  K_S_Cav = -opt$Scav * WBveg$Q_SApo_sat_mmol_perLeafArea * PLC_prime_S / dt  #opt$Scav * WBveg$K_S_Cav #FP corrected a bug sign herehanged by NM (25/10/2021)
  
  
  # 2. While loop in order to decide if cavitation or not :
  # In order to account for the cavitation that occurs only when potentials go below their lowest value "cav" (formerly called "mem" in an earlier version)
  # the following computations are done trying sequentially the resolutions of LApo and TApo eventually activating
  # the appropriate cavitation events when needed (starting assuming no cavit at all...)
  # in case of computational problem, the last case assume no cavitation flux
  LcavitWellComputed=F #initialized to false
  ScavitWellComputed=F
  if ((opt$Lcav==0)&(opt$Scav==0)) { # no cavitation flux computed
    delta_L_cavs=c(0)
    delta_S_cavs=c(0)
  } else if ((opt$Lcav==0)&(opt$Scav==1)) {# Scav only
    delta_L_cavs=c(0,0,0)
    delta_S_cavs=c(0,1,0)
  } else if ((opt$Lcav==0)&(opt$Scav==1)) {# Lcav only
    delta_L_cavs=c(0,1,0)
    delta_S_cavs=c(0,0,0)
  } else { #Lcav=1 and Scav=1
    delta_L_cavs=c(0,1,0,1,0) # the fifth case is here in case no solution with others...
    delta_S_cavs=c(0,0,1,1,0)
  }
  if (opt$numericalScheme=="Explicit"){ # in case of explicit we do the computation only once
    delta_L_cavs=c(opt$Lcav)
    delta_S_cavs=c(opt$Scav)
  }
  nwhilecomp = 0 # count the number of step in while loop (if more than 4 no solution and warning)
  while (((!LcavitWellComputed)|(!ScavitWellComputed))&(nwhilecomp<length(delta_L_cavs))) {
    #browser()
    nwhilecomp = nwhilecomp + 1
    #print(paste0('nwhilecomp=',nwhilecomp)) for debugging 
    delta_L_cav = delta_L_cavs[nwhilecomp]
    delta_S_cav = delta_S_cavs[nwhilecomp]
   
    if (opt$numericalScheme=="Implicit") {
      # 2.1 Compute intermediates

      # compute Psi_L_tilda and K_L_tilda and E_L_tilda
      klsym = kseriesum(K_LSym, C_LSym/dt+0.5 * Eprime_nph) # for Psi_LSym_n
      K_L_td =  C_LApo/dt + klsym + delta_L_cav*K_L_Cav
      Psi_L_td = (C_LApo/dt*Psi_LApo_n + klsym*Psi_LSym_n + delta_L_cav*K_L_Cav*Psi_LApo_cav)/(K_L_td + dbxmin) # dbxmin to avoid 0/0
      E_L_tilda = (E_nph + Emin_L_nph)/(1+(C_LSym/dt+0.5 * Eprime_nph+dbxmin)/K_LSym)
      
      # compute Psi_S_tilda and K_S_tilda and E_S_tilda
      kssym = kseriesum(K_SSym, C_SSym/dt) # for Psi_SSym_n
      K_S_td = C_SApo/dt + kssym + sum(WBveg$k_SoilToStem)  + delta_S_cav*K_S_Cav 
      Psi_S_Td = (C_SApo/dt*Psi_SApo_n + kssym*Psi_SSym_n + sum(WBveg$k_SoilToStem * WBsoil$PsiSoil) + delta_S_cav*K_S_Cav*Psi_SApo_cav) / (K_S_td + dbxmin) # dbxmin to avoid 0/0
      E_S_tilda = K_SL/(K_SL + K_S_td) * Emin_S_nph/(1+(C_SSym/dt + dbxmin)/K_SSym)  # dbxmin to avoid 0/0  
      
      # 2.2 Compute Psi_LApo_np1
      Psi_LApo_np1_Num = kseriesum(K_SL , K_S_td + dbxmin)*Psi_S_Td + K_L_td*Psi_L_td - (E_L_tilda + E_S_tilda)
      Psi_LApo_np1_Denom = kseriesum(K_SL, K_S_td + dbxmin) + K_L_td + dbxmin # dbxmin to avoid 0/0
      Psi_LApo_np1 = Psi_LApo_np1_Num/Psi_LApo_np1_Denom
      
      # 2.3 Compute Psi_SApo_np1
      Psi_SApo_np1 = ((K_L_td + K_SL)*Psi_LApo_np1 - K_L_td*Psi_L_td + E_L_tilda)/(K_SL+ dbxmin) 
    } else if (opt$numericalScheme=="Semi-Implicit") {
      # 2.1 LApo
      alpha = exp(-(K_SL+K_LSym+delta_L_cav*K_L_Cav)/C_LApo*dt)
      Psi_td = (K_SL*Psi_SApo_n + K_LSym*Psi_LSym_n + delta_L_cav*K_L_Cav*Psi_LApo_cav)/(K_SL + K_LSym+delta_L_cav*K_L_Cav + dbxmin) # dbxmin to avoid 0/0
      Psi_LApo_np1 = alpha * Psi_LApo_n +(1-alpha) * Psi_td
        
      # 2.2. SApo
      alpha = exp(-(K_SL+K_SSym + sum(WBveg$k_SoilToStem)+delta_S_cav*K_S_Cav)/C_SApo*dt)
      Psi_td = (K_SL*Psi_LApo_n + K_SSym*Psi_SSym_n + sum(WBveg$k_SoilToStem * WBsoil$PsiSoil)+ delta_S_cav*K_S_Cav*Psi_SApo_cav)/(K_SL + K_SSym+sum(WBveg$k_SoilToStem)+delta_S_cav*K_S_Cav + dbxmin) # dbxmin to avoid 0/0
      Psi_SApo_np1 = alpha * Psi_SApo_n +(1-alpha) * Psi_td
      
    } else if (opt$numericalScheme=="Explicit"){
      # NB for cavitation we use the min(cav-current) because here cav is at time n-1
      # psi L_apo
      # if (Psi_LApo_cav > Psi_LApo_n) {
      #   print(paste0("Lcav",opt$Lcav))
      # }
      # if (Psi_SApo_cav > Psi_SApo_n) {
      #   print(paste0("Scav",opt$Scav))
      # }
      Psi_LApo_np1 = Psi_LApo_n + (dt/C_LApo) * (K_SL * (Psi_SApo_n - Psi_LApo_n) + K_LSym * (Psi_LSym_n - Psi_LApo_n) + delta_L_cav*K_L_Cav * max(Psi_LApo_cav - Psi_LApo_n,0))
      # psi T_apo
      Psi_SApo_np1 = Psi_SApo_n + (dt/C_SApo) * (K_SL * (Psi_LApo_n - Psi_SApo_n) + K_SSym * (Psi_SSym_n - Psi_SApo_n) + delta_S_cav*K_S_Cav * max(Psi_SApo_cav - Psi_SApo_n,0) + sum(WBveg$k_SoilToStem * (WBsoil$PsiSoil - Psi_SApo_n)))
      
    # determine the cfl for each cell 
      cfl_LApo = C_LApo/(2*max(K_SL,K_LSym,K_L_Cav))
      #cfl_LApo = C_LApo/(2*max(K_SL,K_LSym))
      cfl_SApo = C_SApo/(2*max(K_SL,K_SSym,K_S_Cav,max(WBveg$k_SoilToStem)))
    
  
    }
      
    # 2.4 check if cavitation is well computed according to delta_cav, np1 and "cav"
    LcavitWellComputed = (delta_L_cav==(Psi_LApo_np1 < Psi_LApo_cav))|(opt$Lcav==0)
    ScavitWellComputed = (delta_S_cav==(Psi_SApo_np1 < Psi_SApo_cav))|(opt$Scav==0)
    if ((length(delta_L_cavs)>1)&(nwhilecomp==length(delta_L_cavs))) { # we tried the normal cases and the computation is still not ok so we have done a last one desactivating cavitation water source (delta_cav=0)
      warning(paste0("water flux due to Cavitation ignored with time step, no solution from the implicit solver=",dt))
    }
  } # end of the while loop with check on cavitation options
  WBveg$Diag_nwhile_cavit = nwhilecomp  # Diagnostic step to track cavit event and eventual errors (corresponding to nwhilecomp==5)
  
  # 3. Compute Psi_Symp_np1 (L and S)
  if (opt$numericalScheme=="Implicit") {
    klsym = C_LSym/dt+0.5 * Eprime_nph # for Psi_LSym_n
    Psi_LSym_np1 = (K_LSym*Psi_LApo_np1 + klsym*Psi_LSym_n - (E_nph + Emin_L_nph)) / (K_LSym + klsym + dbxmin) # dbxmin to avoid 0/0
    Psi_SSym_np1 = (K_SSym*Psi_SApo_np1 + C_SSym/dt*Psi_SSym_n - Emin_S_nph) / (K_SSym + C_SSym/dt + dbxmin) # dbxmin to avoid 0/0
  } else if (opt$numericalScheme=="Semi-Implicit") {
    alpha = exp(-K_LSym/C_LSym*dt)
    Psi_td = (K_LSym*Psi_LApo_n - (E_nph + Emin_L_nph))/(K_LSym + dbxmin) # dbxmin to avoid 0/0
    Psi_LSym_np1 = alpha * Psi_LSym_n +(1-alpha) * Psi_td
    alpha = exp(-K_SSym/C_SSym*dt)
    Psi_td = (K_SSym*Psi_SApo_n - Emin_S_nph)/(K_SSym + dbxmin) # dbxmin to avoid 0/0
    Psi_SSym_np1 = alpha * Psi_SSym_n +(1-alpha) * Psi_td
  } else if (opt$numericalScheme=="Explicit"){
    # psi L_Sym_np1 and cfl

    if(C_LSym==0){ Psi_LSym_np1 = ((K_LSym * Psi_LApo_n) - E_nph - Emin_L_nph)/(K_LSym ) # cas stationnaire lorsque C_Lsymp=0
       cfl_LSym = 10
    }else { 
      Psi_LSym_np1 =  Psi_LSym_n + (dt/(C_LSym)) * (K_LSym * (Psi_LApo_n - Psi_LSym_n) - E_nph - Emin_L_nph)
    cfl_LSym = C_LSym/(2*K_LSym)}
   
    # psi T_Sym_np1 and cfls
    if (C_SSym==0){Psi_SSym_np1 = ((K_SSym * Psi_SApo_n) - Emin_S_nph)/(K_SSym )# cas stationnaire lorsque C_Lsymp=0
    cfl_SSym = 10 # cfl non limitante
    }else{Psi_SSym_np1 =  Psi_SSym_n + (dt/(C_SSym)) * (K_SSym * (Psi_SApo_n - Psi_SSym_n) - Emin_S_nph)
    cfl_SSym = C_SSym/(2*K_SSym)}
  

    # print(paste0("cfl_LSym = ",cfl_LSym))
    # print(paste0("cfl_SSym = ",cfl_SSym))
    # print(paste0("cfl_LApo = ",cfl_LApo))
    # print(paste0("cfl_SApo = ",cfl_SApo))
    
    cfl_all = min(cfl_LSym,cfl_SSym,cfl_LApo,cfl_SApo)
    #print(paste0("cfl_all = ",cfl_all))
    # if (cfl_all<=dt)
    #   {
    #   stop('execution stopped because cfl<dt')
    #   }
    }

  
  #Step 4 : set computed values in WBveg and update Psi_cav, PLC and Psi_AllSoil
  WBveg$Psi_LApo<-min(-0.00001,Psi_LApo_np1)
  WBveg$Psi_SApo<-min(-0.00001,Psi_SApo_np1)
  WBveg$Psi_LSym<-min(-0.00001,Psi_LSym_np1)
  WBveg$Psi_SSym<-min(-0.00001,Psi_SSym_np1)
  
  # Cavitation
  if (opt$numericalScheme=="Explicit"){
    psiref = min(-0.00001,Psi_LApo_n)  # the reference is at previous time step
  } else {
    psiref = WBveg$Psi_LApo  # the reference is at current time step for other modes
  }
  if (psiref < Psi_LApo_cav) {
    WBveg$Psi_LApo_cav = psiref
    WBveg$PLC_Leaf = PLC.comp(Pmin = psiref, slope = WBveg$params$slope_VC_Leaf, P50 = WBveg$params$P50_VC_Leaf)
  }
  if (opt$numericalScheme=="Explicit"){
    psiref = min(-0.00001,Psi_SApo_n)  # the reference is at previous time step
  } else {
    psiref = WBveg$Psi_SApo  # the reference is at current time step for other modes
  }
  if (psiref < Psi_SApo_cav) {
    WBveg$Psi_SApo_cav = psiref
    WBveg$PLC_Stem = PLC.comp(Pmin = psiref, slope = WBveg$params$slope_VC_Stem, P50 = WBveg$params$P50_VC_Stem)
  }
  
 # browser()
  WBveg$Psi_AllSoil <- sum (WBveg$k_SoilToStem * WBsoil$PsiSoil)/sum (WBveg$k_SoilToStem)
  return(WBveg)
}

# This function computes regulation ie stomatal closure parameters (including derivative from current psi)
# stomatalclosure is 0, 1 or 2 depending on the value of psi in "PiecewieLinear"
# regulFact is regul proportion between 0 and 1
# regulFactPrime is the first order derivative of regul function in psi
compute.regulFact <- function(psi, params, regulationType) {
  if (regulationType == "PiecewiseLinear") {
    if (psi > params$PsiStartClosing) {
      stomatalClosure <- 0
      regulFact <- 1
      regulFactPrime <- 0
    } else if (psi > params$PsiClose) {
      stomatalClosure <- 1
      regulFact <- (psi - params$PsiClose) / (params$PsiStartClosing - params$PsiClose)
      regulFactPrime <- 1 / (params$PsiStartClosing - params$PsiClose)
    } else {
      stomatalClosure <- 2
      regulFact <- 0
      regulFactPrime <- 0
    }
  } else if (regulationType == "Sigmoid") {
    stomatalClosure <- NA # stomatalClosure not relevant ofr sigmoid
    PL_gs <- 1 / (1 + exp(params$slope_gs / 25 * (psi - params$P50_gs)))
    regulFact <- 1 - PL_gs
    al <- params$slope_gs / 25
    regulFactPrime <- al * PL_gs * regulFact
    # regulFactPrime = al*exp(-al*(psi-P50_gs)) / ((1+exp(-al*(psi-P50_gs)))^2) # formulation in psi (same but slower to compute)
  } else if (regulationType == "Turgor") {
    stomatalClosure <- NA # stomatalClosure not relevant for Turgor
    rs <- Rs.Comp(PiFT = params$PiFullTurgor_Leaf, Esymp = params$EpsilonSymp_Leaf, Pmin = psi)
    turgor <- turgor.comp(PiFT = params$PiFullTurgor_Leaf, Esymp = params$EpsilonSymp_Leaf, Rstemp = rs)
    regulFact <- max(0, min(1, turgor / params$turgorPressureAtGsMax))
    #regulFact <- max(0, min(1, turgor / params$minTurgorAtGsMax))

    if (regulFact == 1 | regulFact == 0) {
      regulFactPrime <- 0
    } else {
      regulFactPrime <- 0 # TODO insert the derivative to compute regulFactPrime
    }
  }
  return(list("regulFact" = regulFact, "stomatalClosure" = stomatalClosure, "regulFactPrime" = regulFactPrime))
}

yearlyInitialisation.WBveg <- function(WBveg){
  
  if(WBveg$PLC_Stem >85 | WBveg$PLC_Leaf >85)
  {
  WBveg <- new.WBveg(WBveg$params) 
  }
  
  WBveg$PLC_Stem  <- 0 
  WBveg$PLC_Leaf   <- 0 
  
return(WBveg)
  }
  
# stomatal conductance calculation with Jarvis type formulations
calculate.gsJarvis.WBveg <- function(WBveg, PAR, Ca=400, option =1){
  
  if (option==1) # temperature effect on gs 
  {
    gsMax2    = max(0,WBveg$params$gsMax/(1+((WBveg$leafTemperature-WBveg$params$Tgs_optim)/WBveg$params$Tgs_sens)^2))
    gsNight2  = max(0,WBveg$params$gsNight/(1+((WBveg$leafTemperature-WBveg$params$Tgs_optim)/WBveg$params$Tgs_sens)^2))
  }

  #if (option==7){
  # gsMax2 = (gsMax*(1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((leafTemperature-Tgs_optim)/Tgs_sens,2))  # 
  # gsNight2 = (gs_night* (1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((T_Leaf-Tgs_optim)/Tgs_sens,2));
  #}
  
  WBveg$gs_bound=   gsNight2 + (gsMax2-gsNight2)*(1-exp(-WBveg$params$JarvisPAR*PAR))
  return(WBveg)
}





  
  
