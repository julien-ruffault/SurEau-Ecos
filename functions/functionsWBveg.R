 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
#           Francois Pimont (francois.pimont@inrae.fr)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create an "object" WBveg from  veg_params
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
new.WBveg <- function(veg_params) {
  
  WBveg <- list() # initialisation
  
  WBveg$params <- veg_params # assign parameters 
  
  WBveg$params$PsiTlp_Leaf  <-  WBveg$params$PiFullTurgor_Leaf*WBveg$params$EpsilonSymp_Leaf/(WBveg$params$PiFullTurgor_Leaf+WBveg$params$EpsilonSymp_Leaf)
  WBveg$params$PsiTlp_Trunk <-  WBveg$params$PiFullTurgor_Trunk*WBveg$params$EpsilonSymp_Trunk/(WBveg$params$PiFullTurgor_Trunk+WBveg$params$EpsilonSymp_Trunk)
  
  
  # potentials 
  WBveg$Psi_LApo = 0
  WBveg$Psi_TApo = 0
  WBveg$Psi_LSym = 0
  WBveg$Psi_TSym = 0 
  WBveg$Psi_LApo_cav = 0 # FP replaced "mem" by "cav" (when cavitation starts)
  WBveg$Psi_TApo_cav = 0
  WBveg$Psi_AllSoil = 0
  #----Conductance & capacitance (mmol/m2/s/MPa) Here on leaf area basis but they are to be updated as a function symplasm conductance and leaf area
  # hydraulic conductances
  WBveg$k_LSym <-  WBveg$params$k_LSymInit  # constant value during simulation   
  WBveg$k_TSym <-  WBveg$params$k_TSymInit  # constant value during simulation 
  WBveg$k_Root <-  NA # is updated in compute.kplant.WBveg
  WBveg$k_TL   <-  NA # is updated in compute.kplant.WBveg
  WBveg$kSoilToCollar <- c("NA","NA","NA") # / conductance rhisophere for each soil layer
  
  #Capacitances 
  #Symplasm capacitance updated as a function of water content and Leaf area (mmol/m2/MPa)
  WBveg$C_LSym = NA 
  WBveg$C_TSym = NA
  
  # For now, the aploplasmic cpacicantes are set to their initial values and not modified trouhgout the simulation. 
  WBveg$C_LApo <- WBveg$params$C_LApoInit     # Capacitance apoplasmique Leaf (mmol/m2 of leaf/s/MPA) ref: Sureau-param-Cochard
  WBveg$C_TApo <- WBveg$params$C_TApoInit     # Capacitance apoplasmique Trunk (mmol/m2 of leaf/s/MPA) ref: Sureau-param-Cochard
  
  
  # Leaf and canopy conductance
  WBveg$gmin <- NA #Gmin for leaves
  WBveg$gmin_T <- WBveg$params$gmin_T #Gmin for trunk and branches
  WBveg$gs_bound <- NA
  WBveg$gs_lim   <- 0 # initialised to 0 to compute Tleaf on first time step considering gs =0 and not NA 
  WBveg$gcanopy_Bound <- NA
  WBveg$gcanopy_lim <- NA
  WBveg$gBL <- NA
  WBveg$gCrown <- NA
  WBveg$regulFact <- 0.01 #  TODO voir si y'a besoin d'initialiser 
  
  
  # Fluxes
  WBveg$Eprime = 0
  WBveg$Emin <- 0
  WBveg$EminT <- 0
  WBveg$Ebound <- 0
  WBveg$Elim <- 0
  WBveg$fluxSoilToCollar_mm <- numeric(3)
  WBveg$transpiration_mm<- 0
  WBveg$Emin_mm <- 0

  
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
  WBveg$PLC_Root     <- 0  # percent loss of conductivity [%]/ 
  WBveg$PLC_TL       <- 0  # percent loss of conductivity [%] /
  
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
  # Q leaf apo 
  WBveg$Q_LApo_sat_mmol  <- 0
  WBveg$Q_LApo_sat_L <- 0

  # Q trunk apo 
  WBveg$Q_TApo_sat_mmol <- 0
  WBveg$Q_TApo_sat_L <- 0

  # Q leaf symplasm
  WBveg$Q_LSym_sat_mmol   <- 0
  WBveg$Q_LSym_sat_L      <- 0

  # Q Trunk symplasm
  WBveg$Q_TSym_sat_mmol   <- 0
  WBveg$Q_TSym_sat_L      <- 0

  
  WBveg$Delta_Q_LApo_mmol_diag <- 0
  
  
  WBveg$F_L_Cav <- 0
  WBveg$F_T_Cav <- 0
 
  #---------------------#

  WBveg$PLC_TL  =  PLC.comp(Pmin = WBveg$Psi_LApo, slope = WBveg$params$slope_VC_TL, P50 = WBveg$params$P50_VC_TL)
  WBveg$PLC_Root = PLC.comp(Pmin = WBveg$Psi_TApo, slope = WBveg$params$slope_VC_Root, P50 = WBveg$params$P50_VC_Root)
  
  # initialize capacitance
  WBveg <- update.capaSym.WBveg(WBveg) 
  # NB conductance are computed after when soil, pheno, etc are done
  
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
      if (temperature > WBveg$params$Tbase & DOY >= WBveg$params$DayStart) {
        WBveg$sumTemperature <- WBveg$sumTemperature + temperature # update SumTemp si T>tbase
      }
      
      if (WBveg$sumTemperature > WBveg$params$Fcrit) {
        WBveg$budburstDate <- DOY
      } # budburstday
    }
    
    else if (DOY >= 280) # perte des feuilles au jour 270
    {
      WBveg$LAIpheno= max(0,WBveg$LAI-max(0, WBveg$params$LAImax / WBveg$params$nbdayLAI))
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
    if (WBveg$PLC_TL > 10){
      WBveg$LAIdead <- max(0, WBveg$LAIpheno *WBveg$PLC_TL / 100) # defoliation in LAI unit
    }else {WBveg$LAIdead = 0}
  }
  
  WBveg$LAI     <-  WBveg$LAIpheno - WBveg$LAIdead

  
  # update LAI-dependent variables 
  WBveg$FCC <- (1 - exp(-WBveg$params$K * WBveg$LAI))
  WBveg$canopyStorageCapacity  <- 1.5 * WBveg$LAI
  
  # update water storing capacities of the dead and living canopy
  WBveg$DMLiveCanopy <- WBveg$LAI * WBveg$params$LMA      # water storing capacities of the living component
  WBveg$DMDeadCanopy <- WBveg$LAIdead * WBveg$params$LMA  # water storing capacities of the dead component
  
  WBveg$Q_LSym_sat_L <- (1 / (WBveg$params$LDMC / 1000) - 1) * WBveg$DMLiveCanopy * (1 - WBveg$params$ApoplasmicFrac_Leaf) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
  WBveg$Q_LSym_sat_mmol <- WBveg$Q_LSym_sat_L*1000000/18
  
  WBveg$Q_LApo_sat_L <- (1 / (WBveg$params$LDMC / 1000) - 1) * WBveg$DMLiveCanopy * (WBveg$params$ApoplasmicFrac_Leaf) / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
  WBveg$Q_LApo_sat_mmol <- WBveg$Q_LApo_sat_L*1000000/18
  
  WBveg$Q_TSym_sat_L <- WBveg$params$VolumeLiving_TRB *WBveg$params$SymplasmicFrac_Trunk
  WBveg$Q_TSym_sat_mmol <- WBveg$Q_TSym_sat_L*1000000/18
  
  WBveg$Q_TApo_sat_L <- WBveg$params$VolumeLiving_TRB*WBveg$params$ApoplasmicFrac_Trunk 
  WBveg$Q_TApo_sat_mmol <- WBveg$Q_TApo_sat_L* 1000000/18
  
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
compute.waterStorage.WBveg <- function(WBveg, VPD) {
  
  # Dead FMC [%]
  WBveg$DFMC <- compute.DFMC(VPD)
  
  #----Symplasmic canopy water content of the leaves----
  RWC_LSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Leaf, Esymp = WBveg$params$EpsilonSymp_Leaf, Pmin = WBveg$Psi_LSym) # Relative water content (unitless)
  Q_LSym <- max(0,  RWC_LSym) * WBveg$Q_LSym_sat_L
  WBveg$LFMCSymp <- 100 * (Q_LSym / (WBveg$DMLiveCanopy * (1 - WBveg$params$ApoplasmicFrac_Leaf) / 1000))
  
  #---Apoplasmic water content-------------------------------------------------
  Q_LApo = (1-WBveg$PLC_TL/100) *  WBveg$Q_LApo_sat_L
  WBveg$LFMCApo <- 100 * (Q_LApo / (WBveg$DMLiveCanopy * WBveg$params$ApoplasmicFrac_Leaf / 1000)) #  LFMC of Apo (relative moisture content to dry mass), gH20/gMS
  
  #------LFMC total---- (Apo+Symp) --------------------------------------------
  WBveg$LFMC <- 100 * (Q_LApo + Q_LSym) / (WBveg$DMLiveCanopy / 1000)
  
  #- FMCcanopy 
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


# --> PLC_Root <- il est fonction du PsitrunkAPO 
# --> PLC_TL   <-  il est fonction du PsiLeaf APO 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# updateKplant
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
update.kplant.WBveg <- function(WBveg,WBsoil) {
  # calculate k_Root and k_TL with cavitation
  WBveg$k_Root = WBveg$params$k_RootInit * (1-WBveg$PLC_Root/100)
  WBveg$k_TL   = WBveg$params$k_TLInit * (1-WBveg$PLC_TL/100)
  
  # Root from root length
  WBveg$kSoilToCollar    <- kseriesum(WBsoil$kSoil, WBveg$k_Root) # conductance from soil to collar (two resitances in series Rsoil and Rroot)

  return(WBveg) 
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# update symplasmic plant capacitances for Trunk and leaves 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
update.capaSym.WBveg <- function(WBveg) {
  dbxmin <- 1e-100 # NM minimal double to avoid-INF

  #----Compute the relative water content of the symplasm----
  RWC_LSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Leaf, Esymp = WBveg$params$EpsilonSymp_Leaf, Pmin = WBveg$Psi_LSym - dbxmin) 
  #----Compute the derivative of the relative water content of the symplasm----
  if (WBveg$Psi_LSym > WBveg$params$PsiTlp_Leaf) { # FP derivative of -Pi0- Eps(1-RWC)+Pi0/RWC
    RWC_LSym_prime <- RWC_LSym / (-WBveg$params$PiFullTurgor_Leaf - WBveg$Psi_LSym - WBveg$params$EpsilonSymp_Leaf + 2 * WBveg$params$EpsilonSymp_Leaf * RWC_LSym)
  } else {
    RWC_LSym_prime <- -WBveg$params$PiFullTurgor_Leaf / WBveg$Psi_LSym^2 # FP derivative of Pi0/Psi
  }
  # Compute the leaf capacitance (mmol/MPa/m2_sol)
  WBveg$C_LSym <- WBveg$Q_LSym_sat_mmol * RWC_LSym_prime

  #----Trunk symplasmic canopy water content----
  RWC_TSym <- 1 - Rs.Comp(PiFT = WBveg$params$PiFullTurgor_Trunk, Esymp = WBveg$params$EpsilonSymp_Trunk, Pmin = WBveg$Psi_TSym - dbxmin) 

  #----Compute the derivative of the relative water content of the symplasm----
  if (WBveg$Psi_TSym > WBveg$params$PsiTlp_Trunk) {
    RWC_TSym_prime <- RWC_TSym / (-WBveg$params$PiFullTurgor_Trunk - WBveg$Psi_TSym - WBveg$params$EpsilonSymp_Trunk + 2 * WBveg$params$EpsilonSymp_Trunk * RWC_TSym)
  }
  else {
    RWC_TSym_prime <- -WBveg$params$PiFullTurgor_Trunk / WBveg$Psi_TSym^2
  }
  # Compute the capacitance (mmol/MPa/m2_sol)
  WBveg$C_TSym <- WBveg$Q_TSym_sat_mmol * RWC_TSym_prime

  return(WBveg)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute transpiration
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.transpiration.WBveg <- function(WBveg, WBclim, Nhours, modeling_options) {
  
# Tleaf is calculated using leaf transpiration from the previous time step
TGbl_Leaf <- compute.Tleaf(
  Tair = WBclim$Tair_mean, SWR = WBclim$RG * 1e6 / (Nhours * 3600), # conversion en W/m
  WS = WBclim$WS, VPD = WBclim$VPD, RH = max(100, WBclim$RHair_mean), gs = WBveg$gs_lim, Einst = max(0.00001, WBveg$SumFluxSoilToCollar)
)

WBveg$leafTemperature <- TGbl_Leaf [1]
  
  # Cuticular conductances and transpiration 
  WBveg$gmin <- calcul.gmin(leafTemperature = WBveg$leafTemperature,gmin_20 = WBveg$params$gmin20,TPhase = WBveg$params$TPhase_gmin,Q10_1 = WBveg$params$Q10_1_gmin,Q10_2 = WBveg$params$Q10_2_gmin)
  WBveg$Emin <- calcul.Emin(gmin = WBveg$gmin, VPD = WBclim$VPD)
  WBveg$EminT <-  WBveg$params$fTRBToLeaf * calcul.Emin(gmin = WBveg$gmin_T,VPD= WBclim$VPD)
  
  #compute current stomatal regulation
  regul = compute.regulFact(psi = WBveg$Psi_LSym, params= WBveg$params, regulationType = modeling_options$stomatalRegFormulation)
  WBveg$regulFact = regul$regulFact
  
  # calculate canopy Transpiration with no regulation
  #if (modeling_options$EboundFormulation == 'Jarvis'){
    WBveg <- calculate.gsJarvis.WBveg(WBveg, PAR = WBclim$PAR) # calculate gs_bound
    WBveg$gCrown  = compute.gCrown(gCrown0 = WBveg$params$gCrown0, windSpeed = WBclim$WS)
    WBveg$gBL = TGbl_Leaf[2]
    WBveg$gcanopy_Bound  = 1/(1/WBveg$gCrown+1/WBveg$gs_bound+ 1/WBveg$gBL)
    WBveg$Ebound   = WBveg$gcanopy_Bound * WBclim$VPD / 101.3
  #}
  
  # else if (modeling_options$EboundFormulation =='Granier1999'){
  #   WBveg$Ebound = calculate.EboundGranier.WBveg(WBveg = WBveg, ETP=WBclim$ETP ,timeStep=Nhours)
  # }
  # 

  # canopy with current regulation 
  WBveg$gs_lim = WBveg$gs_bound * WBveg$regulFact
  WBveg$gcanopy_lim = 1/(1/WBveg$gCrown+1/WBveg$gs_lim + 1/WBveg$gBL) # NB: gcanopy_lim =0 when gs_lim=0 (1/(1+1/0)=0 in R)
  WBveg$Elim = WBveg$gcanopy_lim * WBclim$VPD / 101.3   
  #WBveg$E0 = WBveg$Emin + WBveg$Elim
  gs_lim_prime = WBveg$gs_bound * regul$regulFactPrime
  dbxmin = 1e-100
  WBveg$Eprime = WBveg$Elim * gs_lim_prime /(WBveg$gs_lim*(1+WBveg$gs_lim*(1/WBveg$gCrown+1/WBveg$gBL))+dbxmin)
  return(WBveg) 
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Compute integration over time - checked FP 11/03/2021
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
compute.plantNextTimeStep.WBveg <- function(WBveg, WBsoil, WBclim_current,WBclim_next, Nhours, modeling_options) {
  
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
    fluxSoilToCollarLargeTimeStep = 0
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
      WBveg_np1 <- update.capaSym.WBveg(WBveg_np1)
      
      #browser()
      # QUANTITIES TO CHECK IF THE RESOLUTION IS OK
      # 1. delta regulation between n and np1
      regul_np1 = compute.regulFact(psi = WBveg_np1$Psi_LSym, params = WBveg_np1$params,regulationType=modeling_options$stomatalRegFormulation)
      regul_n   = compute.regulFact(psi = WBveg_n$Psi_LSym  , params = WBveg_n$params  ,regulationType=modeling_options$stomatalRegFormulation)# TODO check why recomputed? should be in WBveg_tmp
      deltaRegulMax = max(deltaRegulMax,abs(regul_np1$regulFact-regul_n$regulFact))
      # 2. PLC at n and np1
      #print(c(signif(WBveg_np1$PLCAbove, digits = 5),signif(WBveg_n$PLCAbove, digits = 5),signif(WBveg_np1$PLCBelow, digits = 5),signif(WBveg_n$PLCBelow, digits = 5)))
      deltaPLCMax = max(deltaPLCMax,WBveg_np1$PLC_TL-WBveg_n$PLC_TL,WBveg_np1$PLC_Root-WBveg_n$PLC_Root)
      # Update WBveg_n
      WBveg_n = WBveg_np1
      
      
      # 3. update of soil on small time step (done by FP in version 16)
      fluxSoilToCollar = WBveg$kSoilToCollar*(WBsoil_n$PsiSoil-WBveg_np1$Psi_TApo)
      # NB the time step for fluxSoilToCollar_mm is Nhours/nts!
      WBveg_np1$fluxSoilToCollar_mm = convertFluxFrom_mmolm2s_To_mm(fluxSoilToCollar, LAI = WBveg$LAI, timeStep = Nhours/nts) # Quantity from each soil layer to the below part 
      WBsoil_n <- update.soilWater.WBsoil(WBsoil = WBsoil_n, fluxEvap = WBveg_np1$fluxSoilToCollar_mm)
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
  

  WBveg <- compute.transpiration.WBveg(WBveg, WBclim_next, Nhours,modeling_options=modeling_options) # final update of transpiration at clim_next (useful for consistency in outputs, but not required for the computations)
  
  #  print('')
  # C. UPDATING FLUX FROM SOIL (WBveg$fluxSoilToCollar_mm is used as input in UpdateSoilWater.WBsoil)
  #fluxSoilToCollar = WBveg$kSoilToCollar*(WBsoil$PsiSoil-WBveg$Psi_TApo)
  #TODO FP suggests moving the computation of  fluxSoilToCollar_mm in the main loop, as it is the coupling between the two models...


  
  # mean soil quantities on large time steps
  WBveg$SumFluxSoilToCollar <- sum(fluxSoilToCollarLargeTimeStep) # flux total en mmol/m2/s / used for Tleaf 

  WBveg$fluxSoilToCollar_mm  <- convertFluxFrom_mmolm2s_To_mm(fluxSoilToCollarLargeTimeStep, LAI = WBveg$LAI, timeStep = Nhours) # Flux from each soil layer to the below part 
  WBveg$transpiration_mm     <- sum(WBveg$fluxSoilToCollar_mm) # total flux in mm 

  return(WBveg)
}


# Implicit time integration function on small time step dt
# Notes : np1 means "n+1", nph means "n+1/2" (for n plus half)
implicit.temporal.integration.atnp1 <- function(WBveg, WBsoil, dt, opt) {
  # 1. Initializing current time step according to computation options (FP)
  dbxmin = 1e-100 # FP minimal double to avoid 0/0
  Psi_LApo_n = WBveg$Psi_LApo
  Psi_TApo_n = WBveg$Psi_TApo
  Psi_LSym_n = WBveg$Psi_LSym
  Psi_TSym_n = WBveg$Psi_TSym
  Psi_LApo_cav = WBveg$Psi_LApo_cav
  Psi_TApo_cav = WBveg$Psi_TApo_cav
  
  K_LSym = opt$Lsym * WBveg$k_LSym   #
  K_TSym = opt$Tsym * WBveg$k_TSym   #
  C_LSym = opt$Lsym * WBveg$C_LSym   #
  C_TSym = opt$Tsym * WBveg$C_TSym   #
  
  C_LApo = opt$CLapo * WBveg$C_LApo #
  C_TApo = opt$CTapo * WBveg$C_TApo #
  
  
  K_TL = WBveg$k_TL #
  E_nph = WBveg$Elim # COMPUTED WITH WBveg$Psi_LSym AND INTERPOLATE CLIMATE 
  Eprime_nph = opt$Eord * WBveg$Eprime
  
  Emin_L_nph = WBveg$Emin 
  Emin_T_nph = WBveg$EminT
  
  #Compute K_L_Cav et K_T_Cav
  PLC_prime_L = PLCPrime.comp(WBveg$PLC_TL,WBveg$params$slope_VC_TL)
  K_L_Cav = -opt$Lcav * WBveg$Q_LApo_sat_mmol * PLC_prime_L / dt  # avec WBveg$Q_LSym_sat en l/m2 sol
  PLC_prime_T = PLCPrime.comp(WBveg$PLC_Root,WBveg$params$slope_VC_Root)
  K_T_Cav = -opt$Tcav * WBveg$Q_TApo_sat_mmol * PLC_prime_T / dt  #opt$Tcav * WBveg$K_T_Cav #FP corrected a bug sign here
  
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
    #browser()
    nwhilecomp = nwhilecomp + 1
    #print(paste0('nwhilecomp=',nwhilecomp)) for debugging 
    delta_L_cav = delta_L_cavs[nwhilecomp]
    delta_T_cav = delta_T_cavs[nwhilecomp]
   
    if (opt$numericalScheme=="Implicit") {
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
    } else if (opt$numericalScheme=="Semi-Implicit") {
      # 2.1 LApo
      alpha = exp(-(K_TL+K_LSym+delta_L_cav*K_L_Cav)/C_LApo*dt)
      Psi_td = (K_TL*Psi_TApo_n + K_LSym*Psi_LSym_n + delta_L_cav*K_L_Cav*Psi_LApo_cav)/(K_TL + K_LSym+delta_L_cav*K_L_Cav + dbxmin) # dbxmin to avoid 0/0
      Psi_LApo_np1 = alpha * Psi_LApo_n +(1-alpha) * Psi_td
        
      # 2.2. TApo
      alpha = exp(-(K_TL+K_TSym + sum(WBveg$kSoilToCollar)+delta_T_cav*K_T_Cav)/C_TApo*dt)
      Psi_td = (K_TL*Psi_LApo_n + K_TSym*Psi_TSym_n + sum(WBveg$kSoilToCollar * WBsoil$PsiSoil)+ delta_T_cav*K_T_Cav*Psi_TApo_cav)/(K_TL + K_TSym+sum(WBveg$kSoilToCollar)+delta_T_cav*K_T_Cav + dbxmin) # dbxmin to avoid 0/0
      Psi_TApo_np1 = alpha * Psi_TApo_n +(1-alpha) * Psi_td
      
    }
      
    # 2.4 check if cavitation is well computed according to delta_cav, np1 and "cav"
    LcavitWellComputed = (delta_L_cav==(Psi_LApo_np1 < Psi_LApo_cav))|(opt$Lcav==0)
    TcavitWellComputed = (delta_T_cav==(Psi_TApo_np1 < Psi_TApo_cav))|(opt$Tcav==0)
    if ((length(delta_L_cavs)>1)&(nwhilecomp==length(delta_L_cavs))) { # we tried the normal cases and the computation is still not ok so we have done a last one desactivating cavitation water source (delta_cav=0)
      warning(paste0("water flux due to Cavitation ignored with time step, no solution from the implicit solver=",dt))
    }
  } # end of the while loop with check on cavitation options
  WBveg$Diag_nwhile_cavit = nwhilecomp  # Diagnostic step to track cavit event and eventual errors (corresponding to nwhilecomp==5)
  
  # 3. Compute Psi_Symp_np1 (L and T)
  if (opt$numericalScheme=="Implicit") {
    klsym = C_LSym/dt+0.5 * Eprime_nph # for Psi_LSym_n
    Psi_LSym_np1 = (K_LSym*Psi_LApo_np1 + klsym*Psi_LSym_n - (E_nph + Emin_L_nph)) / (K_LSym + klsym + dbxmin) # dbxmin to avoid 0/0
    Psi_TSym_np1 = (K_TSym*Psi_TApo_np1 + C_TSym/dt*Psi_TSym_n - Emin_T_nph) / (K_TSym + C_TSym/dt + dbxmin) # dbxmin to avoid 0/0
  } else if (opt$numericalScheme=="Semi-Implicit") {
    alpha = exp(-K_LSym/C_LSym*dt)
    Psi_td = (K_LSym*Psi_LApo_n - (E_nph + Emin_L_nph))/(K_LSym + dbxmin) # dbxmin to avoid 0/0
    Psi_LSym_np1 = alpha * Psi_LSym_n +(1-alpha) * Psi_td
    alpha = exp(-K_TSym/C_TSym*dt)
    Psi_td = (K_TSym*Psi_TApo_n - Emin_T_nph)/(K_TSym + dbxmin) # dbxmin to avoid 0/0
    Psi_TSym_np1 = alpha * Psi_TSym_n +(1-alpha) * Psi_td
  }

  
  #Step 4 : set computed values in WBveg and update Psi_cav, PLC and Psi_AllSoil
  WBveg$Psi_LApo<-Psi_LApo_np1
  WBveg$Psi_TApo<-Psi_TApo_np1
  WBveg$Psi_LSym<-Psi_LSym_np1
  WBveg$Psi_TSym<-Psi_TSym_np1
  
  # Cavitation
  if (WBveg$Psi_LApo < Psi_LApo_cav) {
    WBveg$Psi_LApo_cav = WBveg$Psi_LApo
    WBveg$PLC_TL = PLC.comp(Pmin = WBveg$Psi_LApo, slope = WBveg$params$slope_VC_TL, P50 = WBveg$params$P50_VC_TL)
  }
  if (WBveg$Psi_TApo < Psi_TApo_cav) {
    WBveg$Psi_TApo_cav = WBveg$Psi_TApo
    WBveg$PLC_Root = PLC.comp(Pmin = WBveg$Psi_TApo, slope = WBveg$params$slope_VC_Root, P50 = WBveg$params$P50_VC_Root)
  }
  
  WBveg$Psi_AllSoil <- sum (WBveg$kSoilToCollar * WBsoil$PsiSoil)/sum (WBveg$kSoilToCollar)
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

    if (regulFact == 1 | regulFact == 0) {
      regulFactPrime <- 0
    } else {
      regulFactPrime <- 0 # TODO insert the derivative to compute regulFactPrime
    }
  }
  return(list("regulFact" = regulFact, "stomatalClosure" = stomatalClosure, "regulFactPrime" = regulFactPrime))
}

yearlyInitialisation.WBveg <- function(WBveg){
  
  if(WBveg$PLC_Root >85 | WBveg$PLC_TL>85)
  {
  WBveg <- new.WBveg(WBveg$params) 
  }
  
  WBveg$PLC_Root <- 0 
  WBveg$PLC_TL   <- 0 
  
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
  #print(gs_Jarvis)
  return(WBveg)
}

# calculate Ebound (mmol.m-2.s-1) with Granier formulation 
calculate.EboundGranier.WBveg <- function(WBveg,ETP,timeStep){
  ebound_mm = calculate.Ebound_mm.Granier(ETP = ETP, LAI=WBveg$LAI)
  WBveg$Ebound = ConvertFluxFrom_mm_To_mmolm2s(x = ebound_mm, timeStep= timeStep, LAI =WBveg$LAI)
  return(WBveg)
  }




  
  
