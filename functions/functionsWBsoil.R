# create a list of type WBsoil that contains all variables and parameters 
new.WBsoil <- function(soil_params, initialisation = "Full") {
  WBsoil <- list()
  WBsoil$params <- soil_params # add parameters 

  if (initialisation == "Full") {
    WBsoil$soilWaterStock <- soil_params$V_field_capacity
  }
  WBsoil$soilWaterStock <- soil_params$V_field_capacity
  WBsoil$PsiSoil <- numeric(3)
  WBsoil$kSoil <- numeric(3)
  WBsoil$REW <- numeric(3)
  WBsoil$REWt = numeric(1)

  WBsoil$Evaporation    <- numeric(3)
  WBsoil$EvaporationSum <- numeric(1)

  WBsoil$Drainage <- 0
  return(WBsoil)
}

# Soil Evaporation from ETP /  # compute soil evaporation and update Soil water content in each soil layer
compute.evaporation.WBsoil <- function(WBsoil, ETP, K, LAI,Nhours) {
  # date    : 30/03/2020
  # updated : 05/01/2020 by JR (gamma dependence on Nhours) 
 
  gamma  = WBsoil$params$gamma * Nhours/24
  
  Wdef <- max((WBsoil$params$V_field_capacity[1] - WBsoil$soilWaterStock[1]), 0) # water deficit first layer
  tfict <- (Wdef / gamma)^2
  enumax <- gamma*((sqrt(tfict + 1)) - (sqrt(tfict)))
  etpnu <- ETP * exp(-K * LAI)
  evsonu <- min(enumax, etpnu)
  evsonu <- min(evsonu, WBsoil$soilWaterStock[1])
  evsonuPL <- evsonu * WBsoil$params$Fact_Rich


  # Update soil parameters
  for (nl in 1:3)
  {
    WBsoil$soilWaterStock[nl] <- WBsoil$soilWaterStock[nl] - (evsonuPL[nl])
  }

  WBsoil$Evaporation <- evsonuPL
  WBsoil$EvaporationSum <- sum(WBsoil$Evaporation)


  WBsoil <- compute.soilConductanceAndPsi.WBsoil(WBsoil) # update Conductance and Psi

  return(WBsoil)
}

# compute Evaporation from ETP and Gsoil and update SWS, Psi and in each soil layer 
compute.evaporationG.WBsoil <- function(WBsoil, RHair, Tsoil=20, Nhours, gSoil0 = 30, LAI ,ETP , K) {
# created 03/01/2021 by JR / based on SurEau.C with gsoil0
# such as Esoil = gSoil0 * REW1 * VPDsoil/Patm
  # 
  # WBsoil=NULL
  # WBsoil$REW[1]=1
  # RHair =62.5
  # ETP=1
  # LAI=6
  # K=0.5
  # Tsoil=20
  # gSoil0=30
  # Nhours=1
  
  
  
  VPDsoil=compute.VPDfromRHandT(RHair,Tsoil)
  
  if (Tsoil<0){
    WBsoil$Evaporation <- 0}  # no evaporation from frozen soil
  else{
  g_Soil =    gSoil0*WBsoil$REW[1]
  E_Soil1=    g_Soil*VPDsoil/101.3                                #  VPD effect
  ETP_mmol_s = 10^6*ETP/(3600*Nhours*18)
  E_Soil2    =    (g_Soil/gSoil0)*ETP_mmol_s*exp(-K*LAI)              # limitation by ETP depending on radiation reaching the soil
  E_Soil3 =   min(E_Soil1,E_Soil2)
  WBsoil$Evaporation=     E_Soil3*Nhours*3600*18/10^6             # Conversion from mmol/m2/s to mm      
  }
  #print(paste0('E_Soil3=',E_Soil3))
  
  WBsoil$EvaporationSum <- sum(WBsoil$Evaporation)  
  WBsoil$soilWaterStock[1] <- WBsoil$soilWaterStock[1] - WBsoil$Evaporation
  WBsoil <- compute.soilConductanceAndPsi.WBsoil(WBsoil)

return(WBsoil)  
}
  
# Soil infiltration and update SWS, Psi and K in each soil layer 
compute.infiltration.WBsoil <- function(WBsoil, pptSoil, cstinfil = 0.7) {

  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # initialising variables
  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  LALA <- WBsoil # soilWaterStock/potential/Drainage

  # Output

  DRAIN <- NULL

  ####  Temporary local variables
  SWStemp <- numeric(3) # SWS apres ecoulement in each layer
  SWST1 <- numeric(3)
  transfert <- numeric(3) # Transfert between layers
  Decoul <- numeric(1) # drainage du à l'écoulement
  Dsurc <- numeric(1) # drainage du à l'infiltration immediate

  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Updata soilWaterStocks in the three soil layer  (before precipitation)
  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  # ecoulement entre la première et la deuxième couche
  if (LALA$soilWaterStock[1] > LALA$params$V_field_capacity[1]) {
    transfert[1] <- cstinfil * (LALA$soilWaterStock[1] - LALA$params$V_field_capacity[1])
  } else {
    transfert[1] <- 0
  }

  SWStemp[1] <- LALA$soilWaterStock[1] - transfert[1]
  LALA$soilWaterStock[2] <- LALA$soilWaterStock[2] + transfert[1]


  # ecoulement entre la deuxième et la troisième couche
  if (LALA$soilWaterStock[2] > LALA$params$V_field_capacity[2]) {
    transfert[2] <- cstinfil * (LALA$soilWaterStock[2] - LALA$params$V_field_capacity[2])
  } else {
    transfert[2] <- 0
  }

  SWStemp[2] <- LALA$soilWaterStock[2] - transfert[2]
  LALA$soilWaterStock[3] <- LALA$soilWaterStock[3] + transfert[2]

  # drainage profond (Decoul)
  if (LALA$soilWaterStock[3] > LALA$params$V_field_capacity[3]) {
    transfert[3] <- cstinfil * (LALA$soilWaterStock[3] - LALA$params$V_field_capacity[3])
  } else {
    transfert[3] <- 0
  }

  SWStemp[3] <- LALA$soilWaterStock[3] - transfert[3]
  Decoul <- transfert[3]

  # Updating water stocks (after precipitation)

  SWST1[1] <- SWStemp[1] + pptSoil # Prcecipitioatn

  # fluxes layer1 --> layer 2
  if (SWST1[1] > LALA$params$V_saturation_capacity[1]) {
    SWST1[2] <- SWStemp[2] + (SWST1[1] - LALA$params$V_saturation_capacity[1])
    SWST1[1] <- LALA$params$V_saturation_capacity[1]
  } else {
    SWST1[2] <- SWStemp[2]
  }

  # fluxes layer1 --> layer 2
  if (SWST1[2] > LALA$params$V_saturation_capacity[2]) {
    SWST1[3] <- SWStemp[3] + (SWST1[2] - LALA$params$V_saturation_capacity[2])
    SWST1[2] <- LALA$params$V_saturation_capacity[2]
  } else {
    SWST1[3] <- SWStemp[3]
  }

  # fluxes layer2 --> layer 3
  if (SWST1[3] > LALA$params$V_saturation_capacity[3]) {
    Dsurc <- SWST1[3] - LALA$params$V_saturation_capacity[3]
    SWST1[3] <- LALA$params$V_saturation_capacity[3]
  } else {
    Dsurc <- 0
  }

  # Drainage
  LALA$Drainage <- Decoul + Dsurc
  LALA$soilWaterStock <- SWST1

  LALA <- compute.soilConductanceAndPsi.WBsoil(LALA)

  return(LALA)
}

# Compute conductance and Psi
compute.soilConductanceAndPsi.WBsoil <- function(WBsoil) {

  # Compute soil hydraulic conductivity with Van Genuchten
  if (WBsoil$params$method == "vg") {
    # Soil water holding capacity  (volumetric)
    totalavailwater <- (WBsoil$params$V_saturation_capacity_vg - WBsoil$params$V_residual_capacity_vg)

    # Compute the relative water content (m3 water/m3 soil) based on Water Reserve and soil volume
    actualavailwater <- (WBsoil$soilWaterStock - WBsoil$params$V_residual_capacity_vg)

    REW <- actualavailwater / totalavailwater  #/ numeric [ncouches
    REW[REW <= 0.001] <- 0.001


    KSoil_temp <- REW^(WBsoil$params$I_vg) * (1 - (1 - REW^(1 / WBsoil$params$m))^WBsoil$params$m)^2

    #PsiSoil <- (-1 * ((((1 / REW)^(1 / WBsoil$params$m)) - 1)^(1 / WBsoil$params$n)) / WBsoil$params$alpha_vg / 10000)-0.3
    
    PsiSoil <- (-1 * ((((1 / REW)^(1 / WBsoil$params$m)) - 1)^(1 / WBsoil$params$n)) / WBsoil$params$alpha_vg / 10000)
    # diviser par 10000 pour passer de cm à MPa
  }
  # Compute soil hydraulic conductivity with campbell
  if (WBsoil$params$method == "camp") {
    Ks <- WBsoil$params$Ksat_camp * WBsoil$params$B_GC
    kSoil <- (WBsoil$soilWaterStock / WBsoil$params$V_saturation_capacity_camp)^(WBsoil$params$b_camp * 2 + 2)
    PsiSoil <- -1 * (WBsoil$params$psie * ((WBsoil$soilWaterStock / WBsoil$params$V_saturation_capacity_camp)^-WBsoil$params$b_camp))
    REW <- NA
  }

  # Compute Soil conductance
  WBsoil$kSoil <- 1000 * WBsoil$params$Ksat_vg * WBsoil$params$B_GC * KSoil_temp
  WBsoil$PsiSoil <- PsiSoil
  WBsoil$REW <- REW

  return(WBsoil)
}

# Update soil water reservoirs, potential and psi according to fluxes
update.soilWater.WBsoil <- function(WBsoil, fluxEvap, fluxRelease) {
  WBsoil$soilWaterStock <- WBsoil$soilWaterStock - fluxEvap
  WBsoil$soilWaterStock[1] <- WBsoil$soilWaterStock[1] + fluxRelease # release occur in 1st soil layer only
  WBsoil <- compute.soilConductanceAndPsi.WBsoil(WBsoil)

  WBsoil$REWt =(sum(WBsoil$soilWaterStock-WBsoil$params$V_residual_capacity_vg)) / (sum(WBsoil$params$V_saturation_capacity_vg -WBsoil$params$V_residual_capacity_vg))
  
  
  return(WBsoil)
}
 
# set soil water content to its field capacity 
set.SWCtoFieldCapacity.WBsoil <- function(WBsoil)
{
  WBsoil$soilWaterStock <- WBsoil$params$V_field_capacity
  WBsoil <- compute.soilConductanceAndPsi.WBsoil(WBsoil)
  return(WBsoil)
}





