### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Authors : Julien Ruffault (julien.ruff@gmail.com)
#                  &
#           Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#           date   : 30/03/2020 (V0)
#                  : 26/11/2020 (V1) update create_soil_params to read from file
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

new_WBsoil <- function(soil_params, general_params, initialisation = "Full", method = "VG") {

  # method= VG for Van Genuchten, or Camp for Campbell

  WBsoil <- list()

  if (initialisation == "Full") {
    WBsoil$SoilWaterStock <- soil_params$V_field_capacity
  }
  WBsoil$SoilWaterStock <- soil_params$V_field_capacity
  WBsoil$PsiSoil <- numeric(3)
  WBsoil$kSoil <- numeric(3)
  # WBsoil$Conductivity <- numeric(3)
  WBsoil$REW <- numeric(3)
  WBsoil$REWt = numeric(1)

  WBsoil$Evaporation <- numeric(3)
  WBsoil$EvaporationSum <- numeric(1)

  WBsoil$Drainage <- 0

  WBsoil$params <- soil_params

  return(WBsoil)
}


createSoilParams <- function(general_params, filePath, depths = c(0.3, 1, 4), default_soil = F, method = "vg", veg_params) {
  
  warning("if run on puechabon : add an Offset  on psisoil (-0.3) to match observations --> / modify  in function 'computeSoilConductanceAndPsi.WBsoil'  ") 
  
  .soilParams <- list()
  .soilParams$method <- method
  .soilParams$layer_thickness <- numeric(3)
  .soilParams$layer_thickness[1] <- depths[1]
  .soilParams$layer_thickness[2] <- depths[2] - depths[1]
  .soilParams$layer_thickness[3] <- depths[3] - depths[2]

  .soilParams$Fact_Rich <- c(0.7, 0.2, 0.1) # Ritchie parameter to distribute soil evaporation among soils layers


  if (default_soil == T) # default if no file is provided
    {
      .soilParams$rock_fragment_content <- c(40, 80, 93) # coarse elements (stones/rocks) in each layer (%)

      # derniers reglages sol sur Puechabon
      .soilParams$rock_fragment_content <- c(40, 75, 90)

      #--------------
      # A calculer pour diagnostique
      .soilParams$field_capacity <- c(0.4, 0.4, 0.4) # Fraction of water at field capacity (cm3/cm3)
      .soilParams$wilting_point <- c(0.2, 0.2, 0.2) # Fraction of water at wilting point (cm3/cm3)

      # Ricthie parameters
      .soilParams$gamma <- 1 # Ritchie parameter

      # Van Genuchten parameters
      .soilParams$alpha_vg <- rep(0.0035, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
      .soilParams$n_vg <- rep(1.55, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$I_vg <- rep(0.5, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$Ksat_vg <- rep(1.69, 3) # Soil conductivity at saturation (mol/m/s/Mpa)
      .soilParams$saturation_capacity_vg <- c(0.5, 0.5, 0.5) # Fraction of water at saturation capacity (cm3/cm3)
      .soilParams$residual_capacity_vg <- c(0.1, 0.1, 0.1) # Fraction of residual water  (cm3/cm3)



      # Campbell parameters
      # .soilParams$b_camp <- rep(6, 3) # exponent (Campbell 1974)
      # .soilParams$psie_camp <- rep(0.025, 3) # desequilibrium potential (Campbell 1974)
      # .soilParams$Ksat_camp <- rep(2.27, 3) # desequilibrium potential (Campbell 1974)
      # .soilParams$saturation_capacity_camp <- c(0.5, 0.5, 0.5) # Fraction of water at saturaction capacity (cm3/cm3)
      #
      #  ROOT parameters for gardner-cowan model ////  will be changed to be laoded from veg params
      # .soilParams$La=c(3960,3960,3960)  # A changer ici m/m2 de soil / verifier les unites
      # .soilParams$Lv=c(6600,6600,6600)  # A changer ici m/m3 de soil / verifier les unites
      # .soilParams$b=1/sqrt(pi*.soilParams$Lv)
      # .soilParams$r=c(0.0004,0.0004,0.0004)

      # reglages qui marchent
      # .soilParams$La=c(6000,4000,4000)
      # .soilParams$Lv=c(32000,24000,11000)
      # .soilParams$r=c(0.0004,0.0004,0.0004)


      # modele de Gardnar-Wowen for soil-root conductance (uses both soil and vegetation parameters)

      .soilParams$La <- c(3000, 1700, 1700)
      #.soilParams$Lv <- c(9000, 3000, 1000)
      .soilParams$Lv <- c(7000, 3000, 3000)
      .soilParams$r <- c(0.002, 0.002, 0.002)
    }
  if (default_soil == F) #
    {
    
    filePath=general_params$soil_path
      if (file.exists(paste0("../Soil_Parameters/", filePath))) {
        io <- data.frame(read_excel(paste0("../Soil_Parameters/", filePath)))
      } else {
        stop(paste0("Could not find input soil parameter file : ", filePath))
      }


      colnames(io) <- c("Name", "Value")
      #   # setting commomn params for WB_veg (regardless of the options)
      params <- c(
        "RFC_1",
        "RFC_2",
        "RFC_3",
        "field_capacity",
        "wilting_point",
        "Ritchie_Gamma",
        "alpha_vg",
        "n_vg",
        "I_vg",
        "Ksat_vg",
        "saturation_capacity_vg",
        "residual_capacity_vg"
      )

      TTT <- NULL
      for (i in 1:length(params))
      {
        AAA <- which(io$Name == params[i]) ## line number of the variable

        if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
          {
            stop(paste0("'", params[i], "' is not provided in input soil parameter file, check presence or spelling\n", file))
          } else if (length(AAA) > 1) {
          stop(paste0("'", params[i], "' is provided several times in input soil parameter file, correct \n", file))
        } else if (length(AAA) == 1) {
          if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
            #print(params[i])

            eval(parse(text = paste0("TTT$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
          } else {
            stop(paste0(params[i], "must be numeric"))
          }
        }
      } # end loop on params




      .soilParams$rock_fragment_content <- c(TTT$RFC_1, TTT$RFC_2, TTT$RFC_3)

      #--------------
      # A calculer pour diagnostique
      .soilParams$field_capacity <- rep(TTT$field_capacity, 3) # Fraction of water at field capacity (cm3/cm3)
      .soilParams$wilting_point <- rep(TTT$wilting_point, 3) # Fraction of water at wilting point (cm3/cm3)

      # Ricthie parameters
      .soilParams$gamma <- TTT$Ritchie_Gamma # Ritchie parameter

      # Van Genuchten parameters
      .soilParams$alpha_vg <- rep(TTT$alpha_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
      .soilParams$n_vg <- rep(TTT$n_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$I_vg <- rep(TTT$I_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$Ksat_vg <- rep(TTT$Ksat_vg, 3) # Soil conductivity at saturation (mol/m/s/Mpa)
      .soilParams$saturation_capacity_vg <- rep(TTT$saturation_capacity_vg, 3) # Fraction of water at saturation capacity (cm3/cm3)
      .soilParams$residual_capacity_vg <- rep(TTT$residual_capacity_vg, 3) # Fraction of residual water  (cm3/cm3)
    } # end  loop default soil = F




  .soilParams$m <- (1 - 1 / .soilParams$n_vg)

  # other parameters for GARDNARCOWEN
  warning("Developer note (JR, 05/01/2021); Lv,La and r of the Gardner-Cowen soil-root model are hard coded in 'create.soil.params'")

  # anciens parametres 
  # .soilParams$La <- c(6000, 4000, 4000)
  # .soilParams$Lv <- c(32000, 24000, 11000)
  # .soilParams$r <- c(0.0004, 0.0004, 0.0004)

  
   .soilParams$La <- c(1591, 255, 64)
   .soilParams$Lv <- c(4265, 682, 171)
   .soilParams$r <- c(0.0007, 0.0007, 0.0007)
  
  .soilParams$b <- 1 / sqrt(pi * .soilParams$Lv)
  .soilParams$B_GC <- .soilParams$La * 2 * 3.14 / (log(.soilParams$b / .soilParams$r)) # B du modele de Gardnar-Cowen
  .soilParams$Ks <- .soilParams$Ksat_vg * .soilParams$B_GC


  #
  .soilParams$V_field_capacity <- convert.FtoV(.soilParams$field_capacity, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
  .soilParams$V_saturation_capacity_vg <- convert.FtoV(.soilParams$saturation_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
  .soilParams$V_saturation_capacity_camp <- convert.FtoV(.soilParams$saturation_capacity_camp, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
  .soilParams$V_residual_capacity_vg <- convert.FtoV(.soilParams$residual_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
  .soilParams$V_wilting_point <- convert.FtoV(.soilParams$wilting_point, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
  .soilParams$V_saturation_capacity <- .soilParams$V_saturation_capacity_vg

  # For diagnositc (RU)
  .soilParams$V_soil_storage_capacity <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_wilting_point)
  .soilParams$V_soil_storage_capacity_vg <- sum(.soilParams$V_saturation_capacity_vg) - sum(.soilParams$V_residual_capacity_vg)

  
  
  print(paste0("RU (vg)= ", .soilParams$V_soil_storage_capacity_vg))
 


  return(.soilParams)
} # end of the function



# Soil Evaporation from ETP /  # Calculte soil evaporation and update Soil water content in each soil layer
computeEvaporation.WBsoil <- function(WBsoil, ETP, K, LAI,Nhours) {
  # date    : 30/03/2020
  # updated : 05/01/2020 by JR (gamma dependence on Nhours) 
 
  gamma  = WBsoil$params$gamma * Nhours/24
  
  Wdef <- max((WBsoil$params$V_field_capacity[1] - WBsoil$SoilWaterStock[1]), 0) # water deficit first layer
  tfict <- (Wdef / gamma)^2
  enumax <- gamma*((sqrt(tfict + 1)) - (sqrt(tfict)))
  etpnu <- ETP * exp(-K * LAI)
  evsonu <- min(enumax, etpnu)
  evsonu <- min(evsonu, WBsoil$SoilWaterStock[1])
  evsonuPL <- evsonu * WBsoil$params$Fact_Rich


  # Update soil parameters
  for (nl in 1:3)
  {
    WBsoil$SoilWaterStock[nl] <- WBsoil$SoilWaterStock[nl] - (evsonuPL[nl])
  }

  WBsoil$Evaporation <- evsonuPL
  WBsoil$EvaporationSum <- sum(WBsoil$Evaporation)


  WBsoil <- computeSoilConductanceAndPsi.WBsoil(WBsoil) # update Conductance and Psi

  return(WBsoil)
}


computeEvaporation_withG.WBsoil <- function(WBsoil, RHair, Tsoil=20, Nhours, gSoil0 = 30, LAI ,ETP , K) {
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
  
  
  
  VPDsoil=computeVPDfromRHandT(RHair,Tsoil)
  
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
  WBsoil$SoilWaterStock[1] <- WBsoil$SoilWaterStock[1] - WBsoil$Evaporation
  WBsoil <- computeSoilConductanceAndPsi.WBsoil(WBsoil)

return(WBsoil)  
}
  
  
  




# Soil infiltration
computeInfiltration.WBsoil <- function(WBsoil, pptSoil, cstinfil = 0.7) {

  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # initialising variables
  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  LALA <- WBsoil # SoilWaterStock/potential/Drainage

  # Output

  DRAIN <- NULL

  ####  Temporary local variables
  SWStemp <- numeric(3) # SWS apres ecoulement in each layer
  SWST1 <- numeric(3)
  transfert <- numeric(3) # Transfert between layers
  Decoul <- numeric(1) # drainage du à l'écoulement
  Dsurc <- numeric(1) # drainage du à l'infiltration immediate

  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Updata SoilWaterStocks in the three soil layer  (before precipitation)
  #--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  # ecoulement entre la première et la deuxième couche
  if (LALA$SoilWaterStock[1] > LALA$params$V_field_capacity[1]) {
    transfert[1] <- cstinfil * (LALA$SoilWaterStock[1] - LALA$params$V_field_capacity[1])
  } else {
    transfert[1] <- 0
  }

  SWStemp[1] <- LALA$SoilWaterStock[1] - transfert[1]
  LALA$SoilWaterStock[2] <- LALA$SoilWaterStock[2] + transfert[1]


  # ecoulement entre la deuxième et la troisième couche
  if (LALA$SoilWaterStock[2] > LALA$params$V_field_capacity[2]) {
    transfert[2] <- cstinfil * (LALA$SoilWaterStock[2] - LALA$params$V_field_capacity[2])
  } else {
    transfert[2] <- 0
  }

  SWStemp[2] <- LALA$SoilWaterStock[2] - transfert[2]
  LALA$SoilWaterStock[3] <- LALA$SoilWaterStock[3] + transfert[2]

  # drainage profond (Decoul)
  if (LALA$SoilWaterStock[3] > LALA$params$V_field_capacity[3]) {
    transfert[3] <- cstinfil * (LALA$SoilWaterStock[3] - LALA$params$V_field_capacity[3])
  } else {
    transfert[3] <- 0
  }

  SWStemp[3] <- LALA$SoilWaterStock[3] - transfert[3]
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
  LALA$SoilWaterStock <- SWST1

  LALA <- computeSoilConductanceAndPsi.WBsoil(LALA)

  return(LALA)
}

# Compute conductance and Psi
computeSoilConductanceAndPsi.WBsoil <- function(WBsoil) {

  # Compute soil hydraulic conductivity with Van Genuchten
  if (WBsoil$params$method == "vg") {
    # Soil water holding capacity  (volumetric)
    totalavailwater <- (WBsoil$params$V_saturation_capacity_vg - WBsoil$params$V_residual_capacity_vg)

    # Compute the relative water content (m3 water/m3 soil) based on Water Reserve and soil volume
    actualavailwater <- (WBsoil$SoilWaterStock - WBsoil$params$V_residual_capacity_vg)

    REW <- actualavailwater / totalavailwater  #/ numeric [ncouches
    REW[REW <= 0.001] <- 0.001


    KSoil_temp <- REW^(WBsoil$params$I_vg) * (1 - (1 - REW^(1 / WBsoil$params$m))^WBsoil$params$m)^2

    #PsiSoil <- (-1 * ((((1 / REW)^(1 / WBsoil$params$m)) - 1)^(1 / WBsoil$params$n)) / WBsoil$params$alpha_vg / 10000)-0.3
    
    PsiSoil <- (-1 * ((((1 / REW)^(1 / WBsoil$params$m)) - 1)^(1 / WBsoil$params$n)) / WBsoil$params$alpha_vg / 10000)
    # diviser par 10000 pour passer de cm à MPa
  }
  # Compute soil hydraulic conductivity with campbell
  if (WBsoil$params$method == "camp") {
    condFac <- 1000 * WBsoil$params$La * 2 * 3.14 / log(WBsoil$params$b / WBsoil$params$r) # Soil Ks (mmol/m/s/MPa)
    Ks <- WBsoil$params$Ksat_camp * condFac
    kSoil <- (WBsoil$SoilWaterStock / WBsoil$params$V_saturation_capacity_camp)^(WBsoil$params$b_camp * 2 + 2)
    PsiSoil <- -1 * (WBsoil$params$psie * ((WBsoil$SoilWaterStock / WBsoil$params$V_saturation_capacity_camp)^-WBsoil$params$b_camp))
    REW <- NA
  }

  # Compute Soil conductance
  WBsoil$kSoil <- 1000 * WBsoil$params$Ksat_vg * WBsoil$params$B_GC * KSoil_temp
  WBsoil$PsiSoil <- PsiSoil
  WBsoil$REW <- REW

  return(WBsoil)
}

# Update soil water reservoirs, potenetil and psi according to fluxes
UpdateSoilWater.WBsoil <- function(WBsoil, fluxEvap, fluxRelease) {
  WBsoil$SoilWaterStock <- WBsoil$SoilWaterStock - fluxEvap
  WBsoil$SoilWaterStock[1] <- WBsoil$SoilWaterStock[1] + fluxRelease # release occur in 1st soil layer only
  WBsoil <- computeSoilConductanceAndPsi.WBsoil(WBsoil)

  WBsoil$REWt =(sum(WBsoil$SoilWaterStock-WBsoil$params$V_residual_capacity_vg)) / (sum(WBsoil$params$V_saturation_capacity_vg -WBsoil$params$V_residual_capacity_vg))
  
  
  return(WBsoil)
}


# utilitaires
convert.FtoV <- function(x, .rock_fragment_content, .layer_thickness) {
  # . rockfragmentcontent (%)
  # .layer_thickness (m)
  # x :  (cm3.cm3.).
  # y : (mm)

  y <- x * (1 - (.rock_fragment_content / 100)) * .layer_thickness * 1000

  return(y)
}
