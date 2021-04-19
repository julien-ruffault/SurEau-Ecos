

create.soil.parameters<- function(filePath, depths = c(0.3, 1, 4), default_soil = F, method = "vg"){
  
   # note : warning("if run on puechabon : add an Offset  on psisoil (-0.3) to match observations --> / modify  in function 'computeSoilConductanceAndPsi.WBsoil'  ") 
    
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
      if (file.exists(filePath)) {
        io <- data.frame(read.csv(filePath,header=T,sep=';',dec=','))
      } else {
        stop(paste0("Could not find input soil parameter file : ", filePath))
      }
      
      
      colnames(io) <- c("Name", "Value")
      #   # setting common parameters for WB_soil (regardless of the options)
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
          stop(paste0("'", params[i], "' is not provided in input soil parameter file, check presence or spelling\n", filePath))
        } else if (length(AAA) > 1) {
          stop(paste0("'", params[i], "' is provided several times in input soil parameter file, correct \n", filePath))
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
  

  