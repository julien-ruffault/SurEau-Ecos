

#' create a list with soil parameters to run SureauR
#'
#' @param filePath path to a csv file containing parameter values
#' @param depths maximum depth (in m) of the soil layers (default : 0.3, 1 and 4 meters) 
#' @param default_soil a logical value indicating whether a default soil should be used  to run tests (default =F) 
#' @return
#' @export
#'
#' @examples
create.soil.parameters<- function(filePath, depths = c(0.3, 1, 4), default_soil = F) {
  
   # note : warning("if run on puechabon : add an Offset  on psisoil (-0.3) to match observations --> / modify  in function 'computeSoilConductanceAndPsi.WBsoil'  ") 
    
    .soilParams <- list()
    .soilParams$depth =depths
    .soilParams$layer_thickness <- numeric(3)
    .soilParams$layer_thickness[1] <- depths[1]
    .soilParams$layer_thickness[2] <- depths[2] - depths[1]
    .soilParams$layer_thickness[3] <- depths[3] - depths[2]
    
    
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
      #.soilParams$gamma <- 1 # Ritchie parameter
      
      .soilParams$gSoil0 = 30
      
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
    
      

      
      # modele de Gardnar-Wowen for soil-root conductance (uses both soil and vegetation parameters)
      # .soilParams$La <- c(3000, 1700, 1700)
      # .soilParams$Lv <- c(7000, 3000, 3000)
      # .soilParams$r <- c(0.002, 0.002, 0.002)
    }
    if (default_soil == F) #
    {
      if (file.exists(filePath)) {
        io <- data.frame(read.csv(filePath,header=T,sep=';',dec='.'))
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
        "alpha_vg",
        "n_vg",
        "I_vg",
        "Ksat_vg",
        "saturation_capacity_vg",
        "residual_capacity_vg",
        "gSoil0"
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
      
      
      
      
      .soilParams$gSoil0 <- TTT$gSoil0
      
      .soilParams$rock_fragment_content <- c(TTT$RFC_1, TTT$RFC_2, TTT$RFC_3)
      
      #--------------
      # A calculer pour diagnostique
      .soilParams$field_capacity <- rep(TTT$field_capacity, 3) # Fraction of water at field capacity (cm3/cm3)
      .soilParams$wilting_point <- rep(TTT$wilting_point, 3) # Fraction of water at wilting point (cm3/cm3)
    
      
      # Van Genuchten parameters
      .soilParams$alpha_vg <- rep(TTT$alpha_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
      .soilParams$n_vg <- rep(TTT$n_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$I_vg <- rep(TTT$I_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$Ksat_vg <- rep(TTT$Ksat_vg, 3) # Soil conductivity at saturation (mol/m/s/Mpa)
      .soilParams$saturation_capacity_vg <- rep(TTT$saturation_capacity_vg, 3) # Fraction of water at saturation capacity (cm3/cm3)
      .soilParams$residual_capacity_vg <- rep(TTT$residual_capacity_vg, 3) # Fraction of residual water  (cm3/cm3)
    } # end  loop default soil = F
    
    

    .soilParams$m <- (1 - 1 / .soilParams$n_vg)
    

    .soilParams$V_field_capacity <- convert.FtoV(.soilParams$field_capacity, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
    .soilParams$V_saturation_capacity_vg <- convert.FtoV(.soilParams$saturation_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
    #.soilParams$V_saturation_capacity_camp <- convert.FtoV(.soilParams$saturation_capacity_camp, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
    #warning("Developer note (NM, 05/01/2021); Saturation capacity camp is not provided'")
    .soilParams$V_residual_capacity_vg <- convert.FtoV(.soilParams$residual_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
    .soilParams$V_wilting_point <- convert.FtoV(.soilParams$wilting_point, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
    .soilParams$V_saturation_capacity <- .soilParams$V_saturation_capacity_vg
    
    # For diagnositc (RU)
    .soilParams$V_soil_storage_capacity <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_wilting_point)
    .soilParams$V_soil_storage_capacity_vg <- sum(.soilParams$V_saturation_capacity_vg) - sum(.soilParams$V_residual_capacity_vg)
    

    print(paste0("RU (vg)= ", .soilParams$V_soil_storage_capacity_vg))
  
    
    return(.soilParams)
  } # end of the function
  

  