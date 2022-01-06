

#' create a list with soil parameters to run SureauR
#'
#' @param filePath path to a csv file containing parameter values
#' @param listOfParameters a list containing the necessary input parameterr instead of reading them  in file. Will only be used if 'filePath' arguement is not provided
#' @param depths maximum depth (in m) of the soil layers (default : 0.3, 1 and 4 meters) 
#' @param default_soil a logical value indicating whether a default soil should be used  to run tests (default =F) 
#' @return
#' @export
#'
#' @examples
create.soil.parameters <- function(filePath, listOfParameters, default_soil = F, offSetPsoil, modeling_options) {
  
   # note : warning("if run on puechabon : add an Offset  on psisoil (-0.3) to match observations --> / modify  in function 'computeSoilConductanceAndPsi.WBsoil'  ") 
    
    if (default_soil == T) # default if no file is provided
    {
      
      warning("Default soil = Puechabon with Van-Genuchten Pedo transfer")
      
      .soilParams$PedoTransferFormulation <- "VG"
      
      # Offset Psoil
      if(missing(offSetPsoil)) {
        .soilParams$offSetPsoil <- 0
      } 
      if(!missing(offSetPsoil)) {
        .soilParams$offSetPsoil <- offSetPsoil
        print(paste("There is an offset on PSOIL of",.soilParams$offSetPsoil, "MPa"))
      }
      
      #.soilParams$rock_fragment_content <- c(40, 80, 93) # coarse elements (stones/rocks) in each layer (%)
      #derniers reglages sol sur Puechabon
      .soilParams$rock_fragment_content <- c(40, 75, 90)
      
      .soilParams$depth = c(0.3, 1, 4)
      
      .soilParams$layer_thickness <- numeric(3)
      .soilParams$layer_thickness[1] <- .soilParams$depth[1]
      .soilParams$layer_thickness[2] <- .soilParams$depth[2] - .soilParams$depth[1]
      .soilParams$layer_thickness[3] <- .soilParams$depth[3] - .soilParams$depth[2]
      
      #--------------
        
      # Ricthie parameters
      #.soilParams$gamma <- 1 # Ritchie parameter
      
      .soilParams$gSoil0 = 30
      
      # Van Genuchten parameters
      .soilParams$alpha_vg <- rep(0.0035, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
      .soilParams$n_vg <- rep(1.55, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$m <- (1 - 1 / .soilParams$n_vg) #m parameters Van Genuchten equations
      .soilParams$I_vg <- rep(0.5, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$Ksat_vg <- rep(1.69, 3) # Soil conductivity at saturation (mol/m/s/Mpa)
      .soilParams$saturation_capacity_vg <- c(0.5, 0.5, 0.5) # Fraction of water at saturation capacity (cm3/cm3)
      .soilParams$residual_capacity_vg <- c(0.1, 0.1, 0.1) # Fraction of residual water  (cm3/cm3)
      
      # # A calculer pour diagnostique
      # .soilParams$field_capacity <- c(0.4, 0.4, 0.4) # Fraction of water at field capacity (cm3/cm3)
      # .soilParams$wilting_point <- c(0.2, 0.2, 0.2) # Fraction of water at wilting point (cm3/cm3)
      
      #NM 11/12/2021 add computation of wilting and field capacity from functions implemented in soil.utils.r
      .soilParams$wilting_point <- compute.thetaAtGivenPSoil (PsiTarget=1.5,  thetaRes=.soilParams$residual_capacity_vg , thetaSat=.soilParams$saturation_capacity_vg, alpha_vg=.soilParams$alpha_vg, n_vg=.soilParams$n_vg)
      .soilParams$field_capacity <- compute.thetaAtGivenPSoil (PsiTarget=0.033,  thetaRes=.soilParams$residual_capacity_vg , thetaSat=.soilParams$saturation_capacity_vg, alpha_vg=.soilParams$alpha_vg, n_vg=.soilParams$n_vg) 
       
      .soilParams$offSetPsoil <- 0
      
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
            
      .soilParams <- list()
      # Selection of the PedoTransfer function from modelling option or VanGenuchten (by default)
      if(missing(modeling_options)){
      .soilParams$PedoTransferFormulation <- "VG"
      warning("modelling option is missing. Van Genuchten used as default")
      }
      
      if(!missing(modeling_options)){
        .soilParams$PedoTransferFormulation <- modeling_options$PedoTransferFormulation
        print(paste("You are using", modeling_options$PedoTransferFormulation, "pedotransfer formulation"))
      }
      
      # Offset Psoil
      if(missing(offSetPsoil)) {
        .soilParams$offSetPsoil <- 0
        } 
      if(!missing(offSetPsoil)) {
        .soilParams$offSetPsoil <- offSetPsoil
        print(paste("There is an offset on PSOIL of",.soilParams$offSetPsoil, "MPa"))
        }
      
      if (!missing(filePath))
      {TTT = read.soil.file(filePath, PedoTransferFormulation=.soilParams$PedoTransferFormulation)}
      
      if(missing(filePath) &  !missing(listOfParameters))
      {TTT=listOfParameters}
      
      if(!missing(listOfParameters) & !missing(filePath))
      {TTT=listOfParameters
      warning("list of parameters are given by user")
      }
      
      if(missing(filePath) &  missing(listOfParameters))
      {error("'filePath' and 'ListOfParameters' are both missing, please provide one of these two arguments")}
      
      
      .soilParams$depth =c(TTT$depth1, TTT$depth2, TTT$depth3)
      .soilParams$layer_thickness <- numeric(3)
      .soilParams$layer_thickness[1] <- .soilParams$depth[1]
      .soilParams$layer_thickness[2] <- .soilParams$depth[2] - .soilParams$depth[1]
      .soilParams$layer_thickness[3] <- .soilParams$depth[3] - .soilParams$depth[2]
      .soilParams$gSoil0 <- TTT$gSoil0
      .soilParams$rock_fragment_content <- c(TTT$RFC_1, TTT$RFC_2, TTT$RFC_3)
      
      
      #--------------
      # # A calculer pour diagnostique
      # .soilParams$field_capacity <- rep(TTT$field_capacity, 3) # Fraction of water at field capacity (cm3/cm3)
      # .soilParams$wilting_point <- rep(TTT$wilting_point, 3) # Fraction of water at wilting point (cm3/cm3)
      # 
      
      if(.soilParams$PedoTransferFormulation == "VG")
        {
       #Van Genuchten parameters
      .soilParams$alpha_vg <- rep(TTT$alpha_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
      .soilParams$n_vg <- rep(TTT$n_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$I_vg <- rep(TTT$I_vg, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
      .soilParams$Ksat_vg <- rep(TTT$Ksat_vg, 3) # Soil conductivity at saturation (mol/m/s/Mpa)
      .soilParams$m <- (1 - 1 / .soilParams$n_vg)
      
      .soilParams$saturation_capacity_vg <- rep(TTT$saturation_capacity_vg, 3) # Fraction of water at saturation capacity (cm3/cm3)
      .soilParams$residual_capacity_vg <- rep(TTT$residual_capacity_vg, 3) # Fraction of residual water  (cm3/cm3)
      
      #NM 11/12/2021 add computation of wilting and field capacity from functions implemented in soil.utils.r
      .soilParams$wilting_point <- compute.thetaAtGivenPSoil (PsiTarget=1.5,  thetaRes=.soilParams$residual_capacity_vg , thetaSat=.soilParams$saturation_capacity_vg, alpha_vg=.soilParams$alpha_vg, n_vg=.soilParams$n_vg)
      .soilParams$field_capacity <- compute.thetaAtGivenPSoil (PsiTarget=0.033,  thetaRes=.soilParams$residual_capacity_vg , thetaSat=.soilParams$saturation_capacity_vg, alpha_vg=.soilParams$alpha_vg, n_vg=.soilParams$n_vg) 
      
      #--
      .soilParams$V_field_capacity <- convert.FtoV(.soilParams$field_capacity, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
      .soilParams$V_saturation_capacity_vg <- convert.FtoV(.soilParams$saturation_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
      #.soilParams$V_saturation_capacity_camp <- convert.FtoV(.soilParams$saturation_capacity_camp, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
      #warning("Developer note (NM, 05/01/2021); Saturation capacity camp is not provided'")
      .soilParams$V_residual_capacity_vg <- convert.FtoV(.soilParams$residual_capacity_vg, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
      .soilParams$V_wilting_point <- convert.FtoV(.soilParams$wilting_point, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
      .soilParams$V_saturation_capacity <- .soilParams$V_saturation_capacity_vg
      
      # For diagnositc (RU)
      .soilParams$V_soil_storage_capacity_wilt <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_wilting_point)
      .soilParams$V_soil_storage_capacity_res <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_residual_capacity_vg)
      .soilParams$V_soil_storage_capacity <- .soilParams$V_soil_storage_capacity_wilt
      
      
      print(paste0("Available water capacity Wilting: ", .soilParams$V_soil_storage_capacity_wilt, ' mm'))
      print(paste0("Available water capacity Residual: ", .soilParams$V_soil_storage_capacity_res, ' mm'))
      }
  
      if(.soilParams$PedoTransferFormulation == "Campbell") 
        {
        #NM 03/01/2022 option for campbell pedotransfer fucntions
  
        .soilParams$b_camp <- rep(TTT$b_camp, 3) # Shape parameters of the relationship betwen soil water content and soil water potential [-]
        .soilParams$psie <- rep(TTT$psie, 3) # Shape parameters of the relationship betwen soil water content and soil water potential  [-]
        .soilParams$Ksat_campbell <- TTT$Ksat_campbell
        .soilParams$saturation_capacity_campbell <- rep(TTT$saturation_capacity_campbell, 3) # Fraction of water at saturation capacity (cm3/cm3)
        .soilParams$wilting_point <- compute.thetaAtGivenPSoil.Camp (PsiTarget = -1.5, thetaSat=.soilParams$saturation_capacity_campbell, psie=.soilParams$psie, b_camp=.soilParams$b_camp)
        #NM 03/01/2022 TODO : check if there is a mistake here : where is the field capacity @-.33 or @ -.033 MPa ?
        .soilParams$field_capacity <- compute.thetaAtGivenPSoil.Camp (PsiTarget = -0.033, thetaSat=.soilParams$saturation_capacity_campbell, psie=.soilParams$psie, b_camp=.soilParams$b_camp) 
        .soilParams$residual_capacity_camp <- compute.thetaAtGivenPSoil.Camp (PsiTarget = -100, thetaSat=.soilParams$saturation_capacity_campbell, psie=.soilParams$psie, b_camp=.soilParams$b_camp) # Fraction of residual water  (cm3/cm3)
      
        #Water volumes
      
        .soilParams$V_field_capacity <- convert.FtoV(.soilParams$field_capacity, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
        .soilParams$V_saturation_capacity_campbell <- convert.FtoV(.soilParams$saturation_capacity_campbell, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
       
        .soilParams$V_residual_capacity_campbell <- convert.FtoV(.soilParams$residual_capacity_camp, .soilParams$rock_fragment_content, .soilParams$layer_thickness) #There is no residual capacity for campbell model : 0.1 by default
        .soilParams$V_wilting_point <- convert.FtoV(.soilParams$wilting_point, .soilParams$rock_fragment_content, .soilParams$layer_thickness)
        .soilParams$V_saturation_capacity <- .soilParams$V_saturation_capacity_campbell
        
        # For diagnostic (TAW)
        .soilParams$V_soil_storage_capacity_wilt_campbell <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_wilting_point)
        .soilParams$V_soil_storage_capacity_res_campbell <- sum(.soilParams$V_field_capacity) - sum(.soilParams$V_residual_capacity_campbell)
        
        .soilParams$V_soil_storage_capacity <- .soilParams$V_soil_storage_capacity_wilt_campbell
        
        print(paste0("Available water capacity Wilting: ", .soilParams$V_soil_storage_capacity_wilt_campbell, ' mm'))
        print(paste0("Available water capacity Residual: ", .soilParams$V_soil_storage_capacity_res_campbell, ' mm'))
      
      }
      
  }

   return(.soilParams)
  } # end of the function



  
read.soil.file <- function(filePath, PedoTransferFormulation=.soilParams$PedoTransferFormulation)
  { 
  if (file.exists(filePath)) {
    io <- data.frame(read.csv(filePath,header=T,sep=';',dec='.'))
  } else {
    stop(paste0("Could not find input soil parameter file : ", filePath))
  }
  
  colnames(io) <- c("Name", "Value")
  ## Setting common parameters for WB_soil (regardless of the options)
  if(PedoTransferFormulation=="VG") {
  params <- c(
    "RFC_1",
    "RFC_2",
    "RFC_3",
    "depth1",
    "depth2",
    "depth3",
    "field_capacity",
    "wilting_point",
    "alpha_vg",
    "n_vg",
    "I_vg",
    "Ksat_vg",
    "saturation_capacity_vg",
    "residual_capacity_vg",
    "gSoil0",
    "offSetPsoil")
  }

if(PedoTransferFormulation=="Campbell") {  
  params <- c(
    "RFC_1",
    "RFC_2",
    "RFC_3",
    "depth1",
    "depth2",
    "depth3",
    "field_capacity",
    "wilting_point",
    "Ksat_campbell",
    "saturation_capacity_campbell",
    "b_camp",
    "psie",
    "gSoil0",
    "offSetPsoil"
  )
}
  
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
  
  return(TTT)
  
}
