## ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# create list with all species parameters from configuration file
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

#' create a list wit the vegetation parameters to run \code{run.SurEauR}
#'
#' @param filePath path to a csv file containing parameter values 
#' @param listOfParameters a list containing the necessary input parameters instead of reading them in file. Will only be used if 'filePath' arguement is not provided
#' @param stand_parameters  a list containing stand parameters created with \code{create.stand.parameters} 
#' @param modeling_options  a list containing modeling options created with \code{create.modeling.options} 
#'
#' @return
#' a list with modeling options to run  SurEau-Ecos
#' @export
#' @examples
#'
create.vegetation.parameters <- function(filePath,listOfParameters,stand_parameters, soil_parameters, modeling_options) {

 
  
  if (!missing(filePath))
  {TTT = read.vegetation.file(filePath,modeling_options=modeling_options)}

  if(missing(filePath) &  !missing(listOfParameters))
  {TTT=listOfParameters}

  if(missing(filePath) &  missing(listOfParameters))
  {error("'filePath' and 'ListOfParameters' are both missing, please provide one of these two arguments")}


  TTT$LAImax =stand_parameters$LAImax
    
  
  # calculate root distribution within each soil layer (Jackson et al. 1996)
  TTT$rootDistribution <-numeric(3)
  TTT$rootDistribution[1] = 1-TTT$betaRootProfile^(soil_parameters$depth[1]*100) # conversion of depth to cm 
  TTT$rootDistribution[2] = (1-TTT$betaRootProfile^(soil_parameters$depth[2]*100))-TTT$rootDistribution[1]
  TTT$rootDistribution[3] = 1-(TTT$rootDistribution[1]+TTT$rootDistribution[2] )
  

  # determine root lenght (La gardner cowan)
  RAI = TTT$LAImax*TTT$fRootToLeaf
  
  TTT$La = RAI*TTT$rootDistribution / (2*pi*TTT$rootRadius)
  TTT$Lv = TTT$La/(soil_parameters$layer_thickness*(1-soil_parameters$rock_fragment_content/100))
  
  ##### calculate the different conductance of the plant from kPlantInit 
  conduc <- distribute.conductances(kPlantInit=TTT$kPlantInit, ri = TTT$rootDistribution) 
  TTT$k_TLInit <- conduc$k_TLInit
  TTT$k_RTInit <- conduc$k_RTInit 
  TTT$k_LSymInit <- conduc$k_LSymInit
  
  TTT$VolumeLiving_TRB = TTT$VolumeLiving_TRB/TTT$LAImax # convert from L/m2soil to L/m2leaf
  
  
  return(TTT)
}

read.vegetation.file <- function(filePath, modeling_options){
  
  if (file.exists(filePath)) {
    io <- read.csv(file = filePath, sep = ";", dec='.',head = T)
  } else {
    stop(paste0("Could not find input parameter file : ", filePath))
  }
  
  colnames(io) <- c("Name", "Value")
  
  TTT <- list() # initialization 
  
  # setting commomn params for WB_veg (regardless of the options)
  params <- c(
    "P50_VC_Leaf", # [MPa] / Water potential causing 50% Cavitation in the vulnerability curve
    "slope_VC_Leaf", # [%/MPa]             / Slope of the vulnerability curve
    "P50_VC_Trunk",
    "slope_VC_Trunk",
    "EpsilonSymp_Leaf", # [MPa]            / Modulus of elasticity in leaves
    "PiFullTurgor_Leaf", # [MPa]          / Osmotic Potential at full turgor in leaves
    "ApoplasmicFrac_Leaf", # [-]           / Apoplasmic Fraction in leaves
    "LDMC", # [mgMS/g]                / Leaf dry matter content (measured for fully watered leaves)
    "LMA", # [g/m2leaf]                   / Leaf mass per area
    "K", # [-]                        / Light extinction coefficient of the vegetation layer
    "kPlantInit", # [mmol/MPa/s/m2leaf]  / Hydaulic conductance of the plant from soil to leaves
    "gmin20", # [mmol/m2leaf/s]         / Minimum conductance (gmin) at the reference temperature
    "TPhase_gmin", # [degC]            / Temperature for phase transition of minimum conductance
    "Q10_1_gmin", # [-]                 / Q10 value for gmin = f(T) <= Tphase_gmin
    "Q10_2_gmin", # [-]                 / Q10 value for gmin = f(T)  > Tphase_gmin
    "gmin_T",  #  conductance (gmin) of the trunk
    "CanopyStorageParam", # [l/m2leaf]    / Depth of water that can be retained by leaves and trunks per unit of leaf area index (used to compute the canopy water storage capacity as a function of LAI)
    "k_TSymInit",
    "gCrown0", 
    "gsMax",      # parameter in Jarvis model (currently default to 200) 
    "gsNight",    # parameter in jarvis gs model (currently default to 20 
    "JarvisPAR",  # parameter in Jarvis gs mdoel (currently default is 0.006) 
    "Tgs_sens",   # temperature parameter in Jarvis model (currently default Value is 17)
    "Tgs_optim",  # temperature parameter in Jarvis model (currently default value is 25)
    "fRootToLeaf", # root to leaf ratio 
    "rootRadius",  #  radius of roots (m)
    "betaRootProfile", # parameter for the distribution of roots in the soil 
    "PiFullTurgor_Trunk",
    "EpsilonSymp_Trunk",
    "ApoplasmicFrac_Trunk",
    "SymplasmicFrac_Trunk",
    "VolumeLiving_TRB",
    "fTRBToLeaf",
    "C_LApoInit",
    "C_TApoInit"
  )
  
  for (i in 1:length(params)) {
    AAA <- which(io$Name == params[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file/otherwise stop running
    {
      stop(paste0("'", params[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", filePath))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params[i], "' is not provided several times in input vegetation parameter file, correct \n", filePath))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0("TTT$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params[i], "must be numeric"))
      }
    }
  }
  
  
  
  # Gestion des parameters de vegetaion pour la regulation stomatique selon  les options 
  if (modeling_options$stomatalRegFormulation=='PiecewiseLinear')
  {
    params_regulation <- c("PsiStartClosing", "PsiClose")
  } 
  if(modeling_options$stomatalRegFormulation=='Sigmoid') {
    params_regulation <- c("P12_gs", "P88_gs")
  }
  
  if(modeling_options$stomatalRegFormulation=='Turgor') {
    params_regulation <- c("turgorPressureAtGsMax")
  }
  
  for (i in 1:length(params_regulation)) {
    AAA <- which(io$Name == params_regulation[i]) ## line number of the variable
    
    if (length(AAA) == 0) # checking that it exists n input file/otherwise stop running
    {
      stop(paste0("'", params_regulation[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", filePath))
    } else if (length(AAA) > 1) {
      stop(paste0("'", params_regulation[i], "' is provided several times in input vegetation parameter file, correct \n", filePath))
    } else if (length(AAA) == 1) {
      if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
        eval(parse(text = paste0("TTT$", params_regulation[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
      } else {
        stop(paste0(params_regulation[i], "must be numeric"))
      }
    }
  }
  
  if (modeling_options$stomatalRegFormulation=='Sigmoid')
  {TTT$P50_gs  = (TTT$P12_gs + TTT$P88_gs)/2
  TTT$slope_gs = 100/(TTT$P12_gs-TTT$P88_gs)
  }
  
  
  
  ##### Foliage Type  ####
  AAA <- io[which(io$Name == "Foliage"), "Value"]
  if (length(AAA) == 0) # checking that it exists in input file  /otherwise stop running
  {
    stop(paste0("'Foliage' is not provided in input vegetation parameter file, check presence or spelling"))
  } else if (length(AAA) > 1) {
    stop(paste0("'", params[i], "'Foliage' is provided several times in input vegetation parameter file, correct"))
  } else if (length(AAA) == 1) { # checking that parameter is numeric in input file /stop running otherwise
    TTT$Foliage <- as.character(AAA)
  }
  
  if (!TTT$Foliage %in% c("Evergreen", "Deciduous")) {
    stop("'Foliage' should be 'Evergreen' or 'Deciduous' / Check Type of spelling ")
  }
  
  if (TTT$Foliage == "Deciduous") {
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
          eval(parse(text = paste0("TTT$", params_Decid[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_Decid[i], "must be numeric"))
        }
      }
    }
  }
  
  
  
  # ETP parameters for PT or PM
  if (modeling_options$ETPFormulation == "PT") {
    params_PT <- c("PTcoeff")
    for (i in 1:length(params_PT))
    {
      AAA <- which(io$Name == params_PT[i]) ## line number of the variable
      
      if (length(AAA) == 0) # checking that it exists n input file  /otherwise stop running
      {
        stop(paste0("'", params_PT[i], "' is not provided in input vegetation file, check presence and/or spelling"))
      } else if (length(AAA) > 1) {
        stop(paste0("'", params_PT[i], "' is provided several times in input vegetation file"))
      } else if (length(AAA) == 1) {
        if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
          eval(parse(text = paste0("TTT$", params_PT[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
        } else {
          stop(paste0(params_PT[i], "must be numeric, check in input vegetation file "))
        }
      }
    }
  }
  
  return(TTT)
  
  }
