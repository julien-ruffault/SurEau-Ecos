# Creation WB_ouput
new.WBoutput <- function(simulation_parameters) {

  if (!simulation_parameters$resolutionOutput == "none"){
  
  # Read output files / (can be built by the user)
  if (file.exists(paste0(simulation_parameters$mainDir,"/functions/output_types/", simulation_parameters$outputType, ".csv"))) {
    outputvar <- read.csv(paste0(simulation_parameters$mainDir,"/functions/output_types/", simulation_parameters$outputType, ".csv"), dec = ".", sep = ";", header = F, stringsAsFactors = F)
  } else {
    stop(paste0("ouput type: ", simulation_parameters$outputType, "' does not exist, check presence or spelling"))
  }

  # Read references type (saved in output_ref; note : should be modified by developers only)
  if (file.exists(paste0(simulation_parameters$mainDir,"/functions/output_ref.csv"))) {
    outputref <- read.table(paste0(simulation_parameters$mainDir,"/functions/output_ref.csv"), dec = ".", sep = ";", header = T, stringsAsFactors = F)
  } else {
    stop(paste0("Developper issue : the reference file : ", paste0(simulation_parameters$mainDir,'/functions/output_ref.csv'), "' does not exist"))
  }

  # Check that the names in output files exist in outputref / otherwise stop the code and print an error message to avoid further problems
  for  (i in 1:length(outputvar))
  {
    if (!nrow(outputref[outputref[, 1] %in% outputvar[i], ]) == 1) {
      
      if (nrow(outputref[outputref[, 1] %in% outputvar[i], ])>1)
      { 
        stop(paste0("Developper : Error while creating output of type : '",simulation_parameters$outputType,"'. \n --> variable '", outputvar[i], "'has several entries in outputref.csv"))  
      }
      
      if (nrow(outputref[outputref[, 1] %in% outputvar[i], ])==0)
      {
        stop(paste0("Error while creating output of type : '",simulation_parameters$outputType,"'. \n --> variable '", outputvar[i], "' does not exist, check variable name"))  
      }
    }
  }

  contingencyTable <- outputref[outputref[, 1] %in% outputvar, ]

 
  filename <-  simulation_parameters$outputPath


  file.create(filename) # create file
  testcon <- file(description = filename, open = "a+b") # open connection (Kept opened while model is running)
  cat(c("Time", contingencyTable[, 1]), "\n", file = testcon) # add variable names at the top of the output file

  return(list(filename = filename, contingencyTable = contingencyTable, testcon = testcon))
  }
}





# writing outputs in output file
write.WBoutput <- function(WBoutput,Date,WBveg,WBsoil,WBclim){
  
  x.m <- paste(floor(WBclim$TIME), round((WBclim$TIME-floor(WBclim$TIME))*60), sep=":")
  TIME <- as.POSIXct(x=paste0(Date, "/", x.m), format = "%Y-%m-%d/%H:%M", tz = "UTC")
  #browser()
  #df = as.numeric(TIME)
  df= as.character(TIME,format='%Y-%m-%d/%H:%M:%S')
  df2= NULL
  for(i in (1:length(WBoutput$contingencyTable[,1])))
  {
    # The two following lines can be  used or debugging output
    #print(WBoutput$contingencyTable[i,]) #// for debugging only
    #browser()
    df2[i] <- unlist(get(WBoutput$contingencyTable[[i,2]])[WBoutput$contingencyTable[[i,3]]])[[as.numeric(WBoutput$contingencyTable[[i,4]])]]
  }
  cat(c(df,df2),"\n", file=WBoutput$testcon)
#  browser()
}

write.WBoutput.daily <- function(WBoutput,Date,WBdaily){
  TIME <- Date
  df = as.character(TIME,format='%Y-%m-%d%H:%M:%S')
  df2= NULL
  for(i in (1:length(WBoutput$contingencyTable[,1])))
  {
    #print(WBoutput$contingencyTable[i,]) #// for debugging only
    #browser()
    df2[i] <- unlist(get(WBoutput$contingencyTable[[i,2]])[WBoutput$contingencyTable[[i,3]]])[[as.numeric(WBoutput$contingencyTable[[i,4]])]]
  }
  cat(c(df,df2),"\n", file=WBoutput$testcon)  
}

write.WBoutput.yearly <- function(WBoutput,year,WByearly){
  TIME <- year
  df = as.numeric(TIME)
  df2= NULL
  for(i in (1:length(WBoutput$contingencyTable[,1])))
  {
    #print(WBoutput$contingencyTable[i,]) #// for debugging only
    #browser()
    df2[i] <- unlist(get(WBoutput$contingencyTable[[i,2]])[WBoutput$contingencyTable[[i,3]]])[[as.numeric(WBoutput$contingencyTable[[i,4]])]]
  }
  cat(c(df,df2),"\n", file=WBoutput$testcon)  
}
  
  

new.WBdaily  <- function(){
  WBdaily = list()
  
  WBdaily$transpiration_mm = 0
  WBdaily$evaporation_mm   = 0
  
  WBdaily$Psi_LSymMin = 0
  WBdaily$Psi_LSymMax = 0
  WBdaily$Psi_LApoMin = 0
  WBdaily$Psi_LApoMax = 0
  
  WBdaily$Psi_SSymMin = 0
  WBdaily$Psi_SSymMax = 0
  WBdaily$Psi_SApoMin = 0
  WBdaily$Psi_SApoMax = 0
  
  WBdaily$PLC_Leaf_max   = 0
  WBdaily$PLC_Stem_max = 0
  
  WBdaily$temperature  = NA
  WBdaily$RH = NA
  WBdaily$RG = NA
  WBdaily$PPT = NA
  WBdaily$Rn = NA
  WBdaily$PPT = NA
  WBdaily$ETP = NA
  WBdaily$VPDmean = NA
  WBdaily$VPDmax = NA
  
  
  WBdaily$SWS=numeric(3)
  
  
  return(WBdaily)
}

new.WByearly <- function(){
  WByearly = list()
  
  WByearly$transpiration_mm = 0
  WByearly$evaporation_mm   = 0
  
  WByearly$Psi_LSymMin = 0
  WByearly$Psi_LSymMax = 0
  WByearly$Psi_LApoMin = 0
  WByearly$Psi_LApoMax = 0
  
  WByearly$Psi_SSymMin = 0
  WByearly$Psi_SSymMax = 0
  WByearly$Psi_SApoMin = 0
  WByearly$Psi_SApoMax = 0
  
  WByearly$PLC_Leaf_max   = 0
  WByearly$PLC_Stem_max = 0
  
  WByearly$temperature = NA
  WByearly$RH = NA
  WByearly$RG = NA
  WByearly$PPT = NA
  WByearly$Rn = NA
  WByearly$PPT = NA
  WByearly$ETP = NA
  WByearly$VPDmean  = NA
  WByearly$VPDmax = NA
  
  
  
  WByearly$dayOfDeath = NA
  WByearly$dayOfStomatalClosure = NA
  return(WByearly)
  
}

update.WBdaily <- function(WBdaily,WBveg,WBclim,WBsoil){
  
  WBdaily$transpiration_mm = WBdaily$transpiration_mm + WBveg$transpiration_mm
  WBdaily$evaporation_mm   = WBdaily$evaporation_mm + WBsoil$EvaporationSum
  
  WBdaily$Psi_LSymMin = min(WBdaily$Psi_LSymMin,WBveg$Psi_LSym)
  WBdaily$Psi_LSymMax = max(WBdaily$Psi_LSymMax,WBveg$Psi_LSym)
  WBdaily$Psi_LApoMin = min(WBdaily$Psi_LApoMin,WBveg$Psi_LApo)
  WBdaily$Psi_LApoMax = max(WBdaily$Psi_LApoMax,WBveg$Psi_LApo)
  WBdaily$Psi_SSymMin = min(WBdaily$Psi_SSymMin,WBveg$Psi_SSym)
  WBdaily$Psi_SSymMax = max(WBdaily$Psi_SSymMax,WBveg$Psi_SSym)
  WBdaily$Psi_SApoMin = min(WBdaily$Psi_SApoMin,WBveg$Psi_SApo)
  WBdaily$Psi_SApoMax = max(WBdaily$Psi_SApoMax,WBveg$Psi_SApo)

  WBdaily$PLC_Leaf_max   = max(WBdaily$PLC_LEaf_max,WBveg$PLC_Leaf)
  WBdaily$PLC_Stem_max = max(WBdaily$PLC_Stem_max,WBveg$PLC_Stem)
  
  WBdaily$temperature  = WBclim$Tair_mean
  WBdaily$RH = WBclim$RHair_mean
  WBdaily$RG = WBclim$RG
  WBdaily$PPT = WBclim$PPT
  WBdaily$Rn = WBclim$net_radiation
  WBdaily$PPT = WBclim$PPT
  WBdaily$ETP = WBclim$ETP
  WBdaily$VPD = WBclim$VPD
  WBdaily$WS  = WBclim$WS_mean

  
  WBdaily$SWS  = WBsoil$soilWaterStock
  
  
  return(WBdaily)
}

update.WByearly <- function(WByearly,WBdaily,dayOfDeath,WBveg,DAY){

  WByearly$transpiration_mm = WByearly$transpiration_mm  + WBdaily$transpiration_mm 
  WByearly$evaporation_mm   = WByearly$evaporation_mm + WBdaily$evaporation_mm
  
  WByearly$Psi_LSymMin = min(WByearly$Psi_LSymMin, WBdaily$Psi_LSymMin)
  WByearly$Psi_LSymMax = max(WByearly$Psi_LSymMax, WBdaily$Psi_LSymMax)
  WByearly$Psi_LApoMin = min(WByearly$Psi_LApoMin, WBdaily$Psi_LApoMin)
  WByearly$Psi_LApoMax = max(WByearly$Psi_LApoMax, WBdaily$Psi_LApoMax)
  WByearly$Psi_SSymMin = min(WByearly$Psi_SSymMin, WBdaily$Psi_SSymMin)
  WByearly$Psi_SSymMax = max(WByearly$Psi_SSymMax, WBdaily$Psi_SSymMax)
  WByearly$Psi_SApoMin = min(WByearly$Psi_SApoMin, WBdaily$Psi_SApoMin)
  WByearly$Psi_SApoMax = max(WByearly$Psi_SApoMax, WBdaily$Psi_SApoMax)
  
  WByearly$PLC_Leaf_max   = max(WByearly$PLC_Leaf_max,WBdaily$PLC_Leaf_max)
  WByearly$PLC_Stem_max = max(WByearly$PLC_Stem_max,WBdaily$PLC_Stem_max)
  
  WByearly$temperature_max = max(WByearly$temperature_max,WBdaily$temperature)
  WByearly$RH_min = min(WByearly$RH_min,WBdaily$RH)
  WByearly$RG  = WByearly$RG + WBdaily$RG
  WByearly$PPT = WByearly$PPT + WBdaily$PPT
  WByearly$Rn  = WByearly$Rn + WBdaily$Rn
  WByearly$PPT = WByearly$PPT + WBdaily$PPT
  WByearly$ETP = WByearly$ETP + WBdaily$ETP
  WByearly$VPDmax = max(WByearly$VPDmax, WBdaily$VPD)
  
  if (!missing(dayOfDeath))
      {WByearly$dayOfDeath = dayOfDeath}
  
  #print(WBdaily$Psi_LApoMin)
  if(is.na(WByearly$dayOfStomatalClosure)==T & WBdaily$Psi_LApoMin<WBveg$params$P88_gs){
    WByearly$dayOfStomatalClosure=DAY
    }
  
  
  return(WByearly)
}
