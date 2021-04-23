# Creation WB_ouput
new.WBoutput <- function(simulation_parameters) {

  # Read output files / (can be built by the user)
  if (file.exists(paste0(simulation_parameters$mainDir,"/functions/output_types/", simulation_parameters$outputType, ".csv"))) {
    outputvar <- read.csv(paste0(simulation_parameters$mainDir,"/functions/output_types/", simulation_parameters$outputType, ".csv"), dec = ",", sep = ";", header = F, stringsAsFactors = F)
  } else {
    stop(paste0("ouput type: ", simulation_parameters$outputType, "' does not exist, check presence or spelling"))
  }

  # Read references type (saved in output_ref; note : should be modified by the developers only)
  if (file.exists(paste0(simulation_parameters$mainDir,"/functions/output_ref.csv"))) {
    outputref <- read.table(paste0(simulation_parameters$mainDir,"/functions/output_ref.csv"), dec = ",", sep = ";", header = T, stringsAsFactors = F)
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
# writing outputs in output file
write.WBoutput <- function(WBoutput,Date,WBveg,WBsoil,WBclim){
  
  x.m <- paste(floor(WBclim$TIME), round((WBclim$TIME-floor(WBclim$TIME))*60), sep=":")
  TIME <- as.POSIXct(x=paste0(Date, "/", x.m), format = "%Y-%m-%d/%H:%M", tz = "UTC")
  
  df = as.numeric(TIME)
  df2= NULL
  for(i in (1:length(WBoutput$contingencyTable[,1])))
  {
    #print(WBoutput$contingencyTable[i,]) #// for debugging only
    df2[i] <- unlist(get(WBoutput$contingencyTable[[i,2]])[WBoutput$contingencyTable[[i,3]]])[[as.numeric(WBoutput$contingencyTable[[i,4]])]]
  }
  cat(c(df,df2),"\n", file=WBoutput$testcon)
}
