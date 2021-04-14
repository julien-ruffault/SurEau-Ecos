
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#                       &
#           Julien Ruffault (julien.ruff@gmail.com)
#                       &
#           Francois Pimont (francois.pimont@inrae.fr)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##


# V2 (18/12/2020) // JR modified the following in 'write.WBouptut' and 'newWBoutput' to speed up the code  (after profiling) 
#  -  replaced 'eval(parse(text..' formulations by 'get[[...'
 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Creation WB_ouput
New_WBoutput <- function(general_params) {

  # Read output files / (can be built by the user)
  if (file.exists(paste0(general_params$mainDir,"/functions/output_types/", general_params$outputType, ".csv"))) {
    outputvar <- read.csv(paste0(general_params$mainDir,"/functions/output_types/", general_params$outputType, ".csv"), dec = ",", sep = ";", header = F, stringsAsFactors = F)
  } else {
    stop(paste0("ouput type: ", general_params$outputType, "' does not exist, check presence or spelling"))
  }

  # Read references type (saved in output_ref; note : output_ref should be modified by the developers only)
  if (file.exists("../functions/output_ref.csv")) {
    outputref <- read.table("../functions/output_ref.csv", dec = ",", sep = ";", header = T, stringsAsFactors = F)
  } else {
    stop(paste0("file : ", paste0("../", filePath), "' does not exist, check presence or spelling"))
  }

  # Check that the names in output files exist in outputref / otherwise stop the code and print an error message // to avoid further code problems
  for  (i in 1:length(outputvar))
  {
    if (!nrow(outputref[outputref[, 1] %in% outputvar[i], ]) == 1) {
      stop(paste0("Error while creating output of type : '",general_params$outputType,"'. \n --> variable '", outputvar[i], "' does not exist, check variable name"))
    }
  }


  contingencyTable <- outputref[outputref[, 1] %in% outputvar, ]

  if (general_params$addInfotoFileName == T) {
    filename <- paste0("Results_model/", general_params$fileNameInfo, "_Results_model_", outputVersion, "_", paste0(format(Sys.time(), "%Y-%m-%d_%H-%M"), ".csv"))
  } else {
    filename <- paste0(mainDir,"/Results_model/", general_params$fileNameInfo, ".csv")
  }


  file.create(filename) # create file
  testcon <- file(description = filename, open = "a+b") # open connection (Kept opened while model is running)
  cat(c("Time", contingencyTable[, 1]), "\n", file = testcon) # add variable names at the top of the output file

  return(list(filename = filename, contingencyTable = contingencyTable, testcon = testcon))
}



# writing outputs in 
Write.WBoutput <- function(WBoutput,Date,WBveg,WBsoil,WBclim){
  
  x.m <- paste(floor(WBclim$TIME), round((WBclim$TIME-floor(WBclim$TIME))*60), sep=":")
  TIME <- as.POSIXct(x=paste0(Date, "/", x.m), format = "%Y-%m-%d/%H:%M", tz = "UTC")
  
  df = as.numeric(TIME)
  df2= NULL
  for(i in (1:length(WBoutput$contingencyTable[,1])))
  {
    #print(i) #// for debugging only
    df2[i] <- unlist(get(WBoutput$contingencyTable[[i,2]])[WBoutput$contingencyTable[[i,3]]])[[as.numeric(WBoutput$contingencyTable[[i,4]])]]
  }
  cat(c(df,df2),"\n", file=WBoutput$testcon)
}
