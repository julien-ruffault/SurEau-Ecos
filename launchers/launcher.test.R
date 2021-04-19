# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### ##
# Test Launcher to run SurEau-ECOS (V4.1) on Champenoux and compute the reference output
# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>
# date   :  V1  : 11/01/2021    
#           V2  : 14/04/2021 (JR) / change output management and writing .  cleaned the code
#           V3  : 16/04/2021 (JR) / change input  parameters (major update involving creating new Script and modifying  the main program) 
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# notes (JR, 18/04/2021)
# Remove DSStore files from github 
#  - avoid overwriting existing output file by checking existence first / add an option to allow overwriting  --> Julien 
#  - faire la lecture du time a partir du climate.data et et ne pas specifier 2 fois la commande --> Julien 
#  - separate output parameters from simulation parameters  ?
#  - voir pkoi le PAR est multiplie par 10 dans la fonction compute.Transpiration  in functionsWVveg (around L405) -->Julien 
# - voir pour le parameter WBveg$TBA et gminT--W move as a parameters ? 
# -  gCrown0=45 #TODO gCrown0 hardcoded (WBveg)
# - WBveg$gs = WBveg$gs_lim #TODO check why gs and gs_lim ?
#  - rename and comment Rs.Comp PLC.Comp and PLCPrime.Comp in plant.utils
#  Voir fonctions de Jarvis dans NewJarvis -- ne sont pas utilises /. quelle version de ces fonctions utiliser ? ?? 
#  voir pour implmenter avoidSoilTransfertBetweenSoilLayers 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory


# User Parametrization ---------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))                  # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SureauR.functions.R'))                              # do not modify 

climateData_path          <- paste0(mainDir,'/datasets/test_data/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 
soilParameters_path       <- paste0(mainDir,'/datasets/test_data/Soil_test_champenoux.csv')
vegetationParameters_path <- paste0(mainDir,'/datasets/test_data/Parameters_test_quercus_champenoux_evergreen.csv')
#standParameters_path      <- paste0(mainDir,'datasets/test_data/stand_champenoux_test.csv')  
        
modeling_options      <- create.modeling.options(constantClimate=T)                      # <-- indicate  modeling options 
simulation_parameters <- create.simulation.parameters(startYearSimulation=1990,          # <-- indicate here simulation parameters
                                                             endYearSimulation=1990,
                                                             mainDir=mainDir,
                                                             outputFileName='test')

### Create input files and run SurEau-Ecos--------------------------------------
climate_data <- create.climate.data(filePath=climateData_path, modeling_options=modeling_options, simulation_parameters=simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax=6, lat = 48.73, lon=6.23)
soil_parameters <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.373333 ,0.746666,1.119)) 
vegetation_parameters <- create.vegetation.parameters(filePath =vegetationParameters_path, stand_parameters = stand_parameters, modeling_options = modeling_options)

run.SurEauR(modeling_options = modeling_options ,
        simulation_parameters = simulation_parameters, 
       climate_data =climate_data,
       stand_parameters =stand_parameters, 
       soil_parameters = soil_parameters,
       vegetation_parameters=vegetation_parameters)


# for analyses / 
  filename  = paste0(mainDir,"/Results_model/.csv")
  DATA = read.csv(filename,header=T, dec='.',sep="")
  DATA$DD= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")
  
  plot(DATA$DD,DATA$Psi_LSym,type='l',col='red')
  lines(DATA$DD,DATA$Psi_LApo,type='l',col='green')
  


#  plot(DD,DATA$SWS1)



#  filename  = paste0("../Results_model/Champenoux_tests_climat_cst.csv");
#  DATA = fread(filename)
#  DD= as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")
# 
#  plot(DD,DATA$SWS1)
 
 



 
 # datmax=max(DATA$Time)
# datmin=min(DATA$Time)
# 
# nicolasOutput='F'
# if (nicolasOutput) {
#   head(DATA)
#   quartz()
#   plot(DATA$Psi_LSym, type='l', ylim=c(-5,0), col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   #lines(DATA$Psi_LApo_temp, type='l', col=adjustcolor(2, 4), lty=2)
# 
#   #lines(DATA$Psi_LSym, type='l', col=adjustcolor(2, 0.4), lty=2)
#   lines(DATA$Psi_TApo, type='l', col=adjustcolor(3, 0.8))
#   #lines(DATA$Psi_TSym~DATA$Time, type='l', ylim=c(-8,0), col=4, lty=2)
#   lines(DATA$Psi_AllSoil, col=5, type='l', yaxt="n", ylab="", lwd=2)
#   lines(DATA$PsiSoil1, col="brown", type='l')
#   lines(DATA$PsiSoil2, col="brown", type='l')
#   lines(DATA$PsiSoil3, col="brown", type='l')
# 
# 
# 
#   quartz()
#   plot(DATA$Psi_LApo[1:24], type='l', ylim=c(-3,0), col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   lines(DATA$Psi_LSym[1:24], type='l', col=adjustcolor(2, 0.7))
#   quartz()
#   plot(DATA$Psi_TApo[1:24], type='l', ylim=c(-3,0), col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   lines(DATA$Psi_TSym[1:24], type='l', col=adjustcolor(2, 0.7))
# 
#   quartz()
#   plot(DATA$gs_lim [1:24], type='l', col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   lines(DATA$gcanopy_lim [1:24], type='l', col=adjustcolor(2, 0.7))
#   par(new=T)
#   plot(DATA$Psi_LApo[1:24], ylim=c(-3,0) ,type='l', yaxt='n',  col=1, lty=1, ylab="", xlab="")
#   lines(DATA$Psi_LSym[1:24], type='l', col=adjustcolor(2, 0.7))
#   axis(4)
# } else { # FP outputs
#   quartz()
#   par(mfrow=c(2,2), pty='s')
#   #par(mfrow=c(2,4), pty='s')
#   plot(DATA$Psi_LApo~DATA$Time, type='l',xlim=c(datmin, datmax), ylim=c(-8,0), col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   lines(DATA$Psi_LSym~DATA$Time, type='l', ylim=c(-8,0), col=2, lty=2)
#   lines(DATA$Psi_TApo~DATA$Time, type='l', ylim=c(-8,0), col=3)
#   lines(DATA$Psi_TSym~DATA$Time, type='l', ylim=c(-8,0), col=4, lty=2)
#   lines(DATA$Psi_AllSoil~DATA$Time, col=5, type='l', yaxt="n", ylab="", lwd=2)
#   text(x=DATA$Time[200], y=-4, paste("Survie=",DD, "days"), cex=0.7)
#   #text(x=DATA$Time[250], y=-5, paste("C_Apo,T_Symp, No Cavit"), cex=1)
#   plot(DATA$Diag_timeStepInHours~DATA$Time,   type='b', ylab="time step (h)",log="y")
#   #plot(DATA$Diag_nwhile_cavit~DATA$Time,   type='b', ylab="", ylim=c(1,5))
#   #plot(DATA$Diag_deltaRegulMax~DATA$Time,   type='b', ylab="deltaRegulMax", ylim=c(0,0.25))
#   #plot(DATA$Diag_deltaPLCMax~DATA$Time,   type='b', ylab="deltaPLCMax(%)", ylim=c(0,1))
# 
#   #itmin=24/tstep*20+1;itmax=24/tstep*24
#   itmin=24/tstep*30+1;itmax=24/tstep*34
#   #itmin=24/tstep*0+1;itmax=24/tstep*4
#   plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], type='l',ylim=c(-4,0),  col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
#   lines(DATA$Psi_LSym[itmin:itmax]~DATA$Time[itmin:itmax], type='l',  col=2, lty=2)
#   lines(DATA$Psi_TApo[itmin:itmax]~DATA$Time[itmin:itmax], type='l',  col=3)
#   lines(DATA$Psi_TSym[itmin:itmax]~DATA$Time[itmin:itmax], type='l',  col=4, lty=2)
#   lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], col=5, type='l', yaxt="n", ylab="", lwd=2)
#   text(x=DATA$Time[20], y=-4, paste("Survie=",DD, "days"), cex=0.7)
#   #text(x=DATA$Time[250], y=-5, paste("C_Apo,T_Symp, No Cavit"), cex=1)
# 
#   #plot(DATA$Diag_nwhile_cavit[1:itmax]~DATA$Time[1:itmax],   type='b', ylab="", ylim=c(1,5))
#   plot(DATA$Diag_timeStepInHours[1:itmax]~DATA$Time[1:itmax],   type='b', ylab="time step (h)",log="y")
#   #plot(DATA$Diag_deltaRegulMax[itmin:itmax]~DATA$Time[itmin:itmax],   type='b', ylab="deltaRegulMax", ylim=c(0,0.25))
#   #plot(DATA$Diag_deltaPLCMax[itmin:itmax]~DATA$Time[itmin:itmax],   type='b', ylab="deltaPLCMax(%)", ylim=c(0,1))
# }
# 
# # SENSITIVITY TO COMPUTATIONAL MODE
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_180nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATAref=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_60nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_1h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_n1_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_1h_n1=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_f1_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_1h_f1=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_2h_120nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_2h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_2h_n1_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_2h_n1=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_2h_f1_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_2h_f1=DATA
# 
# ## sensibitivity at 1h
# quartz(title='sensitivity to small time step (for large =1h)', width=21, height=13)
# par(mfrow=c(2,3), pty='s')
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-2,0),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 7-14"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-2.8,-1.4),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 40-47"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3.6,-2.8),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 107-114"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-0.5,-0.1),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 7-14"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-2.4,-1.2),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 40-47"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-3.4,-2.9),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 107-114"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_1h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_1h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "1h-1min", "1h-variable","1h-1h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# 
# 
# 
# quartz(title='sensitivity to small time step (for large =2h)', width=21, height=13)
# par(mfrow=c(2,3), pty='s')
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-2,0),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 7-14"), ylab="PsiL", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h-1min", "2h-variable","2h-2h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-2.8,-1.4),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 40-47"), ylab="PsiL", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h-1min", "2h-variable","2h-2h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3.6,-2.8),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 107-114"), ylab="PsiL", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h-1min", "2h-variable","2h-2h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-0.5,-0.1),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 7-14"), ylab="PsiS", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h-1min", "2h-variable","2h-2h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-2.4,-1.2),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 40-47"), ylab="PsiS", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h-1min", "2h-variable","2h-2h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax],ylim=c(-3.4,-2.9),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 107-114"), ylab="PsiS", xlab="time")
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# DATA=DATA_2h_n1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3)
# DATA=DATA_2h_f1;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4)
# legend("topright", c("1h-20s (ref)", "2h", "2hn1","2hf1"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# 
# 
# #### SENSITIVITY TO LARGE TIME STEP
# 
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_180nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATAref=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_1h_60nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_1h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_2h_120nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_2h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_4h_240nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_4h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_6h_360nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_6h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_8h_480nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_8h=DATA
# filename  = paste0("../Results_model/Champenoux_tests_climat_cst_12h_720nts_v2.csv");DATA = fread(filename);DD=strftime(max(DATA$Time), format="%j");datmax=max(DATA$Time);datmin=min(DATA$Time)
# DATA_12h=DATA
# 
# quartz(title='sensitivity to large time step (small=1min)', width=21, height=13)
# par(mfrow=c(2,3), pty='s')
# #quartz()
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-2.5,0),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 7-14"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3.5,-1),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 40-47"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3.6,-2.7),type='l', col='black', lty=1,lwd=2, main=paste("PsiL days 107-114"), ylab="PsiL", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_LApo[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=7;day2=14
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-1,-0.1),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 7-14"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=40;day2=47
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3,-1.),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 40-47"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# day1=107;day2=114
# tstep=1;itmin=24/tstep*day1+1;itmax=24/tstep*day2; pch=16
# DATA=DATAref;plot(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], ylim=c(-3.4,-2.6),type='l', col='black', lty=1,lwd=2, main=paste("PsiS days 107-114"), ylab="PsiS", xlab="time")
# DATA=DATA_1h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=2)
# tstep=2;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_2h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=3, lty=2)
# tstep=4;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_4h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=4, lty=2)
# tstep=6;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_6h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch, type = "b", col=5, lty=2)
# tstep=8;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_8h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=6, lty=2)
# tstep=12;itmin=24/tstep*day1+1;itmax=24/tstep*day2
# DATA=DATA_12h;lines(DATA$Psi_AllSoil[itmin:itmax]~DATA$Time[itmin:itmax], pch=pch,  type = "b",col=7, lty=2)
# legend("topright", c("ref", "1h", "2h",  "4h", "6h", "8h", "12h"), col = c(1:7),lty = c(1,2,2,2,2,2,2), pch = pch*c(NA, 1,1,1,1,1,1),merge = TRUE, bg = "gray90")
# 
# 
# 
# 
# 
# 
# 
# 
# # OLD OUTPUTS
# plot(DATA$Psi_LApo_n[1:48]~DATA$Time[1:48], type='l', ylim=c(-8,0), col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
# #lines(DATA$Psi_LSym_n, type='l', ylim=c(-8,0), col=2, lty=2)
# lines(DATA$Psi_TApo_n[1:48]~DATA$Time[1:48], type='l', ylim=c(-5,0), col=3)
# lines(DATA$Psi_TSym_n[1:48]~DATA$Time[1:48], type='l', ylim=c(-8,0), col=4, lty=2)
# lines(DATA$PsiAllSoil[1:48]~DATA$Time[1:48], col=5, type='l', yaxt="n", ylab="", lwd=2)
# text(x=DATA$Time[200], y=-4, paste("Survie=",DD, "days"), cex=0.7)
# text(x=DATA$Time[250], y=-5, paste("C_Apo,T_Symp, No Cavit"), cex=1)
# 
# plot(DATA$Diag_nwhile_cavit[1:48]~DATA$Time[1:48],   type='b', ylab="", ylim=c(1,5))
# 
# 
# 
# plot(DATA$CavitAbove~DATA$Time, type='l',xlim=c(datmin, datmax),  col=1, lty=1, main=paste("time step =",tstep, "h"), ylab="Psi", xlab="time")
