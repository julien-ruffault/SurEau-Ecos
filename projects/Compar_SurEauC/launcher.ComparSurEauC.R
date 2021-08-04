# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
# Test Launcher to run SurEau-ECOS (V4.0) on Champenoux
# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory



# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))                # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 

climateData_path          <- paste0(mainDir,'/projects/Compar_SurEauC/Climat_constant_test_champenoux.csv') # <-- indicate here the path to input climate data 

soilParameters_path       <- paste0(mainDir,'/projects/Compar_SurEauC/Soil_test_champenoux.csv')
vegetationParameters_path <- paste0(mainDir,'/projects/Compar_SurEauC/VegetationParams_ComparSurEauC.csv')
output_path               <- paste0(mainDir,'/projects/Compar_SurEauC/test.csv')        


# create model input files --------------------------------------------------
modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                numericalScheme = 'Implicit',
                                                defoliation = F,
                                                resetSWC = T)       

modeling_options     <- create.modeling.options(timeStepForEvapo=1,
                                                compOptionsForEvapo = 'Normal',
                                                numericalScheme = 'Implicit',
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                defoliation = F, resetSWC=T)     

modeling_options$thresholdMortatliy <- 100

simulation_parameters <- create.simulation.parameters(startYearSimulation = 1990,                        
                                                      endYearSimulation = 1990,
                                                      mainDir= mainDir,
                                                      resolutionOutput = "subdaily",
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

simulation_parameters$compOptionsForEvapo

climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
stand_parameters <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)
soil_parameters  <- create.soil.parameters(filePath=soilParameters_path, depths = c(0.373333, 0.746666, 1.12)) 


TTT = read.vegetation.file(filePath=vegetationParameters_path ,modeling_options=modeling_options)
TTT$kPlantInit
vegetation_parameters <- create.vegetation.parameters(listOfParameters=TTT, stand_parameters = stand_parameters, soil_parameter = soil_parameters, modeling_options = modeling_options)

vegetation_parameters$kPlantInit

vegetation_parameters$gmin_T<-0
vegetation_parameters$k_LSymInit <- 1.5
vegetation_parameters$k_TSymInit <- 0.26
vegetation_parameters$VolumeLiving_TRB <- 7


vegetation_parameters$LDMC <- 500

vegetation_parameters$P12_gs
vegetation_parameters$P88_gs
vegetation_parameters$gmin20
vegetation_parameters$TPhase_gmin <-37.5
vegetation_parameters$gsNight


# run SurEau-Ecos ---------------------------------------------------------
run.SurEau_Ecos(modeling_options = modeling_options ,
        simulation_parameters = simulation_parameters, 
       climate_data = climate_data,
       stand_parameters = stand_parameters, 
       soil_parameters = soil_parameters,
       vegetation_parameters = vegetation_parameters)

# analyse outputs ---------------------------------------------------------
#File Hervé
# DATARV=read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico_out4_ter.csv"), dec=",", sep=";", h=T)
# DATARV=read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico_out2.csv"), dec=",", sep=";", h=T)
# DATARV=read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico11_gbark0.csv"), dec=",", sep=";", h=T)
DATARV=read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico12.csv"), dec=",", sep=";", h=T)

head(DATARV)


# # for analyses / subdaily time scales 
filename  = paste0(mainDir,"/Projects/Compar_SurEauC/test.csv")
DATA = read.csv(filename,header=T, dec='.',sep="")
DATA$DD = as.POSIXct(DATA$Time,origin = "1970-01-01",tz = "UTC")

SWStot = DATA$SWS1+DATA$SWS2+DATA$SWS3
DATA$SWS1 + DATA$SWS2 + DATA$SWS3
Qsoil = (DATARV$Q_soil1+DATARV$Q_soil2+DATARV$Q_soil3)/10.5*6

quartz()
par(mfrow=c(2,2))
plot(Qsoil, type='l')
lines(SWStot, col=2)
plot(DATA$SWS1, type='l')
lines(DATARV$Q_soil1/10.5*6, col=2)
plot(DATA$SWS2, type='l')
lines(DATARV$Q_soil2/10.5*6, col=2)
plot(DATA$SWS3, type='l')
lines(DATARV$Q_soil3/10.5*6, col=2)


lengthOut=2821
quartz()
par(mfrow=c(2,2))
plot(DATARV$P_leaf_sym[1:lengthOut], type="l", ylab="Psi_L_Symp", col=2, lwd=2)
points(DATA$Psi_LSym[2:lengthOut],type='l', lwd=2, col=adjustcolor(1, alpha.f=0.6))
plot(DATARV$P_soil1[1:lengthOut], type="l" , col=2, ylab="Psi_Soil1", lwd=2)
points(DATA$PsiSoil1[2:lengthOut],type='l', lwd=2)
plot(DATARV$P_soil2[1:lengthOut], type="l",col=adjustcolor('firebrick1', alpha.f=0.6), ylab="Psi_Soil2", lwd=2)
points(DATA$PsiSoil2[2:lengthOut],type='l', lwd=2)
plot(DATARV$P_soil3[1:lengthOut], type="l",col=adjustcolor('firebrick1', alpha.f=0.6), ylab="Psi_Soil3", lwd=2)
points(DATA$PsiSoil3[2:lengthOut],type='l', lwd=2)


plot(DATA$k_Plant[2:lengthOut],type='l', lwd=2)

points(DATARV$P_soil1[1:lengthOut], type="l" , col=2, ylab="Psi_Soil1", lwd=2)

quartz()
par(mfrow=c(2,2))
plot(DATA$Tair[1:24], type='l', ylim=c(15,35))
lines(DATA$leafTemperature[1:24], type='l', col=2)
plot(DATA$Tair, type='l', ylim=c(15,35))
lines(DATA$leafTemperature, type='l',  col=adjustcolor(2, 0.5))
plot(DATARV$T_air[1:24], type='l', ylim=c(15,35))
lines(DATARV$T_leaf[1:24], type='l', ylim=c(15,35), col=2)
plot(DATARV$T_air, type='l', ylim=c(15,35))
lines(DATARV$T_leaf, type='l', ylim=c(15,35), col=adjustcolor(2, 0.5))

quartz()
par(mfrow=c(2,3))
plot(DATA$Tair[2:25], type='l', ylim=c(15,35), ylab="Air temp")
lines(DATARV$T_air[1:24], type='l', ylim=c(15,35), col =2)
legend(1, 35,  c("SurEau.Ecos", "SurEau.C"), col=c(1,2), lty=1)

plot(DATA$leafTemperature[2:25], type='l', ylim=c(15,35), ylab="Leaf temp")
lines(DATARV$T_leaf[1:24], type='l', ylim=c(15,35), col =2)

plot(DATA$leafVPD[2:25], type='l', ylab="VPD_Leaf")
lines(DATARV$VPD_Leaf[1:24], type='l', col =2)

plot(DATA$Elim[2:49]-DATA$Emin[2:49],type='l', ylab="E_stom", ylim=c(0,1.3))
lines(DATARV$E_stomata[1:48]-DATARV$E_cuti_m2[1:48], col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATA$Emin[2:49], type="l", ylab="E_cuti", ylim=c(0,0.15))
lines(DATARV$E_cuti_m2[1:48],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATA$Psi_LSym [2:49], type="l", ylab="P_l_sym", ylim=c(-1.9,0) )
lines(DATARV$P_leaf_sym [1:48], type='l', col =2)
lines(DATARV$P_evap_apo [1:48], type='l', col =2, lty=2)




#VERIF TLeaf
res=NULL
for(n in 1:24)
  {

 
  Tair = DATA$Tair[n]
  PAR = DATA$PAR[n]
Potential_PAR = DATA$Potential_PAR[n]
RH = DATA$RH[n]
WS = DATA$WS[n]
gs  = DATA$gs_lim[n]
g_cuti = DATA$gmin[n]
PsiLeaf =  DATA$Psi_LSym[n]

res=rbind(res, compute.Tleaf(Tair=Tair,PAR=PAR, RH=RH, POTENTIAL_PAR = Potential_PAR, WS=WS,gs=gs, g_cuti=g_cuti, PsiLeaf=PsiLeaf))

}

plot(res[,1], type='l')
lines(DATA$leafTemperature[1:24], type='l', col=3)


#VERIF VPD HERVE OLD
Tleaf = DATARV$T_leaf
RH = DATARV$RH_air
PsiLeaf = DATARV$P_evap_apo
PsiLeaf = DATARV$P_leaf_sym
VPD_Leaf_RV = (611.21*exp((18.678-Tleaf/234.5)*Tleaf/(257.14+Tleaf)) * (exp(PsiLeaf*2.16947115/(Tleaf+273.15)) - RH/100)/1000)

Tleaf = DATA$leafTemperature
RH = DATA$RH
PsiLeaf = DATA$Psi_LSym
VPD_Leaf_Nico = (611.21*exp((18.678-Tleaf/234.5)*Tleaf/(257.14+Tleaf)) * (exp(PsiLeaf*2.16947115/(Tleaf+273.15)) - RH/100)/1000)

quartz()
par(mfrow=c(1,1))
plot(VPD_Leaf[1:48], col=2, type='l', main = "VPD_Leaf")
lines(DATARV$VPD_Leaf[1:48])
legend(20, 3, c("OutPut", "recompute"), col=c(1,2), lty=1)

# plot(DATARV$g_canopy[1:48]*VPD_Leaf[1:48]/100, type='l', ylab="E")
# lines(DATARV$E_stomata[1:48], type='l', col=2)

plot(VPD_Leaf_Nico[1:48], col=2, type='l', main = "Nico")
lines(DATA$leafTemperature[1:48])





plot(DATARV$g_canopy[1:48], type='l', ylim=c(0,200))
lines(DATA$gcanopy_lim[2:49], type='l', col=2)
lines(DATARV$g_s[1:48], type='l', ylim=c(0,200), lty=2)
lines(DATA$gs_lim [2:49], type='l', col=2, lty=2)

plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_cuti", ylim=c(0,1.3))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$VPD_Leaf[1:48], type="l", ylab="VPD_Leaf")
lines(DATA$leafVPD[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))









plot(DATARV$E_Plant_m2/10.5, type="l", ylab="Emin")

quartz()
par(mfrow=c(2,2))
plot(DATARV$E_Plant_m2[1:48], type="l", ylab="E plant")
lines(DATA$Elim[2:49]+DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(Qsoil[1:48], type='l', ylim=c(295,312))
lines(SWStot[2:49], col=2)
aaa=(DATA$Elim+DATA$Emin)[2:2704]-DATARV$E_Plant_m2[1:2703]
plot(aaa[1:48], type='l', ylab="diff E (SureauEcos-SurEau-C)")
plot(DATARV$P_leaf_sym[1:48], type="l", ylab="Psi_L_Symp", lwd=2)
points(DATA$Psi_LSym[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)

sum((DATA$Elim+DATA$Emin)[2:2704])
sum(DATARV$E_Plant_m2[1:2703])
sum(DATARV$E_stomata[1:2703]+DATARV$E_cuti_m2[1:2703])

lengthOut=2821
quartz()
par(mfrow=c(2,2))
plot(DATARV$P_leaf_sym[1:lengthOut], type="l", ylab="Psi_L_Symp", lwd=2)
points(DATA$Psi_LSym[2:lengthOut],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)
plot(DATARV$P_soil1[1:lengthOut], type="l", ylab="Psi_Soil1", lwd=2)
points(DATA$PsiSoil1[2:lengthOut],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)
plot(DATARV$P_soil2[1:lengthOut], type="l", ylab="Psi_Soil2", lwd=2)
points(DATA$PsiSoil2[2:lengthOut],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)
plot(DATARV$P_soil3[1:lengthOut], type="l", ylab="Psi_Soil3", lwd=2)
points(DATA$PsiSoil3[2:lengthOut],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)


quartz()
par(mfrow=c(2,2))
plot(DATARV$E_Plant_m2, type="l", ylab="E plant")
lines(DATA$Elim+DATA$Emin,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(Qsoil, type='l')
lines(SWStot, col=2)
aaa=(DATA$Elim+DATA$Emin)[2:2704]-DATARV$E_Plant_m2[1:2703]
plot(aaa, type='l', ylab="Diff")
plot(DATARV$P_leaf_sym, type="l", ylab="Psi_L_Symp", lwd=1)
points(DATA$Psi_LSym,type='l',col=adjustcolor('firebrick1', alpha.f=0.5), lwd=1)
par(new=T)
plot(DATARV$PLC_leaf, type="l", ylab="Psi_L_Symp", lwd=2)
points(DATA$PLC_Leaf,type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)



Etot= DATA$Elim + DATA$Emin

quartz()
par(mfrow=c(2,2))
plot(DATARV$g_cuti[1:48], type="l", ylab="Emin")
lines(DATA$gmin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$g_s[1:48], type="l", ylab="Emin")
lines(DATA$gs_lim[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$T_leaf[1:48], type="l", ylab="Emin")
lines(DATA$leafTemperature[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$VPD_Leaf[1:48], type="l", ylab="Emin")
lines(DATA$leafVPD[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$E_stomata[1:48], type="l", ylab="Emin")
lines(DATA$Elim[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$E_cuti_m2[1:48], type="l", ylab="Emin")
lines(DATA[1:48], col=2)

lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
lines(Etot[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))


plot(DATARV$P_leaf_sym, type="l", ylab="Psi_L_S", lwd=2)
points(DATA$Psi_LSym,type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)
plot(DATARV$P_soil1, type="l", ylab="Psi_L_S", lwd=2)
points(DATA$PsiSoil1,type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)





SWStot=DATA$SWS1+DATA$SWS2+DATA$SWS3

DATA$SWS1 + DATA$SWS2 + DATA$SWS3
Qsoil = (DATARV$Q_soil1+DATARV$Q_soil2+DATARV$Q_soil3)/10.5*6
quartz()
plot(Qsoil, type='l')
lines(SWStot, col=2)

plot(DATARV$P_leaf_apo~DATARV$Q_soil1)
points(DATA$Psi_LApo~SWStot, col=2)
Qsoil1_mm=(DATARV$Q_soil1/10.5*6)
Qsoil2_mm=(DATARV$Q_soil2/10.5*6)
plot(DATARV$P_soil1~Qsoil1_mm)
points(DATA$PsiSoil1~DATA$SWS1, col=2)
plot(DATARV$P_soil2~Qsoil2_mm)
points(DATA$PsiSoil2~DATA$SWS2, col=2)



plot()
plot(DATARV$P_soil1, type="l", ylab="Psi_L_S", lwd=2)
points(DATA$PsiSoil1,type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lwd=2)


par(new=T)
plot(DATARV$E_Plant_m2, type="l", ylab="", ylim=c(0,1.3), yaxt="n")
points(DATA$Elim+DATA$Emin,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
axis(4)

plot(DATARV$g_s~DATARV$P_leaf_sym, type="p", ylab="gs", ylim=c(0,200))
points(DATA$gs_lim~DATA$Psi_LSym,type='p',col=adjustcolor('firebrick1', alpha.f=0.2))
                                                          
plot(DATARV$g_canopy~DATARV$P_leaf_sym, type="p", ylab="gcanopy", ylim=c(0,40))
points(DATA$gcanopy_lim ~DATA$Psi_LSym,type='p',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$g_cuti[1:100], type="l", ylab="gcuti")
points(DATA$gmin[2:99],type='l',col=adjustcolor('firebrick1', alpha.f=0.4))



quartz()
par(mfrow=c(2,2))
plot(DATARV$g_canopy[1:48], type="l", ylab="T_Leaf")
lines(DATA$gcanopy_lim[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$E_stomata [1:48], type="l", ylab="E_Leaf")

plot(DATARV$E_Plant_m2 [1:48], type="l", ylab="E_Leaf")
lines(DATA$Elim[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$g_cuti[1:48], type="l", ylab="T_Leaf")
lines(DATA$gmin [2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))


plot(DATARV$P_leaf_sym[1:48], type="l", ylab="P_Leaf_Symp", ylim=c(-2,0))
lines(DATA$Psi_LSym[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.7))



Erv=DATARV$g_canopy[1:48] *DATARV$VPD_Leaf[1:48]/101
E=DATA$gcanopy_lim[1:48]*DATA$leafVPD[1:48]/101

plot(Erv, type="l", ylab="T_Leaf")
lines(E, type="l", ylab="T_Leaf", col=2)

quartz()
par(mfrow=c(2,2))
plot(DATARV$T_leaf[1:48], type="l", ylab="T_Leaf")
lines(DATA$leafTemperature[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$T_air [1:48], type="l", ylab="T_Leaf")
lines(DATA$Tair[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_Leaf_mmol_m2", ylim=c(0,.2))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_Leaf_mmol_m2", ylim=c(0,.2))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))



plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_Leaf_mmol_m2", ylim=c(0,.2))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_Leaf_mmol_m2", ylim=c(0,.2))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))


quartz()
par(mfrow=c(2,2))
# plot(DATARV$g_s[1:48], type='l')
# lines(DATA$gs_lim[2:49], type='l', col=adjustcolor(2,0.7))
plot(DATARV$g_canopy[1:48], type='l', ylim=c(0,200))
lines(DATA$gcanopy_lim[2:49], type='l', col=2)
lines(DATARV$g_s[1:48], type='l', ylim=c(0,200), lty=2)
lines(DATA$gs_lim [2:49], type='l', col=2, lty=2)
plot(DATARV$E_stomata[1:48]-DATARV$E_cuti_m2[1:48], type="l", ylab="E_stom", ylim=c(0,1.3))
lines(DATA$Elim[2:49]-DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$E_cuti_m2[1:48], type="l", ylab="E_cuti", ylim=c(0,1.3))
lines(DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$VPD_Leaf[1:48], type="l", ylab="VPD_Leaf")
lines(DATA$leafVPD[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))




plot(DATARV$P_leaf_sym[1:48], type="l", ylab="P_Leaf_Symp", ylim=c(-2,0))
lines(DATA$Psi_LSym[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.7))

plot(DATARV$Q_leaf_s[1:48], type="l", ylab="Q_Leaf_sym", ylim=c(0,1.3))
lines(DATA$Q_LSym_L[2:49]*10.5/stand_parameters$LAImax,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$Q_leaf_a[1:48], type="l", ylab="Q_Leaf_sym", ylim=c(0,1.3))
lines(DATA$Q_LApo_L[2:49]*10.5/stand_parameters$LAImax,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

quartz()
plot(DATARV$P_soil1, type="l", ylab="Psoil")
lines(DATA$PsiSoil1,type='l',col=adjustcolor('firebrick1', alpha.f=0.7))
quartz()
plot(DATARV$Q_soil1, type="l", ylab="Psoil")
lines(DATA$SWS1,type='l',col=adjustcolor('firebrick1', alpha.f=0.7))


quartz()
par(mfrow=c(2,2))
# plot(DATARV$g_s[1:48], type='l')
# lines(DATA$gs_lim[2:49], type='l', col=adjustcolor(2,0.7))
plot(DATARV$g_canopy, type='l', ylim=c(0,40))
lines(DATA$gcanopy_lim, type='l', col=2)
plot(DATARV$P_leaf_sym, type="l", ylab="P_Leaf_Symp")
lines(DATA$Psi_LSym,type='l',col=adjustcolor('firebrick1', alpha.f=0.7))
plot(DATARV$Q_leaf_s, type="l", ylab="P_Leaf_Symp")
lines(DATA$Q_LSym_L*10.5/stand_parameters$LAImax,type='l',col=adjustcolor('firebrick1', alpha.f=0.7))
plot(DATARV$Q_leaf_s, type="l", ylab="P_Leaf_Symp")
lines(DATA$Q_LSym_L*10.5/stand_parameters$LAImax,type='l',col=adjustcolor('firebrick1', alpha.f=0.7))
QtotRV = DATARV$Q_trunk_a + DATARV$Q_trunk_s + DATARV$Q_branch_a + DATARV$Q_branch_s + DATARV$Q_root_a + DATARV$Q_root_s
Qtot =(DATA$Q_TSym_L+DATA$Q_TApo_L)*10.5 #stand_parameters$LAImax
plot(QtotRV, type='l', ylab="Q plant tot (kg)", ylim=c(0,50))
lines(Qtot, type='l', col=adjustcolor("firebrick1", alpha.f=0.6))




plot(DATARV$E_Leaf_m2[1:48]-DATARV$E_cuti_m2[1:48], type="l", ylab="E_stom", ylim=c(0,1.3))
lines(DATA$Elim[2:49]-DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))



quartz()
par(mfrow=c(2,2))
plot(DATARV$g_s[1:48], type='l')
lines(DATA$gs_lim[2:49], type='l', col=adjustcolor(2,0.7))
plot(DATARV$P_trunk_s[1:48], type="l", ylab="P_Leaf_Symp", ylim=c(-2,0))
lines(DATA$Psi_TSym[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.7))
plot(DATARV$E_Leaf_m2[1:48], type="l", ylab="E_Leaf_mmol_m2", ylim=c(0,1.3))
lines(DATA$Elim[2:49]+DATA$Emin[2:49],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
QtotRV = DATARV$Q_trunk_a + DATARV$Q_trunk_s + DATARV$Q_branch_a + DATARV$Q_branch_s + DATARV$Q_root_a + DATARV$Q_root_s
Qtot =(DATA$Q_TSym_L+DATA$Q_TApo_L)*10.5 #stand_parameters$LAImax
plot(QtotRV[1:48], type='l', ylab="Q plant tot (kg)", ylim=c(0,50))
lines(Qtot[2:49], type='l', col=adjustcolor("firebrick1", alpha.f=0.6))





pdf("comparsureaC-Ecos.pdf")
par(mfrow=c(2,2))
plot(DATARV$P_leaf_sym, type="l", ylab="P_Leaf_Symp")
lines(DATA$Psi_LSym,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
legend(0, -4, c("SurEau.C", "SurEau-Ecos"), col=c(1, adjustcolor('firebrick1', alpha.f=0.6)),lty=1, bty="n", cex=0.8)
plot(DATARV$P_leaf_apo, type="l", ylab="P_Leaf_apo")
lines(DATA$Psi_LApo,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
par(new=T)
plot(DATARV$PLC_leaf, type="l", ylab="", yaxt="n")
lines(DATA$PLC_Leaf,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
axis(4)

plot(DATARV$P_trunk_s, type="l", ylab="PTrunk_Symp")
lines(DATA$Psi_TSym,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$P_trunk_a, type="l", ylab="P_Trunk_Apo")
lines(DATA$Psi_TApo,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
par(new=T)
plot(DATARV$PLC_trunk, type="l", ylab="", yaxt="n")
lines(DATA$PLC_Trunk,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
axis(4)

#quartz()
par(mfrow=c(2,2))
plot(DATARV$P_leaf_sym[1:24], type="l", ylab="P_Leaf_Symp", ylim=c(-2,0))
lines(DATA$Psi_LSym[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
legend(0, -1, c("SurEau.C", "SurEau-Ecos"), col=c(1, adjustcolor('firebrick1', alpha.f=0.6)),lty=1, bty="n", cex=0.8)
plot(DATARV$P_leaf_apo[1:24], type="l", ylab="P_Leaf_apo", ylim=c(-2,0))
lines(DATA$Psi_LApo[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$P_trunk_s[1:24], type="l", ylab="PTrunk_Symp", ylim=c(-1,0))
lines(DATA$Psi_TSym[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATARV$P_trunk_a[1:24], type="l", ylab="P_Trunk_Apo", ylim=c(-1,0))
lines(DATA$Psi_TApo[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

# quartz()
# plot(DATA$gcanopy_lim, type='l')


#quartz()
par(mfrow=c(2,2))
plot(DATARV$E_Leaf_m2, type="l", ylab="E_Leaf_mmol_m2")
lines(DATA$Elim+DATA$Emin,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
legend(1000, 1, c("SurEau.C", "SurEau-Ecos"), col=c(1, adjustcolor('firebrick1', alpha.f=0.6)),lty=1, bty="n", cex=0.8)
plot(DATARV$E_Leaf_m2[1:24], type="l", ylab="E_Leaf__mmol_m2")
lines(DATA$Elim[1:24]+DATA$Emin[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$T_leaf, type="l", ylab="T_leaf")
lines(DATA$leafTemperature,type='l',col=adjustcolor('firebrick1', alpha.f=0.6))

plot(DATARV$T_leaf[1:24], type="l", ylab="Temperature (Leaf and Air)")
lines(DATA$leafTemperature[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
lines(DATA$Tair[1:24],type='l',col=adjustcolor("firebrick1", alpha.f=0.6), lty=2)
lines(DATARV$T_air,type='l',col=adjustcolor(1, alpha.f=0.6), lty=2)
legend(3,30,c("Tleaf","Tair"),lty=c(1,2), cex=.8, bty="n")


#quartz()
quartz()
par(mfrow=c(1,2))
QLeaf = (DATA$Q_LSym_L+DATA$Q_LApo_L)*10.5/stand_parameters$LAImax
QLeafRV = DATARV$Q_leaf_s + DATARV$Q_leaf_a
plot(QLeafRV, type='l', ylab="Qleaf tot (kg)")
lines(QLeaf, type='l', col=adjustcolor("firebrick1", alpha.f=0.6))

QtotRV = DATARV$Q_trunk_a + DATARV$Q_trunk_s + DATARV$Q_branch_a + DATARV$Q_branch_s + DATARV$Q_root_a + DATARV$Q_root_s
Qtot =(DATA$Q_TSym_L+DATA$Q_TApo_L)*10.5 #stand_parameters$LAImax
plot(QtotRV, type='l', ylab="Q plant tot (kg)", ylim=c(0,50))
lines(Qtot, type='l', col=adjustcolor("firebrick1", alpha.f=0.6))



#-----------------------
#Climate
#quartz()
par(mfrow=c(2,2))
plot(DATA$Tair[1:24],type='l',col=adjustcolor("firebrick1", alpha.f=0.6), lty=1, ylab="Air temperature ")
lines(DATARV$T_air[1:24],type='l',col=adjustcolor(1, alpha.f=0.6), lty=1)
legend(3,30, c("SurEau.C", "SurEau-Ecos"), col=c(1, adjustcolor('firebrick1', alpha.f=0.6)),lty=1, bty="n", cex=0.8)
plot(DATARV$T_leaf[1:24], type="l", ylab="Leaf temperature ")
lines(DATA$leafTemperature[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATA$PAR[1:24], type="l", ylab="PAR umol/m2/s")
plot(DATA$VPD[1:24], type="l", ylab="VPD air")

dev.off()



#-----------------------
   nrow(DATA)
   plot(DATA$Psi_LApo[1:48])
   
   plot(DATA$DD,DATA$Psi_LSym,type='l',col='firebrick1',ylim=c(-6,0))
   lines(DATA$DD,DATA$Psi_LApo,type='l',col='firebrick4')
   lines(DATA$DD,DATA$Psi_AllSoil,type='l',col='blue')
   par(new=T)
   plot(DATA$DD,DATA$PLC_Leaf,type='l',col=1, ylab="", yaxt="n",xlab="")
   axis(4)
   
   plot(DATA$DD,DATA$regulFact,type='l',col=1, ylab="", xlab="")
   plot(DATA$DD,DATA$gcanopy_lim,type='l',col=1, ylab="",xlab="")
   plot(DATA$DD,DATA$gs_lim,type='l',col=1, ylab="",xlab="")

   head(DATA)
   as.Date(DATA$DD, format="%j")
   plot(DATA$DD,DATA$transpiration_mm ,type='l',col=1, ylab="",xlab="")
   plot(DATA$DD,DATA$EminT,type='l',col=2, ylab="",xlab="")
   
   head(DATA)
   plot(DATA$ETP[1:48] ,type='l',col=1, ylab="",xlab="")
   plot(DATA$VPD[1:48] ,type='l',col=1, ylab="",xlab="")
   plot(DATA$Elim[1:48] ,type='l',col=1, ylab="",xlab="")
   plot(DATA$regulFact[1:48] ,type='l',col=1, ylab="",xlab="")
   plot(DATA$transpiration_mm[1:48] ,type='l',col=1, ylab="",xlab="")

   