# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### #
# Test Launcher to compare SurEau-ECOS with Sureau-C (Cochard et al 2021 AFS)
# Authors : <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)> / 25/11/2021
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
                                                compOptionsForEvapo = 'Normal',
                                                numericalScheme = 'Implicit',
                                                constantClimate=T,
                                                stomatalRegFormulation = "Sigmoid",
                                                defoliation = F, 
                                                thresholdMortality =100,
                                                resetSWC=T,
                                                transpirationModel="Jarvis")     



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
soil_parameters  <- create.soil.parameters(filePath=soilParameters_path) 


TTT = read.vegetation.file(filePath=vegetationParameters_path ,modeling_options=modeling_options)
TTT$kPlantInit
vegetation_parameters <- create.vegetation.parameters(listOfParameters=TTT, stand_parameters = stand_parameters, soil_parameter = soil_parameters, modeling_options = modeling_options)

#On mets le gmin Tronc à 0 pour limiter les interférence avec gmin root+Trunk+Branch
vegetation_parameters$gmin_T <- 0
vegetation_parameters$k_LSymInit <- 1.5
vegetation_parameters$k_TSymInit <- 2.69/10.5 #0.0269 #.84
vegetation_parameters$VolumeLiving_TRB <- 39.9 #/20


vegetation_parameters$LDMC <- 500

vegetation_parameters$P12_gs
vegetation_parameters$P88_gs
vegetation_parameters$gmin20
vegetation_parameters$TPhase_gmin <-37.5

vegetation_parameters$P50_VC_Leaf <- -3.4 #-100 #-3.4
vegetation_parameters$P50_VC_Trunk <- -3.4 #-100 #-3.4


vegetation_parameters$VolumeLiving_TRB
vegetation_parameters$SymplasmicFrac_Trunk <- 0.0917 #  0.0975 #Trunk 0.0917

vegetation_parameters$SymplasmicFrac_Trunk * vegetation_parameters$VolumeLiving_TRB

# Run SurEau-Ecos ---------------------------------------------------------

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
DATARV = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico12.csv"), dec=",", sep=";", h=T)
DATARV_P50_100 = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico17.csv"), dec=",", sep=";", h=T)
DATARV_Q20 = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico18.csv"), dec=",", sep=";", h=T)
DATARV_Q20_P50_100 = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico19.csv"), dec=",", sep=";", h=T)
DATARV_TrunkEquiv = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico20.csv"), dec=",", sep=";", h=T)
DATARV_TrunkEquiv_P50_100 = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico21.csv"), dec=",", sep=";", h=T)
DATARV_48 = read.table(paste0(mainDir,"/Projects/Compar_SurEauC/nico21_48H.csv"), dec=".", sep=";", h=T)


DATARV = DATARV_Q20_P50_100
DATARV=DATARV_P50_100
DATARV = DATARV_TrunkEquiv
DATARV = DATARV_TrunkEquiv_P50_100
DATARV= DATARV_48
# # for analyses / subdaily time scales 
filename  = paste0(mainDir,"/Projects/Compar_SurEauC/test.csv")
DATA1 = read.csv(filename,header=T, dec='.',sep="")
DATA2 = read.csv(filename,header=T, dec='.',sep="")
DATA=DATA1

#Capacitance
#Ecos
PsiTSymp_Ecos = DATA$Psi_TSym[1:(nrow(DATA)-1)]
dQ_TSymp_Ecos = (DATA$Q_TSym_L[1:(nrow(DATA)-1)] - DATA$Q_TSym_L[2:(nrow(DATA))]) *1000000/18
dPsi_TSymp_Ecos = DATA$Psi_TSym[1:(nrow(DATA)-1)] - DATA$Psi_TSym[2:(nrow(DATA))]
C_TSymp_Ecos = dQ_TSymp_Ecos/dPsi_TSymp_Ecos
C_TSymp_EcosInt = DATA$C_TSym[2:24]*6

#RV
PsiTSymp_RV = DATARV$P_trunk_s
Q_TSymp_RV = DATARV$Q_trunk_s/1.75
dQ_TSymp_RV = (Q_TSymp_RV[1:(nrow(DATARV)-1)] - Q_TSymp_RV[2:(nrow(DATARV))]) *1000000/18
dPsi_TSymp_RV = PsiTSymp_RV[1:(nrow(DATARV)-1)] - PsiTSymp_RV[2:(nrow(DATARV))]
C_TSymp_RV = dQ_TSymp_RV/dPsi_TSymp_RV

quartz()
par(mfrow=c(2,3))
barplot(c(Q_S_InitRV = Q_TSymp_RV[1]*1000000/18,
          Q_S_InitEcos = DATA$Q_TSym_L[1] *1000000/18), ylab="QSymp_Sat mmol/m2 sol")

par(mar=c(4.1,4.1,4.1,4.1))
plot(C_TSymp_RV[1:24], col=adjustcolor(2, 1), type='b', ylab="C_TSymp_RV", col.axis=2)
points(C_TSymp_EcosInt, yaxt='n', ylab="", type='b', xaxt="n")
points(C_TSymp_Ecos[2:24],type='b', col=4)
legend(1, 16600, c("RV","Ecos","Ecos_Recomp"), col=c(2,1,4), pch=1, cex=0.6, bty="n")

#Comparaison des dQ
dPsi_TSymp_Ecos =  DATA$Psi_TSym[2:(nrow(DATA))]-DATA$Psi_TSym[1:(nrow(DATA)-1)]
dQ_Tsymp_Ecos = DATA$C_TSym[1:(nrow(DATA)-1)] * dPsi_TSymp_Ecos * 6
dQ_TSymp_RV = (Q_TSymp_RV[2:(nrow(DATARV))]-Q_TSymp_RV[1:(nrow(DATARV)-1)]) *1000000/18
plot(dQ_TSymp_RV[1:24], typ="b", col=2, ylab="dQ")
points(dQ_Tsymp_Ecos[2:24], type='b', col=1)


#mmol/m2/s/MPa
FluxEcos_Ts_Ta = vegetation_parameters$k_TSymInit*(DATA$Psi_TSym-DATA$Psi_TApo)

# 
# dQ = DATA$C_TSym*(DATA$Psi_TSym-DATA$Psi_TApo)/3600
# plot(FluxEcos_Ts_Ta ~ dQ)
# abline(0,1)
# 
#plot(DATA$LAI)
# quartz()
# plot(dQ_Tsymp_Ecos[1:24] , (FluxEcos_Ta_Ts[1:24]*10.5*3600))
#Flux Apo/Symp
#Ecos
vegetation_parameters$k_TLInit
FluxEcos_Ts_Ta = vegetation_parameters$k_TSymInit*(DATA$Psi_TSym-DATA$Psi_TApo)
FluxEcos_Ta_La=vegetation_parameters$k_TLInit*(DATA$Psi_TApo-DATA$Psi_LApo)
FluxEcos_La_Ls=vegetation_parameters$k_LSymInit*(DATA$Psi_LApo-DATA$Psi_LSym)
FluxRV_Ta_Ts = vegetation_parameters$k_TSymInit*10.5 *(DATARV$P_trunk_a-DATARV$P_trunk_s)
FluxEcos_Ta_Ts = vegetation_parameters$k_TSymInit*(DATA$Psi_TApo-DATA$Psi_TSym)*10.5


plot((FluxEcos_Ta_Ts[2:24])*3600, ylim=c(-5500, 3000),type='b', ylab="Flux TApo-TSymp (mmol/s)")
points(FluxRV_Ta_Ts[1:24]*3600, typ="b", col=2, pch=16)

plot(FluxEcos_Ta_La[2:24]*10.5, type='b', ylab="Flux TApo->Leaf or branch Apo (mmol/s)")
points(DATARV_48$F_ta_ba[1:24],  col=2, pch=16, type='b')

plot(DATARV_48$E_Leaf_m2, type="b", col=2, pch=16, ylab="Estom or Emin (mmol/m2Leaf/s)")
lines(DATARV_48$E_cuti_m2, col=2)

lines(DATA1$Elim[2:48], type='b')
lines(DATA1$Emin[2:48], type='l', lty=3)




quartz()
par(mfrow=c(2,1))
plot(DATARV$P_trunk_s[1:24], ylim=c( -1,0), type='b')
points(DATARV$P_trunk_a[1:24], col=3, type='b')
plot(DATA$Psi_TSym[1:24], ylim=c( -1,0), type='b')
points(DATA$Psi_TApo[1:24], col=3, type='b')




quartz()
par(mfrow=c(3,1))
plot(Q_TSymp_RV[0:2500],col=2)
points(DATA$Q_TSym_L[0:2500], col=1)
plot(PsiTSymp_RV[0:2500],col=2)
points(PsiTSymp_Ecos[0:2500], col=1)
plot(C_TSymp_RV[0:2500], col=2)
points(C_TSymp[0:2500], col=1)



DATA$SWS1+DATA$SWS2
quartz()
plot(DATA$Elim, type='l', col=1, ylab= 'transpiration')
plot(DATA$SWS1, type='l', col=1, ylab= 'transpiration')


quartz()
plot(DATARV$P_leaf_apo, type='l', col=1, xlim=c(0,4000), ylim=c(-10,0))
lines(DATARV_P50_100$P_leaf_apo, col=2)
legend(0, -4, c("RV-Normal", "RV-P50=-100"), c(1,2))
lines(DATARV_Q20$P_leaf_apo, col=3)
lines(DATA$Psi_LApo, col=5, lwd=1)
legend(0, -4, c("Normal", "P50=-100", "Q=Q/20", "SurEau-Ecos"), c(1,2,3,5))
plot(DATARV_P50_100$PLC_leaf, col=2)


EtotRV = DATARV$E_Plant_m2*10.5/1.75*18/1000*3600/1000
EtotRV_P50_100 = DATARV_P50_100$E_Plant_m2*10.5/1.75*18/1000*3600/1000
quartz()
par(mfrow=c(2,3), pty="s")
plot(DATARV$P_leaf_apo, type='l', col=1, xlim=c(0,4000), ylim=c(-10,0))
lines(DATARV_P50_100$P_leaf_apo, col=adjustcolor(2, 0.4))
legend(0, -4, c("RV-Normal", "RV-P50=-100"), c(1,2), cex=0.7)
plot(EtotRV, type='l', col=1, xlim=c(0,4000))
lines(EtotRV_P50_100, col=adjustcolor(2, 0.4))
plot(DATARV$E_cuti_m2, type='l', col=1, xlim=c(0,4000))
lines(DATARV_P50_100$E_cuti_m2, col=adjustcolor(2, 0.4))



Etot1=DATA1$transpiration_mm+DATA1$Emin_mm
Etot2=DATA2$transpiration_mm+DATA2$Emin_mm
# quartz()
#par(mfrow=c(1,3), pty="s")
plot(DATA1$Psi_LApo, col=1, lwd=1, type='l', ylim=c(-10,0), xlim=c(0,4000))
lines(DATA2$Psi_LApo, col=adjustcolor(2, 0.4), lwd=1, type='l')
legend(0, -4, c("Ecos-Normal", "Ecos-P50=-100"), c(1,2), cex=0.7)
plot(DATA1$transpiration_mm, col=1, lwd=1, type='l', xlim=c(0,4000))
lines(DATA2$transpiration_mm, col=adjustcolor(2, 0.4), lwd=1, type='l')
plot(DATA1$Emin, col=1, lwd=1, type='l', xlim=c(0,4000))
lines(DATA2$Emin, col=adjustcolor(2, 0.4), lwd=1, type='l')

DATA1$transpiration_mm


legend(0, -4, c("Ecos-Normal", "Ecos-P50=-100"), c(1,2))


quartz()
plot(DATA$Psi_LApo, col=2)
lines(DATA$Psi_AllSoil, col=3)



RV_TRB=sum(DATARVInit$Q_branch_a+DATARVInit$Q_branch_s,
           DATARVInit$Q_trunk_a+DATARVInit$Q_trunk_s,
           DATARVInit$Q_root_a+DATARVInit$Q_root_s)

RV_TRB/1.75

TRB_RV_apo=DATARV$Q_branch_a+DATARV$Q_trunk_a+DATARV$Q_root_a
TRB_RV_sym=DATARV$Q_branch_s+DATARV$Q_trunk_s+DATARV$Q_root_s


ECOSTRB=(DATA$Q_TApo_L+DATA$Q_TSym_L)
RV_TRB=DATARV$Q_branch_a+DATARV$Q_branch_s+DATARV$Q_trunk_a+DATARV$Q_trunk_s+DATARV$Q_root_a+DATARV$Q_root_s
RV_TRB_perSoilArea = RV_TRB/1.75

quartz()
par(mfrow=c(2,3))
plot(RV_TRB_perSoilArea, type='l', ylab="Volume TRB_L", col=2, ylim=c(0,25),xlim=c(0, 4000))
lines(ECOSTRB)
legend(0,18,c("Ecos", "RV"), col=c(1,2), lty=1, bty="n")

plot(TRB_RV_apo/1.75, type='l',ylim=c(0,17), col=2, ylab="total apo (L)",xlim=c(0, 4000))
lines(DATA$Q_TApo_L)

plot(TRB_RV_sym/1.75, type='l',ylim=c(0,10), col=2, ylab='total symp (L)',xlim=c(0, 4000))
lines(DATA$Q_TSym_L)


plot(DATARV$P_trunk_s, type='l', ylab="P trunk symp (MPa)", col=2,xlim=c(0, 4000), ylim=c(-5, 0))
lines(DATA$Psi_TSym, type='l',yaxt="n")

plot(DATARV$P_trunk_a, type='l', ylab="P trunk apo", col=2,xlim=c(0, 4000), ylim=c(-5, 0))
lines(DATA$Psi_TApo, type='l',yaxt="n")
par(new=T)
plot(DATARV$PLC_trunk, type='l',yaxt="n", ylab="", col=2,xlim=c(0, 4000), ylim=c(0,100))
lines(DATA$PLC_Trunk, type='l',yaxt="n")
axis(4)






quartz()
par(mfrow=c(2,1))
plot(DATA$Psi_LSym, type='l', ylab="P branc symp", ylim=c(-10,0))
lines(DATA$Psi_LApo, type='l', col=2, lty=2)
plot(DATA$Psi_TSym, type='l', ylab="P trunk symp")
lines(DATA$Psi_TApo, type='l', ylab="", col=2, lty=2)

quartz()
par(mfrow=c(2,1))
plot(DATA$Psi_LSym, type='l', ylab="P branc symp", ylim=c(-10,0))
lines(DATARV$P_leaf_sym, type='l', ylab="P leaf symp", col=2)


quartz()
par(mfrow=c(2,2))
plot(DATARV$P_branch_s, type='l', ylab="P branc symp")
lines(DATARV$P_branch_a, type='l', col=2, lty=2)
plot(DATARV$P_trunk_s, type='l', ylab="P trunk symp")
lines(DATARV$P_trunk_a, type='l', ylab="", col=2, lty=2)
plot(DATARV$P_root1_sy, type='l', ylab="P root symp")
lines(DATARV$P_root1_ap, type='l', ylab="", col=2, lty=2)
plot(DATARV$P_leaf_sym, type='l', ylab="P leaf symp")
lines(DATARV$P_leaf_apo, type='l', ylab="P leaf symp", col=2, lty=2)

DATARV$Q_root_a[1]/1.75
DATARV$Q_root_s[1]/1.75
DATARV$Q_trunk_a[1]/1.75
DATARV$Q_trunk_s[1]/1.75
DATARV$Q_leaf_s[1]/1.75


plot(DATA$Q_TSym_L~DATA$Psi_TSym, ylim=c(0,9))
points(TRB_RV_sym/1.75~DATARV$P_trunk_s, col=2)

plot(DATA$Q_TApo_L~DATA$Psi_TApo, type='l')
lines(TRB_RV_apo/1.75~DATARV$P_trunk_a, col=2)
legend(-4, 16, c("RV", "Ecos"), col=c(2,1), lty=1)

plot(DATA$Q_TApo_L~DATA$PLC_Trunk)
points(TRB_RV_apo/1.75~DATARV$PLC_trunk, col=2)



points(DATARV$Q_branch_s/1.75 ~ DATARV$P_branch_s, col=3)
points(DATARV$Q_trunk_s/1.75 ~ DATARV$P_trunk_s, col=4)
points(DATARV$Q_root_s/1.75 ~ DATARV$P_root1_sy, col=5)



vegetation_parameters$DMLiveCanopy = vegetation_parameters$LAImax * vegetation_parameters$LMA
Q_L_sat_L <- (1 / (vegetation_parameters$LDMC / 1000) - 1) * vegetation_parameters$DMLiveCanopy / 1000 # Leaf symplastic water content in l/m2 (i.e. mm)
Q_L_sat_L/vegetation_parameters$LAImax*10.5

ttt=cbind.data.frame(DATARV$Q_leaf_a , DATARV$Q_leaf_s, DATARV$Q_trunk_a , DATARV$Q_trunk_s , DATARV$Q_branch_a , DATARV$Q_branch_s , DATARV$Q_root_a , DATARV$Q_root_s)
Qleaf_RV= DATARV$Q_leaf_a + DATARV$Q_leaf_s
QleafSymp =DATA$Q_LSym_L/vegetation_parameters$LAImax * 10.5
QleafApo =DATA$Q_LApo_L/vegetation_parameters$LAImax * 10.5
Qleaf = QleafSymp + QleafApo

lengthOut=2821
quartz()
par(mfrow=c(2,1))
plot(DATARV$P_leaf_sym[1:lengthOut], type="l", ylab="Psi of leaf symplasm", col=2, lwd=2)
points(DATA$Psi_LSym[2:lengthOut],type='l', lwd=2, col=adjustcolor(1, alpha.f=0.4))
par(new=T)
plot(DATARV$PLC_leaf [1:lengthOut], type="l", col=2, lwd=2, yaxt="n", ylab="")
points(DATA$PLC_Leaf[2:lengthOut],type='l', lwd=2, col=adjustcolor(1, alpha.f=0.4))
axis(4)
mtext("PLC Leaf",4,1.7, cex=0.8)
legend(0,40,c("SurEau", "SurEau-Ecos"), col=c(2, adjustcolor(1, alpha.f=0.4)), lwd=2, cex=0.7, bty="n")

plot(Qleaf_RV[1:lengthOut], type="l", col=2, lwd=2, ylab="",xlab="Time", ylim=c(0,1.1))
mtext("Leaf water content (l/m2-leaf)", 2, 1.8, cex=0.8)
lines(Qleaf[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=2)

lines(DATARV$Q_leaf_a[1:lengthOut], type="l", lty=3, col=2, lwd=1, ylab="")
lines(DATARV$Q_leaf_s[1:lengthOut], type="l", lty=2, col=2, lwd=1, ylab="")

lines(QleafSymp[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=1, lty=2)
lines(QleafApo[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=1, lty=3)
legend(2000,1,c("total leaf Q", "Symplasm Leaf Q", "Apoplasm Leaf Q"), lty=c(1,2,3), cex=0.7, bty="n")


plot(Qleaf_RV[1:lengthOut], type="l", col=2, lwd=2, ylab="",xlab="Time", ylim=c(0,1.1))
mtext("Leaf water content (l/m2-leaf)", 2, 1.8, cex=0.8)
lines(Qleaf[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=2)

lines(DATARV$Q_leaf_a[1:lengthOut], type="l", lty=3, col=2, lwd=1, ylab="")
lines(DATARV$Q_leaf_s[1:lengthOut], type="l", lty=2, col=2, lwd=1, ylab="")

lines(QleafSymp[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=1, lty=2)
lines(QleafApo[2:lengthOut], type="l", col=adjustcolor(1, alpha.f=0.6), lwd=1, lty=3)
legend(2000,1,c("total leaf Q", "Symplasm Leaf Q", "Apoplasm Leaf Q"), lty=c(1,2,3), cex=0.7, bty="n")





FluxTot= DATA$fluxSoilToCollar1_mm+DATA$fluxSoilToCollar2_mm+DATA$fluxSoilToCollar3_mm
Fluxmol=DATA$Elim+DATA$Emin
plot(FluxTot, type='l')
lines(DATA$transpiration_mm, col=2)
plot(FluxTot~Fluxmol)

quartz()
par(mfrow=c(2,2))
plot(DATA$Q_LSym_L, type='l')
lines(DATARV$Q_leaf_s, type='l', col=2)
plot(DATA$Q_LApo_L, type='l')
plot(DATA$Q_TSym_L, type='l')
plot(DATA$Q_TApo_L, type='l')





QtotRV = DATARV$Q_leaf_a + DATARV$Q_leaf_s + DATARV$Q_trunk_a + DATARV$Q_trunk_s + DATARV$Q_branch_a + DATARV$Q_branch_s + DATARV$Q_root_a + DATARV$Q_root_s
Qtot =(DATA$Q_TSym_L+DATA$Q_TApo_L + DATA$Q_LSym_L+DATA$Q_LApo_L)*10.5 #stand_parameters$LAImax
plot(QtotRV, type='l', ylab="Q plant tot (kg)", ylim=c(0,50))
lines(Qtot, type='l', col=adjustcolor("firebrick1", alpha.f=0.6))


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


Qtot = DATA$Q_LApo_L+DATA$Q_LSym_L+DATA$Q_TApo_L+DATA$Q_TSym_L
plot(DATARV$Q_leaf_a[1:lengthOut], type="l", ylab="", col=2, lwd=2)
plot((DATA$Q_LApo_L*1000/18)[2:lengthOut],type='l', lwd=2, col=adjustcolor(1, alpha.f=0.6))






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
plot(DATA$Tair[2:25], type='l', ylim=c(15,35), ylab="air temperature", col="dark blue", lwd=2, xlab="Hours")
par(new=T)
plot(DATA$PAR[2:25], type='l', yaxt="n", xlab="",ylab="", col="orange", lwd=2)
axis(4)
plot(DATA$leafTemperature[2:25], type='l', ylim=c(15,35), ylab="Leaf temperature", xlab="Hours")
lines(DATARV$T_leaf[1:24], type='l', ylim=c(15,35), col =2, lty=2)
legend(1, 36,  c("SurEau.Ecos", "SurEau.C"), col=c(1,2), lty=1, bty="n")
plot(DATA$leafVPD[2:25], type='l', ylab="Leaf VPD", xlab="Hours")
lines(DATARV$VPD_Leaf[1:24], type='l', col =2, lty=2)

plot(DATA$Elim[2:25]-DATA$Emin[2:25],type='l', ylab="E_stom", xlab="Hours", ylim=c(0,1.3))
lines(DATARV$E_stomata[1:24]-DATARV$E_cuti_m2[1:48], col=adjustcolor('firebrick1', alpha.f=0.6), lty=2)
plot(DATA$Emin[2:25], type="l", ylab="E_cuti", xlab="Hours", ylim=c(0,0.15))
lines(DATARV$E_cuti_m2[1:24],type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lty=2)
plot(DATA$Psi_LSym[2:25], type="l", xlab="Hours", ylab="P_l_sym", ylim=c(-1.9,0) )
lines(DATARV$P_leaf_sym[1:24], type='l', col =2, lty=2)
#lines(DATARV$P_evap_apo [1:24], type='l', col =2, lty=2)


plot(DATA$Elim-DATA$Emin,type='l', ylab="E_stom", xlab="Hours", ylim=c(0,1.3))
lines(DATARV$E_stomata-DATARV$E_cuti_m2, col=adjustcolor('firebrick1', alpha.f=0.6), lty=2)
plot(DATA$Elim,type='l', ylab="E_stom", xlab="Hours", ylim=c(0,1.3))
lines(DATARV$E_stomata, col=adjustcolor('firebrick1', alpha.f=0.6), lty=2)
plot(DATA$Emin, type="l", ylab="E_cuti", xlab="Hours", ylim=c(0,0.3))
lines(DATARV$E_cuti_m2,type='l',col=adjustcolor('firebrick1', alpha.f=0.6), lty=2)
plot(DATA$Psi_LSym, type="l", xlab="Hours", ylab="P_l_sym", ylim=c(-3.9,0) )
lines(DATARV$P_leaf_sym, type='l', col =2, lty=2)

#lines(DATARV$P_evap_apo [1:24], type='l', col =2, lty=2)


quartz()
par(mfrow=c(2,3))
plot(DATA$gs_lim[2:49], type="l", ylab="gs")
lines(DATARV$g_s [1:48],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
plot(DATA$gcanopy_lim [2:49], type="l", ylab="gs")
lines(DATARV$g_canopy [1:48],type='l',col=adjustcolor('firebrick1', alpha.f=0.6))
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

plot(DATA$gmin,type='l')
lines(DATARV$g_cuti,col=2)

plot(DATA$leafVPD[2:25], type='l', ylab="VPD_Leaf")
lines(DATARV$VPD_Leaf[1:24], type='l', col =2)

plot(DATA$leafTemperature[2:25], type='l', ylim=c(15,35), ylab="Leaf temp")
lines(DATARV$T_leaf[1:24], type='l', ylim=c(15,35), col =2)

plot(DATA$Emin,type='l')
lines(DATARV$E_cuti_m2,col=2)
>>>>>>> 08c3efe5c6c2cbb942d927b024362df4d9c3c6ce


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

   