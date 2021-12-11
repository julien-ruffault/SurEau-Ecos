

PlotTheStandAndPlant <- function(vegetation_parameters, soil_parameters, openWindow=F){
  #Plot basic parameters of the plant and the stand : to see what you model ...
  
  print("Look at your Stand & Soil & Plant")
  
  slope_gs = 100/(vegetation_parameters$P12_gs-vegetation_parameters$P88_gs)
  P50_gs= (vegetation_parameters$P12_gs + vegetation_parameters$P88_gs)/2 
  
  if(openWindow==T){
  quartz()}
  
  par(mfrow=c(2,2), pty='s')
  DepthLayers=soil_parameters$depth*100
  depthvec = seq(0, 4, 0.1)
  fractionofRoot = 1-vegetation_parameters$betaRootProfile^(depthvec*100)
  plot(fractionofRoot, depthvec*100, type='l', ylim=c(400,0), xlim=c(1,0), ylab="depth (cm)", xlab="root fraction")
  abline(h=DepthLayers)
  barplot(vegetation_parameters$rootDistribution, ylab='root fraction per layer')
  barplot(soil_parameters$V_field_capacity, ylab="Vol. field capacity")
  barplot(vegetation_parameters$k_RTInit, ylab="soil-root conductance per layer")
  
  if(openWindow==T){
    quartz()}
  
  par(mfrow=c(2,2), pty='s')
  curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Leaf,P50=vegetation_parameters$P50_VC_Leaf),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", xlab="Psi")
  curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Trunk,P50=vegetation_parameters$P50_VC_Trunk),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", lty=3, add=T, col=2, lwd=2)
  par(new=T)
  curve(GsCurve(x, slope=slope_gs,P50=P50_gs, gsmax=vegetation_parameters$gsMax),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , col=4, yaxt="n", ylab="",xlab="")
  axis(4)
  mtext("gs",4,1.8,cex=.8)
  barplot(c(vegetation_parameters$kPlantInit ,vegetation_parameters$LAImax,vegetation_parameters$gmin20), names.arg = c("Kp", "LAI", "gmin"))
  barplot(soil_parameters$V_soil_storage_capacity, names.arg="RU")
  barplot(c(vegetation_parameters$P12_gs ,vegetation_parameters$P88_gs,vegetation_parameters$P50_VC_Leaf), names.arg = c("P12g", "P88g", "P50x"))
}

