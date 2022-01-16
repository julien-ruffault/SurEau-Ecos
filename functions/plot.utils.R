

PlotTheStandAndPlant <- function(vegetation_parameters, soil_parameters, modeling_options, openWindow=F){
  #Plot basic parameters of the plant and the stand : to see what you model ...
  
  print("Look at your Stand & Soil & Plant")
  
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
  barplot(vegetation_parameters$k_RSApoInit, ylab="soil-root conductance per layer")
  
  if(openWindow==T){
    quartz()}
  
  par(mfrow=c(2,2), pty='s')
  curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Leaf,P50=vegetation_parameters$P50_VC_Leaf),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", xlab="Psi")
  curve(VCCurve(x,slope=vegetation_parameters$slope_VC_Stem,P50=vegetation_parameters$P50_VC_Stem),from=0, to=vegetation_parameters$P50_VC_Leaf-3 , ylab="PLC", lty=3, add=T, col=2, lwd=2)
  par(new=T)
  
  curve(GsCurve(
    x, 
    slope=vegetation_parameters$slope_gs,
    P50= vegetation_parameters$P50_gs, 
    PsiStartClosing = vegetation_parameters$PsiStartClosing,
    PsiClose = vegetation_parameters$PsiClose, 
    PiFT = vegetation_parameters$PiFullTurgor_Leaf, 
    Esymp = vegetation_parameters$epsilonSym_Leaf, 
    turgorPressureAtGsMax = vegetation_parameters$turgorPressureAtGsMax, 
    gsmax = vegetation_parameters$gsMax, 
    stomatalRegFormulation = modeling_options$stomatalRegFormulation), 
    from=0, to= vegetation_parameters$P50_VC_Leaf-3 , col=4, yaxt="n", ylab="",xlab="")
  
    axis(4,  col = 4)
    mtext("gs",4,1.8,cex=.8, col=4)
  
  barplot(c(vegetation_parameters$k_PlantInit ,vegetation_parameters$LAImax,vegetation_parameters$gmin20), names.arg = c("Kp", "LAI", "gmin"), cex.lab=0.8)
  if(modeling_options$stomatalRegFormulation=="Sigmoid"){
  barplot(c(vegetation_parameters$P50_gs,vegetation_parameters$P50_VC_Leaf), names.arg = c("P50gs", "P50x"), cex.lab=0.8)}
  if(modeling_options$stomatalRegFormulation=="PiecewiseLinear"){
    barplot(c(vegetation_parameters$PsiClose,vegetation_parameters$P50_VC_Leaf), names.arg = c("PClose", "P50x"), cex.lab=0.8)}
  if(modeling_options$stomatalRegFormulation=="Turgor"){
    tlp = (vegetation_parameters$PiFullTurgor_Leaf *vegetation_parameters$epsilonSym_Leaf)/(vegetation_parameters$PiFullTurgor_Leaf + vegetation_parameters$epsilonSym_Leaf)
    barplot(c(vegetation_parameters$PiFullTurgor_Leaf ,tlp, vegetation_parameters$P50_VC_Leaf,vegetation_parameters$P50_VC_Stem), names.arg = c("Pi0","TLP", "P50L","P50S" ), col = c(4,3,2,1), cex.lab = 0.5)}
  
  
#  barplot(soil_parameters$V_soil_storage_capacity, names.arg="TAW", density=10, ylim=c(0, soil_parameters$V_soil_storage_capacity+50))
  TAW = as.data.frame(matrix(ncol =4, nrow=1))
  
  VECTAW = c(soil_parameters$V_soil_storage_capacity, 
             vegetation_parameters$TAW_AtTLP-soil_parameters$V_soil_storage_capacity,
             vegetation_parameters$TAW_AtP50-vegetation_parameters$TAW_AtTLP)
  
  TAW[,c(1:3)] =VECTAW
  TAW[,4] = "TAW"
  a= barplot(cbind(V1,V2,V3)~V4, data=TAW, density =c(20,40, 60), col=c(4,3,2) ,ylim=c(0,(vegetation_parameters$TAW_AtP50+40)), names.arg="Total available water (TAW)", ylab="")
  text(a,cumsum(VECTAW)[1]-10,"TAW@pF4.2" )  
  text(a,cumsum(VECTAW)[2]-10,"TAW@TLP" )
  text(a,cumsum(VECTAW)[3]-10,"TAW@P50" )
  
  
}
  

