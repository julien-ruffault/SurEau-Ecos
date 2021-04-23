
calculate_gs_Jarvis <- function(PAR,leafTemperature, option=1,gsMax =200, Ca=400,gsNight=20, JarvisPAR = 0.006,Tgs_sens=17, Tgs_optim=20)
{
  
  
  if (option==1) # temeperature effect on gs 
  {
    gsMax2    = max(0,gsMax/(1+((leafTemperature-Tgs_optim)/Tgs_sens)^2))
    gsNight2  = max(0,gsNight/(1+((leafTemperature-Tgs_optim)/Tgs_sens)^2))
  }
  
  
  #if (option==7){
  # gsMax2 = (gsMax*(1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((leafTemperature-Tgs_optim)/Tgs_sens,2))  # 
  # gsNight2 = (gs_night* (1 + gs_CO2_sens/100*(Ca-300)/100))/(1+pow((T_Leaf-Tgs_optim)/Tgs_sens,2));
  #}
  
  gs_Jarvis=   gsNight2 + (gsMax2-gsNight2)*(1-exp(-JarvisPAR*PAR))
  
  return(gs_Jarvis)
}

calculate_ET_Jarvis <- function(VPD, PAR, windSpeed, gsMax =100, gsNight=20, JarvisPAR = 0.006, gCrown0=45,gBL)
{
  if(windSpeed<0.1){windSeed <- 0.1}
  gs_Jarvis=   gsNight + (gsMax-gsNight)*(1-exp(-JarvisPAR*PAR))
  g_crown = gCrown0*windSpeed^0.6
  gcanopy= 1/((1/g_crown)+(1/gs_Jarvis) + (1/gBL))
  Ebound = gcanopy * VPD / 101.3
  return(Ebound)
}

# VPD=WBclim$VPD
# PAR=WBclim$PAR
# windSpeed=WBclim$WS
# gBL
# PsiStartClosing=WBveg$params$PsiStartClosing
# PsiClose=WBveg$params$PsiClose
# Psi=PsiCanopy0
# gmin=    WBveg$gmin
# 
# gs_Jarvis=calculate_gs_Jarvis(VPD, PAR, gsMax =100, gsNight=20, JarvisPAR = 0.006)

calculate_gs_Jarvis <- function(VPD, PAR, windSpeed, gsMax =100, gsNight=20, JarvisPAR = 0.006, gCrown0=45){
  gs_Jarvis =   gsNight + (gsMax-gsNight)*(1-exp(-JarvisPAR*PAR))
  return(gs_Jarvis)
}

# 
# calculate_ET_Jarvis(VPD,PAR,windSpeed, gmin, gs=gs_Jarvis, gCrown0=45, gBL, PsiStartClosing, PsiClose, Psi)
# 
calculate_ET_Gs <- function(VPD, PAR, windSpeed, gmin, gs=gs_Jarvis, gCrown0=45, gBL, PsiStartClosing, PsiClose, Psi)
{
 
  # Il faudrait remplacer :  PsiStartClosing, PsiClose & Psi Par le type de régulation de Psi
  
  if(windSpeed<0.1) {windSeed <- 0.1}
  
  gs_Jarvis_Bound = gs_Jarvis
  gs_Jarvis_lim = gs_Jarvis * (Psi - PsiClose)/(PsiStartClosing - PsiClose)
  g_crown = gCrown0*windSpeed^0.6
  
  gcanopy_bound = 1/((1/g_crown)+(1/gs_Jarvis_Bound) + (1/gBL))
  gcanopy_lim = 1/((1/g_crown)+(1/gs_Jarvis_lim) + (1/gBL))
  
  if(Psi > PsiStartClosing){
    Eprime =  0
    E0 =   gmin*VPD/101.3  + gcanopy_bound*VPD/101.3
    
  } else if(Psi < PsiClose)
    {
    Eprime =  0
    E0 =   gmin*VPD/101.3 
    
  } else if(Psi > PsiClose & PsiCanopy0<WBveg$params$PsiStartClosing)
    {
    
    Eprime =  gcanopy_bound*VPD/101.3/(WBveg$params$PsiStartClosing - WBveg$params$PsiClose)
    
    E0 =  gmin*VPD/101.3 + gcanopy_lim*VPD/101.3
    
  }  
  
  return(c(E0, Eprime, gs_Jarvis_lim, gcanopy_lim, g_crown, gcanopy_bound))
}





