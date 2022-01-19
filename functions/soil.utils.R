# Several plant functions used for SurEau-Ecos
# Added NM 11/12/2021

compute.b <- function(Lv){
  #calculate b used to compte thhe B of the gardnar cowen model 
  #From Lv the length of fine root per unit volume
  
  return(1 / sqrt(pi * Lv))
  }

compute.La <- function(Lv)
  {
  #calculate La thhe length of fine root per unit soil area
  
}

compute.B_GC <- function (La, b, rootRadius) {
  #calculate B Gardner cowen thhe scaling factor for soil conductance
  return(La * 2 * pi / (log(b/rootRadius)))
  }


compute.KSoil <- function(REW, I_vg, n_vg, Ksat_vg, B_GC) {
  
  m <- (1 - 1 / n_vg)
  
  KSoil = Ksat_vg* REW^(I_vg) * (1 - (1 - REW^(1 / m))^m)^2
  
  kSoil_GC = 1000  * B_GC * KSoil
  return(c(KSoil, kSoil_GC))
  
  }

compute.thetaAtGivenPSoil <- function(PsiTarget, thetaRes, thetaSat, alpha_vg, n_vg)
  {
  
  thetaAtGivenPSoil = thetaRes+(thetaSat-thetaRes)/(1+(alpha_vg*PsiTarget*10000)^n_vg)^(1-1/n_vg)
  
  return(thetaAtGivenPSoil)
  
}


compute.PSoil <- function(REW, alpha_vg, n_vg) {
  m <- (1 - 1 / n_vg)
  Psoil = (-1 * ((((1 / REW)^(1 / m)) - 1)^(1 / n_vg)) / alpha_vg / 10000)
  # diviser par 10000 pour passer de cm à MPa
  return(Psoil)
  
}  

#--------------------
#Campbell
compute.PSoil.Camp <- function(SWS, tsc, b_camp, psie) {
  Psoil <- -1 * (psie * ((SWS / tsc)^-b_camp))
  return(Psoil)
}


compute.KSoil.Camp <- function(SWS, tsc, b_camp, Ksat_campbell) {
  ksoil <- Ksat_campbell*(SWS / tsc)^(-b_camp * 2 + 2)
  return(ksoil)
}

#compute.KSoil.Camp(0.1,0.5, -4, 1.69)

compute.thetaAtGivenPSoil.Camp <- function(thetaSat, PsiTarget, psie, b_camp) {
  
  thetaAtGivenPSoil =  thetaSat*(PsiTarget/-psie)^(1/-b_camp)
  
  return(thetaAtGivenPSoil)
}

