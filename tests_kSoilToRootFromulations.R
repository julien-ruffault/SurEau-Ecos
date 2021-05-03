


# function de kSoilToRoot in kennedy, 
# 
# 
# 
 
test.RAI <- function(f_root_shoot, LAI, SAI)
{
  RAI  = f_root_shoot*LAI*SAI
}

 
test.RAIi <- function(RAI,ri){
  RAIi= RAI*ri}


test.ri <-  function(beta_i)


test.kSoilToRoot <- function(KSoilSat,SWC, SWS_sat,b, RAIi, Di){

  kSoilToRoot  = KSoilSat*((SWC/SWSsat)^(2*b+3))*(sqrt(RAIi)/(pi*Di))
  }
  

  
  
  
  
  
  
  