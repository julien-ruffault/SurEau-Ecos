### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors  : Julien Ruffault (julien.ruff@gmail.com)
#                             &
#           Nicolas Martin-StPaul (nicolas.martin@inrae.fr)
#                             &
#           Francois Pimont (francois.pimont@inrae.fr)          
# Date    : V0 (17/06/2020, JR)
#           V1 (21/12/2020, JR) // general cleaning //  update code for the choice of the temporal resolution 
#           V2 (20/01/2021, JR) // V2.1 (running version)  
#           V3 (16/04/2021, JR) // update outoput managmement 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Main function for Sureau_Ecos
# INPUTS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#   - climate_data   :  data.frame avec  ppt(mm), temp(°C) et rg(?), wind (m/s),...  n colonne,
#   - soil_params    :  liste avec les parametres de sol
#   - veg_params     :  liste avec les parametres de la vegetation
#   - General_Params :  list avec les parametrs generaux pour la modélisation
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##


### NOTES ####
#   differencier les general params des stands parameters  (lat,lon)
WB_MAIN <- function(climate_data, general_params, soil_params, veg_params) {
  
  soil_var_list <- new_WBsoil(soil_params, general_params) #  Create soil layers from input parameters
  veg_var_list  <- new_WBveg(veg_params)                   #  Create Vegetation layer from  vegetation parameters
  
  modelOutput   <- New_WBoutput(general_params)            #  initialize output (writing files andvariables)

  # @run@ #### YEAR=1990
  for (YEAR in (general_params$start_year:general_params$end_year)) { # loop on year
    
    print(paste0("year=",YEAR))
    
    if (general_params$reset_SWC==T) { # See how to integrate this option in the code (22/12/2020, JR)
      soil_var_list$SoilWaterStock=soil_var_list$params$V_field_capacity
      soil_var_list <- computeSoilConductanceAndPsi.WBsoil(soil_var_list)
      }
    
    for (DAY in 1:max(yday(general_params$time_mod[year(general_params$time_mod) == YEAR]))) # Loop on days ####
    {
      print(paste0("day=",DAY))
      # Processes at the daily time step : pheno, rain, interception and soil infiltration
      climDay         <- new_WBclim(climate_data, YEAR = YEAR, DOY = DAY,general_params = general_params)          # Create WBclim for the day 
      veg_var_list    <- computePheno.WBveg(.WBveg = veg_var_list, temperature = climDay$Tair_mean, DOY = DAY)   # LAI and update 
      
      climDay         <- computeRnAndETP.WBclim(WBclim = climDay, WBveg  = veg_var_list, Rn.formulation = general_params$Rn.formulation, ETP.formulation = general_params$ETP.formulation)# calculate Rn and ETP
      veg_var_list    <- computeInterception.WBveg(.WBveg = veg_var_list, ppt = climDay$PPT) # vegetation QR and ppt soil with Interpception by the canopy
      soil_var_list   <- computeInfiltration.WBsoil(WBsoil = soil_var_list, pptSoil = veg_var_list$pptsoil)       # Infiltration / update soil water stocks / PsiSoil and KSoil
      
      climHour        <- new_WBclimHour(WBclim=climDay, WBveg = veg_var_list, TIME=general_params$TIME, general_params=general_params, lat = general_params$lat, lon = general_params$lon,PTcoeff = veg_var_list$params$PTcoeff)
      
      #FP IMPORTANT Kplant should be computed once all initialization of veg or soil are done
      veg_var_list <- updateKplant.WBveg(veg_var_list,soil_var_list) # compute conductance soil+plant  
      
      veg_var_list
      
      
      for (tt in 1:length(climHour$ETP)) # 
        {
          #set climate 
          Clim_current = lapply(climHour,function(x) x[max(tt-1,1)]) # select climate of the h
          Clim_next = lapply(climHour,function(x) x[tt]) # select climate of the next time step
          Clim_mid = interpClim(Clim_current,Clim_next,0.5) # note : Clim_mid is not a list of "type" WBclimHour
          
          
          veg_var_list  <- computeEvapIntercepted.WBveg(veg_var_list, ETP = Clim_mid$ETP)
      
    
          if(veg_var_list$PLCAbove < 100 & veg_var_list$PLCBelow <100){
          # Compute plant hydraulic to the next time step
          veg_var_list <- computePlantNextTimeStep.WBveg(WBveg = veg_var_list,WBsoil = soil_var_list,Nhours = Clim_next$nHours,WBclim_current = Clim_current,WBclim_next = Clim_next)
          # compute water storage in vegetation
          veg_var_list <- computeWaterStorage.WBveg(veg_var_list,VPD=Clim_next$VPD)
          # update soil water stocks after T and water release and update psi and conductance
          # TODO with assign the values of the small time step could be updated exactly so that these final updates of soil could be removed
          # In theory the updatSoilWater is already consistent between the small time step and the large time step but it is not the case for soil evaporation
          soil_var_list <- computeEvaporation_withG.WBsoil(WBsoil = soil_var_list,ETP = veg_var_list$ETPr,RHair = Clim_mid$RHair,K = veg_var_list$params$K,LAI = veg_var_list$LAI,Nhours = Clim_next$nHours)
          soil_var_list <- UpdateSoilWater.WBsoil(WBsoil = soil_var_list, fluxEvap = veg_var_list$fluxSoilToCollar.C, fluxRelease  = 0) # veg_var_list$waterRelease)
          
          } else (stop('The plant is dead..'))
          # write outputs (NOTE : integrate x.m and TIME in WBclimHour directluy, would be cleaner (JR : 22/12/2020)
          if(general_params$resolutionOutput  == 'subdaily')
          {Write.WBoutput(Date = climDay$Date, WBoutput = modelOutput, WBsoil = soil_var_list, WBveg = veg_var_list,WBclim=Clim_next)}
          
        } # end loop on hours
      if (general_params$resolutionOutput == 'daily')
      {Write.WBoutput(Date = climDay$Date, WBoutput = modelOutput, WBsoil = soil_var_list, WBveg = veg_var_list,WBclim=Clim_next)}
    } # end loop on days
    if (general_params$resolutionOutput  == 'yearly')
    {Write.WBoutput(Date = climDay$Date, WBoutput = modelOutput, WBsoil = soil_var_list, WBveg = veg_var_list,WBclim=Clim_next)}
  } # end loop on years
}
