### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Authors : <Julien Ruffault (julien.ruff@gmail.com)>
#           <Nicolas Martin-StPaul (nicolas.martin@inrae.fr)>
#           <Francois Pimont (francois.pimont@inrae.fr)>
# Date    : V0 (17/06/2020, JR)
#           V1 (21/12/2020, JR) // general cleaning //  update code for the choice of the temporal resolution
#           V2 (20/01/2021, JR) // V2.1 (running version)
#           V3 (16/04/2021, JR) // update outoput managmement
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# Main function for Sureau_Ecos
# INPUTS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#   - simulation_parameters :  simulation parameters for Sureau-Ecos
#   - modeling_options      :  modeling options for SurEau-Ecos
#   - climate_data          :  climate data created by
#   - stand_parameters      :  stand parameters for SurEau-Ecos
#   - soil_parameters       :  soil parameters for SurEau-Ecos
#   - vegetation_parameters :  vegetation parameters for SurEau-Ecos
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

run.SurEauR <- function(modeling_options, simulation_parameters, climate_data,soil_parameters, vegetation_parameters, stand_parameters) { # start loop on years
  
  soil_var_list <- new.WBsoil(soil_parameters) #  create soil from input parameters
  veg_var_list  <- new.WBveg(vegetation_parameters) #  create vegetation from vegetation parameters
  model_output  <- new.WBoutput(simulation_parameters) #  create output file and parameters


  # @run@ ####
  for (YEAR in (simulation_parameters$startYearSimulation : simulation_parameters$endYearSimulation)) { # start loop on year

    print(paste0("year=", YEAR))

    if (modeling_options$resetSWC == T) {
      soil_var_list <- set.SWCtoFieldCapacity.WBsoil(soil_var_list)
    }

    for (DAY in yday(simulation_parameters$timeDateSimulation[year(simulation_parameters$timeDateSimulation) == YEAR])) # Loop on days ####
    {
      print(paste0("day=", DAY))
      
      climDay <- new.WBclim(climate_data =climate_data, YEAR = YEAR, DOY = DAY) # Create WBclim for the day
      veg_var_list <- compute.pheno.WBveg(WBveg = veg_var_list, temperature = climDay$Tair_mean, DOY = DAY) # LAI and update
      climDay <- compute.RnAndETP.WBclim(WBclim = climDay, WBveg = veg_var_list, RnFormulation = modeling_options$RnFormulation, ETPFormulation = modeling_options$ETPFormulation) # calculate Rn and ETP
      veg_var_list <- compute.interception.WBveg(WBveg = veg_var_list, ppt = climDay$PPT) # vegetation QR and ppt soil with Interpception by the canopy
      soil_var_list <- compute.infiltration.WBsoil(WBsoil = soil_var_list, pptSoil = veg_var_list$pptsoil) # Infiltration / update soil water stocks / PsiSoil and KSoil

      climHour <- new.WBclimHour(WBclim = climDay, WBveg = veg_var_list, modeling_options= modeling_options, lat = general_params$lat, lon = general_params$lon, PTcoeff = veg_var_list$params$PTcoeff)

      # FP IMPORTANT Kplant should be computed once all initialization of veg or soil are done
      veg_var_list <- update.kplant.WBveg(veg_var_list, soil_var_list) # compute conductance soil+plant

      for (tt in 1:length(climHour$ETP)) #
      {
        # set climate
        Clim_current <- lapply(climHour, function(x) x[max(tt - 1, 1)]) # select climate of the h
        Clim_next <- lapply(climHour, function(x) x[tt]) # select climate of the next time step
        Clim_mid <- interp.WBclim(Clim_current, Clim_next, 0.5) # note : Clim_mid is not a list of "type" WBclimHour


        veg_var_list <- compute.evapoIntercepted.WBveg(veg_var_list, ETP = Clim_mid$ETP)


        if (veg_var_list$PLCAbove < 100 & veg_var_list$PLCBelow < 100) {
          # Compute plant hydraulic to the next time step
          veg_var_list <- compute.plantNextTimeStep.WBveg(WBveg = veg_var_list, WBsoil = soil_var_list, Nhours = Clim_next$nHours, WBclim_current = Clim_current, WBclim_next = Clim_next)
          # compute water storage in vegetation
          veg_var_list <- compute.waterStorage.WBveg(veg_var_list, VPD = Clim_next$VPD)
          # update soil water stocks after T and water release and update psi and conductance
          # TODO with assign the values of the small time step could be updated exactly so that these final updates of soil could be removed
          # In theory the updatSoilWater is already consistent between the small time step and the large time step but it is not the case for soil evaporation
          soil_var_list <- compute.evaporationG.WBsoil(WBsoil = soil_var_list, ETP = veg_var_list$ETPr, RHair = Clim_mid$RHair, K = veg_var_list$params$K, LAI = veg_var_list$LAI, Nhours = Clim_next$nHours)
          soil_var_list <- update.soilWater.WBsoil(WBsoil = soil_var_list, fluxEvap = veg_var_list$fluxSoilToCollar.C, fluxRelease = 0) # veg_var_list$waterRelease)
        } else {
          (stop("The plant is dead.."))
        }
        # write outputs (NOTE : integrate x.m and TIME in WBclimHour directluy, would be cleaner (JR : 22/12/2020)
        if (simulation_parameters$resolutionOutput == "subdaily") {
          write.WBoutput(Date = climDay$Date, WBoutput = model_output, WBsoil = soil_var_list, WBveg = veg_var_list, WBclim = Clim_next)
        }
      } # end loop on hours
      if (simulation_parameters$resolutionOutput == "daily") {
        write.WBoutput(Date = climDay$Date, WBoutput = model_output, WBsoil = soil_var_list, WBveg = veg_var_list, WBclim = Clim_next)
      }
    } # end loop on days
    if (simulation_parameters$resolutionOutput == "yearly") {
      write.WBoutput(Date = climDay$Date, WBoutput = model_output, WBsoil = soil_var_list, WBveg = veg_var_list, WBclim = Clim_next)
    }
  } # end loop on years
}
