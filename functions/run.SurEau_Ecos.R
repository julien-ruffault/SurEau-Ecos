#' Main function to run SureauR
#'
#' @param modeling_options a list with modeling options created by  \code{create.modeling.options}
#' @param simulation_parameters a list with simulation parameters created by  \code{create.simulation_parameters}
#' @param climate_data a data.frame with the input climate data created by  \code{create.climate.data}
#' @param soil_parameters a list with soil parameters created by  \code{create.soil.parameters}
#' @param vegetation_parameters a list with stand parameters created by  \code{create.vegetation/parameters}
#' @param stand_parameters a list with stand parameters created by  \code{create.stand.parameters}
#'
#' @return
#' @export
#'
#' @examples
#'
run.SurEau_Ecos <- function(modeling_options, simulation_parameters, climate_data, soil_parameters, vegetation_parameters, stand_parameters) { # start loop on years

  if (!nargs() == 6) {
    stop("One or several input parameters were missing")
  }
  soil_var_list <- new.WBsoil(soil_parameters, vegetation_parameters) #  create soil from input parameters
  veg_var_list <- new.WBveg(vegetation_parameters) #  create vegetation from vegetation parameters
  model_output <- new.WBoutput(simulation_parameters) #  create output file and parameters

  # @run@ ####
  for (YEAR in (simulation_parameters$startYearSimulation:simulation_parameters$endYearSimulation)) { # start loop on year
    
    
    print(paste0("SurEau running, year = ", YEAR))

    stopDeadPlant <- FALSE # set breaking conditions to allow to run on following years after death

    if (modeling_options$resetSWC == T) {
      soil_var_list <- set.SWCtoFieldCapacity.WBsoil(soil_var_list)
    }

    veg_var_list <- yearlyInitialisation.WBveg(veg_var_list)

    if (simulation_parameters$resolutionOutput == "yearly") {
      output_yearly <- new.WByearly() # create list for yearly outputs
    }

    for (DAY in climate_data$Doy[climate_data$Year == YEAR]) # Loop on days ####
    {
     
      if(modeling_options$printProg) {print(paste("Day=",DAY))}
      
      if (simulation_parameters$resolutionOutput %in% c("daily", "yearly")) {
        output_daily <- new.WBdaily() # create list for yearly outputs
      }
      
      climDay <- new.WBclim(climate_data = climate_data, YEAR = YEAR, DOY = DAY) # Create WBclim for the day
      veg_var_list <- compute.pheno.WBveg(WBveg = veg_var_list, temperature = climDay$Tair_mean, DOY = DAY) # LAI and update
      veg_var_list <- updateLAIandStocks.WBveg(WBveg = veg_var_list, modeling_options = modeling_options) # update reservoirs, Q and C 
      climDay <- compute.RnAndETP.WBclim(WBclim = climDay, WBveg = veg_var_list, RnFormulation = modeling_options$RnFormulation, ETPFormulation = modeling_options$ETPFormulation) # calculate Rn and ETP
      veg_var_list <- compute.interception.WBveg(WBveg = veg_var_list, ppt = climDay$PPT) # vegetation interceptedWaterAmount  and pptSoil with Interpception by the canopy
      veg_var_list$interceptedWaterAmount = 0
      soil_var_list <- compute.infiltration.WBsoil(WBsoil = soil_var_list, pptSoil = veg_var_list$pptSoil) # Infiltration / update soil water stocks / PsiSoil and KSoil
      climHour <- new.WBclimHour(WBclim = climDay, WBveg = veg_var_list, modeling_options = modeling_options, lat = stand_parameters$lat, lon = stand_parameters$lon, PTcoeff = veg_var_list$params$PTcoeff)

      # kplant should be computed after rainfall
      veg_var_list <- update.kplant.WBveg(veg_var_list, soil_var_list) # compute conductance soil+plant

      for (tt in 1:length(climHour$ETP)) # first hour is 0, last hour is 23 for a day
      {
        
        # set climate
        Clim_current <- lapply(climHour, function(x) x[max(tt - 1, 1)]) # select climate of the h
        Clim_next <- lapply(climHour, function(x) x[tt]) # select climate of the next time step
        Clim_mid <- interp.WBclim(Clim_current, Clim_next, 0.5) # note : Clim_mid is not a list of "type" WBclimHour

        # for the first iteration, Clim_current and Clim_next are climate at 0h (no matter the time_step_evap) so the first
        # iteration of the model is from 0h to 0h in order to equilibrate initial values with 0h climate
        # Then any iteration brings model variables to Clim_next (which is printed in the ouputs)

        veg_var_list <- compute.evapoIntercepted.WBveg(veg_var_list, ETP = Clim_mid$ETP)


        # Compute plant hydraulic to the next time step
        veg_var_list <- compute.plantNextTimeStep.WBveg(WBveg = veg_var_list, WBsoil = soil_var_list, Nhours = Clim_next$nHours, WBclim_current = Clim_current, WBclim_next = Clim_next, modeling_options = modeling_options,WBoutput=model_output)
        # compute water storage in vegetation
        veg_var_list <- compute.waterStorage.WBveg(veg_var_list, VPD = Clim_next$VPD)
        # update soil water stocks after T and water release and update psi and conductance
        # TODO with assign the values of the small time step could be updated exactly so that these final updates of soil could be removed
        # In theory the updatSoilWater is already consistent between the small time step and the large time step but it is not the case for soil evaporation
        if(modeling_options$soilEvap) {
          soil_var_list <- compute.evaporationG.WBsoil(WBsoil = soil_var_list, ETP = Clim_mid$ETP, Tair = Clim_mid$Tair_mean, RHair = Clim_mid$RHair, K = veg_var_list$params$K, LAI = veg_var_list$LAI, Nhours = Clim_next$nHours)  
          }
        soil_var_list <- update.soilWater.WBsoil(WBsoil = soil_var_list, fluxEvap = veg_var_list$fluxSoilToStem_mm)

        if (veg_var_list$PLC_Leaf >= modeling_options$thresholdMortatliy) {
          print("The plant is dead...")
          stopDeadPlant <- TRUE
          break
        }


        if (simulation_parameters$resolutionOutput == "subdaily") {
          write.WBoutput(Date = climDay$Date, WBoutput = model_output, WBsoil = soil_var_list, WBveg = veg_var_list, WBclim = Clim_next)
        }

        if (simulation_parameters$resolutionOutput %in% c("daily", "yearly")) {
          output_daily <- update.WBdaily(WBdaily = output_daily, WBsoil = soil_var_list, WBveg = veg_var_list, WBclim = climDay)
        }

        if (stopDeadPlant == T) {
          break
        }
      } # end loop on hours
      if (simulation_parameters$resolutionOutput == "daily") {
        write.WBoutput.daily(Date = climDay$Date, WBoutput = model_output, WBdaily = output_daily)
      }
      if (simulation_parameters$resolutionOutput == "yearly") {
        if (stopDeadPlant == T) {
          output_yearly <- update.WByearly(WByearly = output_yearly, WBdaily = output_daily, WBveg = veg_var_list, dayOfDeath = DAY)
        } else {
          output_yearly <- update.WByearly(WByearly = output_yearly, WBdaily = output_daily, WBveg = veg_var_list , DAY = DAY)
        }
      }


      if (stopDeadPlant == T) {
        break
      }
    } # end loop on days
    if (simulation_parameters$resolutionOutput == "yearly") {
      write.WBoutput.yearly(year = YEAR, WBoutput = model_output, WByearly = output_yearly)
    }
  } # end loop on years
  close(con = model_output$testcon)
}
