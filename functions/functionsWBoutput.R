# Creation WB_ouput 
new_WBoutput <-  function(general_params){

  outputVersion <- general_params$outputType
  
  if(outputVersion=='Simple') {    # just LAI,potential and soil layer content for now 
    
    
    contingencyTable <-data.frame(variable =  c('LAI' ,
                                                'SWS1',
                                                'SWS2',
                                                'SWS3'), 
                             corInMod      =  c('WBveg$LAI',
                                                'WBsoil$SoilWaterStock[1]',
                                                'WBsoil$SoilWaterStock[2]',
                                                'WBsoil$SoilWaterStock[3]'),
                             stringsAsFactors = F)
  }
  
  
  
  if(outputVersion=='FMCdiag') {    # just LAI,potential and soil layer content for now 
    
    
    contingencyTable <-data.frame(variable =  c('ETP',
                                                'VPD',
                                                'LAI' ,
                                                'LAI_dead',
                                                'SWS1',
                                                'SWS2',
                                                'SWS3',
                                                'AET',
                                                'PLCAbove',
                                                'dPLCAboveDay',
                                                'Defoliation',
                                                'LFMC',
                                                'LFMCApo',
                                                'LFMCSymp'),
                                  corInMod  = c('WBclim$ETP',
                                                'WBclim$VPD',
                                                'WBveg$LAI', 
                                                'WBveg$deadLAI',
                                                'WBsoil$SoilWaterStock[1]',
                                                'WBsoil$SoilWaterStock[2]',
                                                'WBsoil$SoilWaterStock[3]',
                                                'WBveg$AET.C',
                                                'WBveg$PLCAbove',
                                                'WBveg$dPLCAboveDay',
                                                'WBveg$defoliation',
                                                'WBveg$LFMC',
                                                'WBveg$LFMCApo',
                                                'WBveg$LFMCSymp'),
                                  stringsAsFactors = F)
  }
  
  
  
  
  
  if(outputVersion=='RootProfile') {    # just LAI,potential and soil layer content for now 
    
    
    contingencyTable <-data.frame(variable =  c('ETP',
                                                'VPD',
                                                'RH',
                                                'LAI' ,
                                                'SWS1',
                                                'SWS2',
                                                'SWS3',
                                                'AET',
                                                'AET1',
                                                'AET2',
                                                'AET3',
                                                'kSoil1',
                                                'kSoil2',
                                                'kSoil3',
                                                'kStoC1',
                                                'kStoC2',
                                                'kStoC3',
                                                'PsiSoil1',
                                                'PsiSoil2',
                                                'PsiSoil3',
                                                'REW1',
                                                'REW2',
                                                'REW3',
                                                'Psi_AllSoil',
                                                'kPlant',
                                                'Emin'),
                                  
                                  corInMod  = c('WBclim$ETP',
                                                'WBclim$VPD',
                                                'WBClim$RHair_mean',
                                                'WBveg$LAI',
                                                'WBsoil$SoilWaterStock[1]',
                                                'WBsoil$SoilWaterStock[2]',
                                                'WBsoil$SoilWaterStock[3]',
                                                'WBveg$AET.C',
                                                'WBveg$fluxSoilToCollar.C[1]',
                                                'WBveg$fluxSoilToCollar.C[2]',
                                                'WBveg$fluxSoilToCollar.C[3]',
                                                'WBsoil$kSoil[1]',
                                                'WBsoil$kSoil[2]',
                                                'WBsoil$kSoil[3]',
                                                'WBveg$kSoilToCollar[1]',
                                                'WBveg$kSoilToCollar[2]',
                                                'WBveg$kSoilToCollar[3]',
                                                'WBsoil$PsiSoil[1]',
                                                'WBsoil$PsiSoil[2]',
                                                'WBsoil$PsiSoil[3]',
                                                'WBsoil$REW[1]',
                                                'WBsoil$REW[2]',
                                                'WBsoil$REW[3]',
                                                'WBveg$Psi_AllSoil',
                                                'WBveg$kPlant',
                                                'WBveg$Emin'),
                                  stringsAsFactors = F)
  }
  

  if(outputVersion=='ValidICOS_simple') {    # just LAI,potential and soil layer content for now 
    
    
    contingencyTable <-data.frame(variable =  c('ETP',
                                                'ETPr',
                                                'VPD',
                                                'RG',
                                                'LAI',
                                                "Psi_LApo",
                                                "Psi_TApo",
                                                "Psi_LSym",
                                                "Psi_TSym",
                                                'SWS1',
                                                'SWS2',
                                                'SWS3',
                                                'AET',
                                                'AET.C',
                                                'AET1.C',
                                                'AET2.C',
                                                'AET3.C',
                                                'PsiSoil1',
                                                'PsiSoil2',
                                                'PsiSoil3',
                                                'REW1',
                                                'REW2',
                                                'REW3',
                                                'REWt',
                                                'Psi_AllSoil',
                                                'kAll',
                                                'Emin',
                                                'Ebound',
                                                'Elim',
                                                'Tleaf',
                                                'RH',
                                                'Tair',
                                                "PAR",
                                                'CavitBelow',
                                                'CavitAbove',
                                                'SoilEvaporation',
                                                'gmin',
                                                'gs',
                                                'gs_bound',
                                                'gs_lim',
                                                'gcanopy_Bound',
                                                'gcanopy_lim',
                                                'GbL',
                                                'g_crown',
                                                'RegulFact',
                                                'Q_TApo_mmol',
                                                'Q_LApo_mmol',
                                                'Psi_LApo_cav',
                                                'Psi_TApo_cav',
                                                'Delta_Q_LApo_mmol_diag',
                                                'F_L_Cav',
                                                'kAboveGround',
                                                'kBelowGround',
                                                'Diag_deltaRegulMax',
                                                'Diag_deltaPLCMax',
                                                'Diag_nwhile_cavit',
                                                'Diag_timeStepInHours'
                                               
                                               ),
                                
                                  
                                  corInMod  = c('WBclim$ETP',
                                                'WBveg$ETPr',
                                                'WBclim$VPD',
                                                'WBclim$RG',
                                                'WBveg$LAI',
                                                "WBveg$Psi_LApo",
                                                "WBveg$Psi_TApo",
                                                "WBveg$Psi_LSym",
                                                "WBveg$Psi_TSym",
                                                'WBsoil$SoilWaterStock[1]',
                                                'WBsoil$SoilWaterStock[2]',
                                                'WBsoil$SoilWaterStock[3]',
                                                'WBveg$SumFluxSoilToCollar',
                                               # 'WBveg$Transpiration',
                                                'WBveg$AET.C',
                                                'WBveg$fluxSoilToCollar.C[1]',
                                                'WBveg$fluxSoilToCollar.C[2]',
                                                'WBveg$fluxSoilToCollar.C[3]',
                                                'WBsoil$PsiSoil[1]',
                                                'WBsoil$PsiSoil[2]',
                                                'WBsoil$PsiSoil[3]',
                                                'WBsoil$REW[1]',
                                                'WBsoil$REW[2]',
                                                'WBsoil$REW[3]',
                                                'WBsoil$REWt',
                                                'WBveg$Psi_AllSoil',
                                                'WBveg$KAllSoilToCanopy',
                                                'WBveg$Emin',
                                                'WBveg$Ebound',
                                                'WBveg$Elim',
                                                'WBveg$Tleaf',
                                                "WBclim$RHair_mean",
                                                'WBclim$Tair_mean',
                                                'WBclim$PAR',
                                                'WBveg$PLCBelow',
                                                'WBveg$PLCAbove',
                                                'WBsoil$EvaporationSum',
                                                'WBveg$gmin',
                                                'WBveg$gs',
                                                'WBveg$gs_bound',
                                                'WBveg$gs_lim',
                                                'WBveg$gcanopy_Bound',
                                                'WBveg$gcanopy_lim',
                                                'WBveg$gBL',
                                                'WBveg$g_crown',
                                                'WBveg$RegulFact',
                                                'WBveg$Q_TApo_mmol',
                                                'WBveg$Q_LApo_mmol',
                                                'WBveg$Psi_LApo_cav',
                                                'WBveg$Psi_TApo_cav',
                                                'WBveg$Delta_Q_LApo_mmol_diag',
                                                'WBveg$F_L_Cav',
                                                'WBveg$kAboveGround',
                                                'WBveg$kBelowGround',
                                                'WBveg$Diag_deltaRegulMax',
                                                'WBveg$Diag_deltaPLCMax',
                                                'WBveg$Diag_nwhile_cavit',
                                                'WBveg$Diag_timeStepInHours'
                                                  ),
                                  stringsAsFactors = F)
  }
  

    if (general_params$addInfotoFileName==T){
    filename = paste0('../Results_model/',general_params$fileNameInfo,'_Results_model_',outputVersion,'_',paste0(format(Sys.time(), "%Y-%m-%d_%H-%M"), ".csv"))
    } else {filename= paste0('../Results_model/',general_params$fileNameInfo,".csv")} 
    
  
    # creation du fichier avec le nom de la variable 
    Output = data.frame(data=matrix(ncol=length(contingencyTable$variable)+1,nrow=0),stringsAsFactors = F)
    colnames(Output) <- c('Time',contingencyTable$variable)

    fwrite(Output,file=filename,col.names=T,row.names=F)

   return(list(filename=filename,contingencyTable=contingencyTable))
    
}
# ecriture dans les fichiers
write.WBoutput <- function(WBoutput,TIME,WBveg,WBsoil,WBclim){
    

  df = data.frame(TIME=TIME)
  
  df2= data.frame(matrix(NA,nrow=1,ncol=length(WBoutput$contingencyTable$variable)))
  for(i in (1:length(WBoutput$contingencyTable$variable)))
  {
    #print(i) #// for debugging only
    df2[1,i] <- eval(parse(text = WBoutput$contingencyTable$corInMod[i]))
  }
  
  temp=cbind(df,df2)
  
  io = grep("numeric",sapply(temp,FUN=class))
  temp[,io] = sapply(temp[,io],formatC)
  fwrite(x=temp,file=WBoutput$filename,append=T,row.names=F)
  
  
  
  
}