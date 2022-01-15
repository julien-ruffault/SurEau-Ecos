
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
# Notes to implement  specific hydraulic conductance (JR : #19/12/2020) 
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##

# inputs ( in veg paramaters)
#   - LSPCinit   : leaf specific plant hydraulic conductance (mmmol/m2feuille/s/MPa)  / mesurée à partir des saplflows / ou des fluxes 
#   - LAImax     : leaf area index maximum of the stand (m2/m2) 

# derived parameters (do not change with time)
#   - kEcosInit  : LSCPinit*LAImax (mmol/m2sol/sMPa) 
#   - kAboveInit = 2*kEcosInit // half of the resistance in the roots
#   - kBelowInit = 2*kEcosInit // half of the resistance in aerial biomass

# at each time step  : 
#   - kAbove = kAboveInit*f(PLC_Above)
#   - kBelow = kBelowInit*f(PLC_Below)
#   - kEcos  = 1/((1/kAbove)+(1/kBelow))

# 
# computeNotOK = T
# while(computeNotOK) 
# {
#   # ----------------------
#   # 2. Compute intermediates
#   K_L_td = Eprime_n + C_LApo/dt + 1/(1/K_LSym + dt/C_LSym) + delta_L_cav*K_L_Cav  
#   Psi_L_td = ((Eprime_n + C_LApo/dt)*Psi_LApo_n + (1 /(1/K_LSym + dt/C_LSym))*Psi_LSym_n + delta_L_cav*K_L_Cav*Psi_LApo_mem)/(K_L_td + dbxmin) # dbxmin to avoid 0/0
#   
#   K_T_td = C_SApo/dt + 1/(1/K_TSym + dt/C_SSym) + sum(WBveg$k_SoilToStem)  + delta_T_cav*K_T_Cav 
#   #Erreur dans psi_T_td, FP: A supprimer ?
#   Psi_T_td = (C_SApo/dt*Psi_SApo_n + 1/(1/K_TSym + dt/C_SSym)*Psi_SSym_n + sum(WBveg$k_SoilToStem * WBsoil$PsiSoil) + delta_T_cav*K_T_Cav*Psi_SApo_mem) / (K_T_td + dbxmin) # dbxmin to avoid 0/0
#   
#   # 3. Compute Psi_LApo_np1
#   Eleaks = E_n + Emin_L_np1/(1+(C_LSym+dbxmin)/(K_LSym*dt)) + K_TL/(K_TL + K_T_td) * Emin_T_np1/(1+(C_SSym + dbxmin)/(K_TSym *dt))  # dbxmin to avoid 0/0  FP ATTENTION BUG SUR EMIN!!!!
#   Psi_LApo_np1_Num = kseriesum(K_TL , K_T_td + dbxmin)*Psi_T_td + K_L_td*Psi_L_td - Eleaks
#   Psi_LApo_np1_Denom = kseriesum(K_TL, K_T_td + dbxmin) + K_L_td + dbxmin # dbxmin to avoid 0/0
#   Psi_LApo_np1 = Psi_LApo_np1_Num/Psi_LApo_np1_Denom
#   
#   # 4. Compute Psi_SApo_np1
#   Psi_SApo_np1 = ((K_L_td + K_TL)*Psi_LApo_np1 - K_L_td*Psi_L_td + E_n + Emin_L_np1 / (1+(C_LSym + dbxmin)/(K_LSym*dt)))/(K_TL+ dbxmin) #   FP ATTENTION BUG SUR EMIN!!!!
#   
#   # 5. Compute Psi_Symp_np1 (L and T)
#   Psi_LSym_np1 = (K_LSym*Psi_LApo_np1 + C_LSym/dt*Psi_LSym_n - Emin_L_np1) / (K_LSym + C_LSym/dt + dbxmin) # dbxmin to avoid 0/0
#   Psi_SSym_np1 = (K_TSym*Psi_SApo_np1 + C_SSym/dt*Psi_SSym_n - Emin_T_np1) / (K_TSym + C_SSym/dt + dbxmin) # dbxmin to avoid 0/0
#   
#   
#   if(Psi_SApo_np1 > Psi_SApo_mem & !delta_T_cav) {Scav_well_computed=1}
#   if(Psi_SApo_np1 < Psi_SApo_mem & !delta_T_cav) {Scav_well_computed=0}
#   if(Psi_SApo_np1 < Psi_SApo_mem & delta_T_cav) {Scav_well_computed=1}
#   #Scav_well_computed = (Psi_SApo_np1 > Psi_SApo_mem)*delta_T_cav  # 1 if OK
#   computeNotOK = 1 -  Scav_well_computed
#   #print(computeNotOK)
#   if (computeNotOK==0) { # we update Psi_SApo_mem if needed before leaving the while loop
#     if (Psi_SApo_np1 < Psi_SApo_mem) {Psi_SApo_mem = Psi_SApo_np1}
#   } else { # trunk cavitation is not well computed in the first path so a second is needed with T cavitation 
#     delta_T_cav = 1 # in the next step of the while, we do the computation with Scavit activated
#   }
#   
#   F_S_Cav = K_T_Cav * (Psi_SApo_mem-Psi_SApo_np1) * dt
#   print(F_S_Cav)
#   Q_SApo_mmol <- WBveg$Q_SApo_mmol - max(F_S_Cav,0)
#   
#   
#   
# }
# 
# 


# ComputeCapaSymp <- function(Q_LSym_sat_mmol, PiFullTurgor, EpsilonSymp, Psi)
# {
#   
#   PsiTLP <- PiFullTurgor*EpsilonSymp/(PiFullTurgor+EpsilonSymp)
#   dbxmin = 1e-100 # NM minimal double to avoid-INF
#   Psi=Psi-dbxmin
#   Rs1 <- (-1 * (Psi + PiFullTurgor - EpsilonSymp) - sqrt((Psi + PiFullTurgor - EpsilonSymp)^2 + 4 * (Psi * EpsilonSymp))) / (2 * EpsilonSymp)
#   Rs2 <- 1 - PiFullTurgor / Psi
#   
#   Rs1[Rs1<Rs2]<-Rs2[Rs1<Rs2]
#   #plot((1-Rs1)~x)
#   
#   RWC_LSym <- 1 - Rs1 # Relative water content (unitless)
#   #----Compute the derivative of the relative water content of the symplasm----
#   RWC_LSym_prime1 = RWC_LSym/(-PiFullTurgor- Psi- EpsilonSymp+2*EpsilonSymp*RWC_LSym)
#   RWC_LSym_prime2 = -(PiFullTurgor)/Psi^2
#   #Compute the leaf capacitance (mmol/MPa/m2_sol)
#   RWC_LSym_prime1[Psi<PsiTLP]<-RWC_LSym_prime2[Psi<PsiTLP]
#   
#   C_LSym <- Q_LSym_sat_mmol*RWC_LSym_prime1
#   return(list("C_LSym"=C_LSym, "RWC_LSym_prime1"=RWC_LSym_prime1,"RWC_LSym"=RWC_LSym))
#   
# # }
# 
# 
# ComputeCapaSymp(Q_LSym_sat_mmol=10000, PiFullTurgor=-2, EpsilonSymp=8, Psi=-1)
# 
# x=seq(0,-6, -0.1)
# CapaSym=ComputeCapaSymp(Q_LSym_sat_mmol=10000, PiFullTurgor=-2, EpsilonSymp=10, Psi=x)
# quartz()
# par(las=1, tck=0.01, mar=c(4.1,4.1,4.1,4.1))
# plot(CapaSym$C_LSym~x, type='b', ylab="Capa_Symp (mmol/m2/s/MPa)")
# par(new=T, las=1, tck=0.01)
# plot(CapaSym$RWC_LSym~x, type='l', lwd=2, col=2, ylab="", yaxt="n")
# axis(4, col=2, col.axis=2)
# mtext("RWC", 4, line=1.5, col=2, adj=0.5)
# ?mtext
