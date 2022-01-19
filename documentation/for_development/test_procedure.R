# created on 04/08/2021  / under development 
# author : julien ruffault (julien.ruff@gmail.com)
# launch a series a simulations to check SurEau-Ecos functioning and help with bugs identification
# more tests should be added 




# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# User options  ----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))            # <-- indicate here the main directory of SurEau_Ecos
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))                             # do not modify 



# test 1  /climate constant evergreen ------------------------------------------


# test 2 / climat constant / deciduous -----------------------------------------


# test 3 / real climate evergreen ----------------------------------------------

 



