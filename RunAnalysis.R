source("R_func/InjectionSetup.R")
source('R_func/ReferenceGeneration.R')
events<-InjectionSetting.generation(InjATime=c(2)*24 , numberA = 100)
#events<-InjectionSetting.generation(InjATime=c(7,67,127,187,247)*24 , numberA = 100)
#making_the_reference("healthy")
# 
#  library(devtools)
# install_github("https://github.com/qBioTurin/epimod", ref="DiscreteEvents",dependencies=TRUE,force=TRUE)
# library(epimod)
# downloadContainers()
# 
library(epimod)
library(readr)

# downloadContainers()

model_generation(net_fname = "Net/Rete_SM_newAM_SR_Laura.PNPRO",
                 functions_fname = "cpp/transitionNew.cpp")



sensitivity_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
                     f_time = 20*24,
                     s_time = 24,
                     n_config = 1,
                     parameters_fname = "input/plistSensitivityNew.csv",
                     functions_fname = "R_func/Functions.R",
                     distance_measure_fname = "R_func/msqd.R",
                     reference_data = "input/reference.csv",
                     target_value_fname = "R_func/prcc_target.R",
                     timeout = "1d",
                     parallel_processors = 1,
                     event.list = events)

source('./R_func/Plot_sensitivity.R')

init <- unlist(read_table2("paramFromSens.csv"))
init["TeE"]<- 0.01
init["TrE"]<- 0.001
init["Te2"]<- 0.05
init["Tr2"]<- 0.001
init["TrkTe"]<- 0.001
init["TekA"]<- 0.0001
init["TekODC"]<- 0.0001
init["NKkillsTeff_out"]<- 0.001
init["INFgConsuption_in"]<-.4
init["Pass_BBB_teff"]<-.01
init["cA"]<- 20


Lb <- init* 0.98
Ub <- init*1.2

model_calibration(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
                  f_time = 20*24,
                  s_time = 24,
                  parameters_fname = "input/plistCalib.csv",
                  functions_fname = "R_func/Functions.R",
                  distance_measure_fname = "R_func/msqd.R",
                  reference_data = "input/reference.csv",
                  timeout = "1d",
                  ini_v = init,
                  lb= Lb,
                  ub= Ub,
                  event.list = events )



source('./R_func/reading_from_calibration.R')
write.table(t(optim),file = "paramFromCalib.csv",row.names = FALSE,quote = F)


model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 20*24,
               s_time = 1,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               event.list = events)

# Automatically the plots regarding the Infected individuals and deaths
# are generated

source('./R_func/Plot_modelAnalysis.R')
