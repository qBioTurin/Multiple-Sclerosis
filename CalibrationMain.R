library(epimod)

#### Solver generation:
# model_generation(net_fname = "Net/MS_Model.PNPRO",
#                  functions_fname = "cpp/transition.cpp")


source("R_func/InjectionSetup.R")
events<-InjectionSetting.generation(InjATime=c(2)*24 , numberA = 100)
parmNames<-c('TeE','TrE','Tr2','Te2','TekODC','TrkTe','TekA','Pass_BBB_treg','Pass_BBB_teff','cA','CIL10','Cifn','NKkillsTeff_out','NK_prod_IFNg',
             'NK_prod_IL10','IL17_BBB','IL10_BBB','Remyelinization_l_le2','IL10Consuption_in','IL17Consuption_in','IFNgConsuption_in')

##### HEALTHY case:

init <- unlist(read.csv("./paramHealthy.csv", sep=""))
names(init)<-parmNames

## Deterministic:
model_analysis(solver_fname = "Net/MS_Model.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events
               )

if(file.exists("Healthy_model_analysis")) system(paste('rm -dr', sprintf("Healthy_model_analysis")) )
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf("Healthy_model_analysis")) )

## Stochastic:

model_analysis(solver_fname = "Net/MS_Model.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events,
               n_run = 1000,
               solver_type = "SSA",
               parallel_processors = 20)


if(file.exists("Healthy_Stoch_analysis")) system(paste('rm -dr', sprintf("Healthy_Stoch_analysis")) )
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf("Healthy_Stoch_analysis")) )

##### MS case:

init <- unlist(read.csv("./paramMS.csv", sep=""))
names(init)<-parmNames

## Deterministic:
model_analysis(solver_fname = "Net/MS_Model.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events
)

if(file.exists("MS_model_analysis")) system(paste('rm -dr', sprintf("MS_model_analysis")) )
system(paste('mv',
             sprintf("results_model_analysis"),
             sprintf("MS_model_analysis")) )

## Stochastic:

model_analysis(solver_fname = "Net/MS_Model.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events,
               n_run = 1000,
               solver_type = "SSA",
               parallel_processors = 20)

if(file.exists("MS_Stoch_analysis")) system(paste('rm -dr', sprintf("MS_Stoch_analysis")) )
system(paste('mv',
             sprintf("results_model_analysis"),
             sprintf("MS_Stoch_analysis")) )

#### Plot generation:

source('./R_func/FirstPlot.R')
