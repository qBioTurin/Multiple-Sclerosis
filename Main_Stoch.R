library(epimod)
source("R_func/InjectionSetup.R")
source('R_func/ReferenceGeneration.R')
events<-InjectionSetting.generation(InjATime=c(2)*24 , numberA = 100)
#making_the_reference("healthy")
# 

parmNames<-c('TeE','TrE','Tr2','Te2','TekODC','TrkTe','TekA','Pass_BBB_treg','Pass_BBB_teff','cA','CIL10','Cinf','NKkillsTeff_out','NK_prod_IFNg',
             'NK_prod_IL10','IL17_BBB','IL10_BBB','Remyelinization_l_le2','IL10Consuption_in','IL17Consuption_in','INFgConsuption_in')
init <- unlist(read.csv("./paramHealthy.csv", sep=""))
names(init)<-parmNames

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events,
               n_run = 500,
               solver_type = "SSA",
               #taueps = .1,
               parallel_processors = 20)


if(file.exists("Healthy_Stoch_analysis")) system(paste('rm -dr', sprintf("Healthy_Stoch_analysis")) )
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf("Healthy_Stoch_analysis")) )


init <- unlist(read.csv("./paramMS.csv", sep=""))
names(init)<-parmNames

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 24*30,
               s_time = 24,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events,
               n_run = 500,
               solver_type = "SSA",
               #taueps = .1,
               parallel_processors = 20)

if(file.exists("MS_Stoch_analysis")) system(paste('rm -dr', sprintf("MS_Stoch_analysis")) )
system(paste('mv',
             sprintf("results_model_analysis"),
             sprintf("MS_Stoch_analysis")) )

plotnames <- "StochDynamics"
Multy = F
source('./R_func/Plot_StochAnalysis.R')

