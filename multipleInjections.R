source("R_func/InjectionSetup.R")
source('R_func/ReferenceGeneration.R')
events<-InjectionSetting.generation(InjATime=c(2,67,127,247,300,500,530,600)*24 , numberA = 100)

parmNames<-c('TeE','TrE','Tr2','Te2','TekODC','TrkTe','TekA','Pass_BBB_treg','Pass_BBB_teff','cA','CIL10','Cinf','NKkillsTeff_out','NK_prod_IFNg',
'NK_prod_IL10','IL17_BBB','IL10_BBB','Remyelinization_l_le2','IL10Consuption_in','IL17Consuption_in','INFgConsuption_in')
init <- unlist(read.csv("~/paramHealthy.csv", sep=""))
names(init)<-parmNames

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 365*2*24,
               s_time = 1,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events)

if(file.exists("Healthy_model_analysis")) system(paste('rm -dr', sprintf("Healthy_model_analysis")) )
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf("Healthy_model_analysis")) )


init <- unlist(read.csv("~/paramMS.csv", sep=""))
names(init)<-parmNames

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 365*2*24,
               s_time = 1,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events)

if(file.exists("MS_model_analysis")) system(paste('rm -dr', sprintf("MS_model_analysis")) )
system(paste('mv',
             sprintf("results_model_analysis"),
             sprintf("MS_model_analysis")) )


source('./R_func/Plot_modelAnalysis.R')


##############
############


init <- unlist(read.csv("./paramMS.csv", sep=""))
names(init)<-parmNames

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 24*30*24,
               s_time = 24,
               parameters_fname = "input/plistTherapy.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events, # se lo attivi al secondo giorno fa l'iniezione
               n_run = 100,
               n_config = 5,
               solver_type = "SSA",
               #taueps = .1,
               parallel_processors = 3)

if(file.exists("Therapy_MS_model_analysis")) system(paste('rm -dr', sprintf("Therapy_MS_model_analysis")) )
system(paste('mv',
             sprintf("results_model_analysis"),
             sprintf("Therapy_MS_model_analysis")) )

source("R_func/Plot_DiffTherapy.R")

