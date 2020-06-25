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

# init["TeE"] <- 0.015
# init["TrE"] <- 0.0004
# init["Te2"] <- 0.04
# init["Tr2"] <- 0.006
# init["TrkTe"] <- .02
# init["TekA"] <- 0.0006
# init["TekODC"] <- 0.0006
# init["Pass_BBB_treg"] <- 0.05
# init["Pass_BBB_teff"] <- 0.005
# init["CIL10"] <- 10
# init["Cinf"] <- 20 
# init["NKkillsTeff_out"] <- 0.01
# init["NK_prod_IFNg"] <- 0.03
# init["NK_prod_IL10"] <- 0.045
# init["IL17_BBB"] <- 0.07  
# init["IL10_BBB"] <- 0.08  
# init["Remyelinization_l_le2"] <- 0.01035
# init["IL10Consuption_in"] <- 0.09
# init["IL17Consuption_in"] <- 0.03
# init["INFgConsuption_in"] <- 0.05 

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 30*24,
               s_time = 1,
               parameters_fname = "input/plistCalib.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = init,
               event.list = events
               )

if(file.exists("Healthy_model_analysis")) system(paste('rm -dr', sprintf("Healthy_model_analysis")) )
system(paste('mv', 
             sprintf("results_model_analysis"),
             sprintf("Healthy_model_analysis")) )

source('./R_func/Plot_modelAnalysis.R')

init <- unlist(read.csv("./paramMS.csv", sep=""))
names(init)<-parmNames

 init["TeE"] <- 0.0175
# init["TrE"] <- 0.00007

model_analysis(solver_fname = "Net/Rete_SM_newAM_SR_Laura.solver",
               f_time = 30*24,
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

#write.table(t(init),file = "paramMS.csv",row.names = FALSE,quote = F)
