library(epimod)
source("R_func/InjectionSetup.R")
parmNames<-c('TeE','TrE','Tr2','Te2','TekODC','TrkTe','TekA','Pass_BBB_treg','Pass_BBB_teff','cA','CIL10','Cifn','NKkillsTeff_out','NK_prod_IFNg',
             'NK_prod_IL10','IL17_BBB','IL10_BBB','Remyelinization_l_le2','IL10Consuption_in','IL17Consuption_in','IFNgConsuption_in')

init <- unlist(read.csv("input/paramHealthy.csv", sep=""))
names(init)<-parmNames

Stoch = TRUE

model<-function(r,q, events,Stoch = FALSE, FolderName = NULL){
  init["cA"] <- r
  if(Stoch)
  {
    model_analysis(solver_fname = "Net/MS_Model.solver",
                   f_time = 24*30*24,
                   s_time = 24,
                   parameters_fname = "input/plistTherapy.csv",
                   functions_fname = "R_func/Functions.R",
                   ini_v = init,
                   event.list = events, # se lo attivi al secondo giorno fa l'iniezione
                   solver_type = "SSA",
                   n_run = 1000,
                   #taueps = .1,
                   parallel_processors = 2)
  }else{
     model_analysis(solver_fname = "Net/MS_Model.solver",
                 f_time = 24*30*24,
                 s_time = 24,
                 parameters_fname = "input/plistTherapy.csv",
                 functions_fname = "R_func/Functions.R",
                 ini_v = init,
                 event.list = events, # se lo attivi al secondo giorno fa l'iniezione
                 #solver_type = "SSA",
                 #taueps = .1,
                 parallel_processors = 20)
  }
 
  if(is.null(FolderName))folder<- paste0("EarlyTherapy_r",r,"_DAC",q)
  else if(FolderName == "Late") folder<- paste0("LateTherapy_r",r,"_DAC",q)
  else  folder<- paste0(FolderName)
  
  
  if(file.exists(folder)) system(paste('rm -dr', sprintf(folder)) )
  system(paste('mv',
               sprintf("results_model_analysis"),
               sprintf(folder)) )
}

##### Healthy
eventsNoTherapy<-InjectionSetting.generation(InjATime=c(2,67,127,295,300,303,307,600)*24 , numberA = 100)

model(r = 0,q = 0 , events = eventsNoTherapy,Stoch = Stoch, FolderName = "Therapy_Healthy0")

#### MS
init <- unlist(read.csv("input/paramMS.csv", sep=""))
names(init)<-parmNames

model(r = 0,q = 0 , events = eventsNoTherapy,Stoch = Stoch,FolderName = "Therapy_MS")

#### MS with late therapy

#result<-lapply(c(50,100,200,400,600,800),function(q){
result<-lapply(c(1000,2000,5000,10000,15000),function(q){
  
  eventsTherapy<-InjectionSetting.generation(InjATime=c(2,67,127,295,300,303,307,600)*24 , numberA = 100,
                                             numberDAC = q, InjDACTime = (6:23)*30*24 )
  
  r1<-lapply(c(0.01 ,0.02, 0.03 ), function(r,q1=q, event = eventsTherapy){
    rete <- model(r,q1,event,Stoch = Stoch,FolderName = "Late")
  })
  
})

#### MS with early therapy
#result<-lapply(c(50,100,200,400,600,800),function(q){
result<-lapply(c(1000,2000,5000,10000,15000),function(q){
  eventsTherapy<-InjectionSetting.generation(InjATime=c(2,67,127,295,300,303,307,600)*24 , numberA = 100,
                                             numberDAC = q, InjDACTime = (1:23)*30*24 )
  
  r1<-lapply(c(0.01 ,0.02,0.03), function(r,q1=q, event = eventsTherapy){
    rete <- model(r,q1,event,Stoch = Stoch)
  })
  
})
