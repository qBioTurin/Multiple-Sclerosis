library(ggplot2)
load("~/results_sensitivity_analysis/ranking_Rete_SM_newAM_SR_Laura-sensitivity.RData")
load("./results_sensitivity_analysis/Rete_SM_newAM_SR_Laura-sensitivity.RData")
load("./results_sensitivity_analysis/prcc_Rete_SM_newAM_SR_Laura-sensitivity.RData")
config<-readRDS("./results_sensitivity_analysis/params_Rete_SM_newAM_SR_Laura-sensitivity.RDS")

filesAll <- list.files('./results_sensitivity_analysis/',pattern='*.trace')

######### FIND the best solution
min.rank<-rank[which.min(rank$measure),"id"]
config$config-> parmasList

parm <- lapply(parmasList, `[[`,min.rank)
parm<-  parm[-1]
names.parm <- sapply(parm, `[[`,1)
namesToDelete<-c("SecondInjectionTime","Remyelinization_l_le3", "Remyelinization_l_le4" ,"DACkillTreg","DACkillTeff" ,"Treg_prod_IL10_in",
  "IL10Consuption_out","IL17Consuption_out","INFgConsuption_out","FromTimoEFF","FromTimoREG",  "DACDegradation","NKarrive","NK2",
  "Teff_prod_IFNg_in", "Teff_prod_IL17_in","Treg_prod_IL10","Teff_prod_IL17","Teff_prod_IFNg"  )
names.parm.sub <- names.parm[-which(names.parm %in% namesToDelete )]

parameters<-sapply(names.parm.sub,function(i){
  parm[[which(names.parm == i)]][3]
})
parameters=unlist(parameters)
write.table(t(parameters),file = "paramFromSens.csv",row.names = FALSE,quote = F)

################################

rank[1:50,2] ->best.id

traceAll <- lapply(best.id, function(x) {
  #IDsim<- -readr::parse_number(x)
  #IDsim<- as.numeric(gsub(pattern="(.trace)", gsub(pattern="([[:graph:]]+(-){1})", x=x, replacement=""), replacement="") )
  #data.frame(read.csv(paste0('./results_sensitivity_analysis/',x), sep="") ,ID=IDsim)

  data.frame(read.csv(paste0('./results_sensitivity_analysis/Rete_SM_newAM_SR_Laura-sensitivity-',x,".trace"), sep="") ,ID=x)
  
} )

dati<-do.call("rbind", traceAll)

Entities = c("Resting_Teff_out","Teff_out","Resting_Treg_out","Treg_out",
             "EffectorMemory","NK_out","BBB","Antigen" ,
             "IL10_out","IL17_out","INFg_out", "ODC_le1" ,
             "Resting_Treg_in","Treg_in", "Resting_Teff_in", "Teff_in",
             "IL17_in","INFg_in","IL10_in")

min.rank<-rank[which.min(rank$measure),"id"]


DatiPlotlist<- lapply(1:length(Entities),function(i){
  place<-Entities[i]
  sub_dati=dati[,c("Time",place,"ID")]
  
  score_list=lapply(sub_dati$ID, function(j){
    rank[which( rank$id == j),1]
  } )
    
  score= do.call("rbind", score_list)
  
  dt=data.frame(sub_dati,Error=score,Ent=place)
  colnames(dt)=c("Time","Value","ID","Error","Entity")
  return(dt)
})

DatiPlot_all<-do.call("rbind", DatiPlotlist)

DatiPlot <- DatiPlot_all[which(DatiPlot_all$Error %in% sort(rank$measure)[1:100]),]
DatiPlot <-DatiPlot[order(DatiPlot$Error,decreasing = T),]

pl<-ggplot(DatiPlot[DatiPlot$Entity %in% Entities,],aes(x=Time/24,y=Value,color=Error,group=ID))+
    geom_line()+
    facet_wrap(~ Entity,scales = "free",ncol = 4)

ggsave(pl,filename = "Plot/LinesSensitivity.pdf",device = "pdf",width = 16, height = 20 )


#########################
### PRCC plot
table(dati$Time)

ID = dati[which(dati$Time == max(dati$Time)),"ID"]

idNofinished<-unique(dati$ID[ which( ! (  dati$ID %in% ID) ) ])

time <- c(0:20)*365

#####################################
### in prcc 
### Rows= times Columns=parameters

prcc <- t(unname(as.data.frame(prcc)))
n_param= length(prcc[1,])
#Param_names = colnames(prcc)
Param_names = paste("V",1:n_param,sep="") ## Questo poi dovrebbe essere sostituito con il nome dei Parametri, che immagino dovrebbe essere il nome delle colonne di prcc, giusto?

prcc_frame <- data.frame(Time = rep(time,n_param), PRCC = c(prcc), Param = rep(Param_names,each=length(time)) )

#names(prcc)[1]<-"Time"

plt <- ggplot(prcc_frame, aes(x=Time/365))+ 
  geom_line(aes(y=PRCC,group=Param,col=Param)) +
  ylim(-1,1) +
  xlab("Time")+ 
  ylab("PRCC")+
  geom_rect(
    mapping=aes(xmin=-Inf, xmax=Inf, ymin=-.2, ymax=.2),
    alpha=0.001961,
    fill="yellow")

#########################