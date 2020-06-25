library(ggplot2)
library(cowplot)

dirs<- list.dirs()
folders <- grep("Therapy_*", dirs, value = TRUE)

traceAll =  lapply( folders, function(i)
{
  p <- readRDS(paste0("./",i,"/params_Rete_SM_newAM_SR_Laura-analysys.RDS") )
  filesAll <- list.files(paste0("./",i,"/"),pattern='*.trace')
 
  traceAll <- lapply(filesAll, function(x,param=p) {
  IDsim<- as.numeric(gsub(pattern="(.trace)", gsub(pattern="([[:graph:]]+(-){1})", x=x, replacement=""), replacement="") )
    rate <- param$ini_v[10]
    param$event.list[[length(param$event.list)]][[2]]["DAC"] -> qDAC
    
    data.frame(read.csv(paste0('./',i,'/',x), sep="") ,ID= paste("rate",rate,"; DAC",qDAC ) )
  } )
  trace<-do.call("rbind", traceAll)
})

trace<-do.call("rbind", traceAll)


time <- unique(trace$Time)
nsim <- unique(table(trace$Time))

trace$SimID = rep(1:nsim,each = length(time))

#(unique(trace$ID) )-> DACKill

#trace = trace[which(trace$ID %in% c(min(DACKill),max(DACKill))),]

Entities = c("Teff_out","Treg_out","Antigen" ,
             "EffectorMemory","NK_out","BBB",
             "IL10_out","IL17_out","INFg_out", 
             "Treg_in", "Teff_in","ODC_le1" ,
             "IL17_in","INFg_in","IL10_in","DAC")

dataCreation <- function(trace){
  DatiPlotlist<- lapply(1:length(Entities),function(i){
    place<-Entities[i]
    
    sub_dati=trace[,c("Time",place)]
    
    dt=data.frame(sub_dati,ID=trace$ID,Ent=place)
    colnames(dt)=c("Time","Value","ID","Entity")
    return(dt)
  })
  
  DatiPlot<-do.call("rbind", DatiPlotlist)
  
  return(DatiPlot)
}


Mean <- aggregate(trace[, which(colnames(trace) %in% Entities)], list(Time = trace$Time,ID = trace$ID), mean )
ub <- aggregate(trace[, which(colnames(trace) %in% Entities)],list(Time = trace$Time,ID = trace$ID), max)
lb <- aggregate(trace[, which(colnames(trace) %in% Entities)], list(Time = trace$Time,ID = trace$ID), min)

DatiPlot <- dataCreation(trace)
MeanPlot <- dataCreation(Mean)
ubPlot <- dataCreation(ub)
lbPlot <- dataCreation(lb)

MeanArea<-data.frame(mean=MeanPlot,ub=ubPlot$Value,lb=lbPlot$Value)


pl=ggplot(MeanArea,aes(x=mean.Time/24))+
  geom_ribbon(aes(ymin=lb,ymax=ub,fill=as.factor(mean.ID) ),color="grey70",alpha=0.2)+
  geom_line(aes(y=mean.Value ,color= as.factor(mean.ID) ))+
  facet_wrap(~ mean.Entity,scales = "free",ncol = 3)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position="top",
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm") )

pl

ggsave(pl,filename = paste0("Plot/DiffTherapy.pdf"),device = "pdf",width = 16, height = 20 )

