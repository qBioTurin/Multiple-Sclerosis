library(ggplot2)
library(cowplot)

dirs<- list.dirs()
folders <- grep("Therapy_*", dirs, value = TRUE)
namesFol <- c(rep("Early therapy",12 ),rep("Late therapy",12 ),rep("No therapy",2) )
traceAll =  lapply( 1:length(folders), function(i)
{
  p <- readRDS(paste0("./",folders[i],"/params_Rete_SM_newAM_SR_Laura-analysys.RDS") )
  filesAll <- list.files(paste0("./",folders[i],"/"),pattern='*.trace')
 
  traceAll <- lapply(filesAll, function(x,param=p, ind = i) {
  IDsim<- as.numeric(gsub(pattern="(.trace)", gsub(pattern="([[:graph:]]+(-){1})", x=x, replacement=""), replacement="") )
    rate <- param$ini_v[10]
    param$event.list[[length(param$event.list)]][[2]]["DAC"] -> qDAC
    
    if(folders[ind] == "./Therapy_Healthy0") data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("Healthy" ), Therapy = namesFol[ind], DACq = "Healthy",rate = "Healthy" )
    else if(folders[ind] == "./Therapy_r0_DAC0") data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("MS"), Therapy = namesFol[ind], DACq = "MS",rate = "MS" )
      else data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("rate",rate,"; DAC",qDAC ), Therapy = namesFol[ind], DACq = paste(qDAC),rate =paste(rate) )
    
  } )
  
  trace<-do.call("rbind", traceAll)
})

trace<-do.call("rbind", traceAll)


time <- unique(trace$Time)
nsim <- unique(table(trace$Time))

trace$SimID = rep(1:nsim,each = length(time))

#(unique(trace$ID) )-> DACKill

#trace = trace[which(trace$ID %in% c(min(DACKill),max(DACKill))),]

Entities = c("Antigen" , "DAC" ,"Teff_out","INFg_out", "IL17_out","Treg_out",
             "IL10_out","NK_out","BBB",
               "Teff_in" ,"INFg_in",
              "IL17_in","Treg_in","IL10_in","ODC_le1")

dataCreation <- function(trace){
  DatiPlotlist<- lapply(1:length(Entities),function(i){
    place<-Entities[i]
    
    sub_dati=trace[,c("Time",place)]
    
    dt=data.frame(sub_dati,ID=trace$ID,Therapy = trace$Therapy, Ent=place, DACq = trace$DACq, rate =trace$rate)
    colnames(dt)=c("Time","Value","ID","Therapy","Entity","DACq","rate")
    return(dt)
  })
  
  DatiPlot<-do.call("rbind", DatiPlotlist)
  
  return(DatiPlot)
}


Mean <- aggregate(trace[, which(colnames(trace) %in% Entities)], list(Time = trace$Time,ID = trace$ID,Therapy = trace$Therapy,DACq = trace$DACq, rate =trace$rate), median )
ub <- aggregate(trace[, which(colnames(trace) %in% Entities)],list(Time = trace$Time,ID = trace$ID,Therapy = trace$Therapy,DACq = trace$DACq, rate =trace$rate), FUN = function(i, ...) quantile(i, probs = 0.75) )
lb <- aggregate(trace[, which(colnames(trace) %in% Entities)], list(Time = trace$Time,ID = trace$ID,Therapy = trace$Therapy,DACq = trace$DACq, rate =trace$rate), FUN = function(i, ...) quantile(i, probs = 0.25))

DatiPlot <- dataCreation(trace)
MeanPlot <- dataCreation(Mean)
ubPlot <- dataCreation(ub)
lbPlot <- dataCreation(lb)

MeanArea<-data.frame(mean=MeanPlot,ub=ubPlot$Value,lb=lbPlot$Value)
MeanArea$mean.DACq = factor(MeanArea$mean.DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
MeanArea$mean.ID = factor(MeanArea$mean.ID, levels = c("Healthy","MS",
                                                       "rate 0.015 ; DAC 50" , "rate 0.015 ; DAC 100","rate 0.015 ; DAC 200","rate 0.015 ; DAC 400",
                                                       "rate 0.015 ; DAC 600" ,"rate 0.015 ; DAC 800",
                                                       "rate 0.025 ; DAC 50", "rate 0.025 ; DAC 100", "rate 0.025 ; DAC 200", "rate 0.025 ; DAC 400",
                                                       "rate 0.025 ; DAC 600" , "rate 0.025 ; DAC 800" ))

LateMeanArea = MeanArea[which(MeanArea$mean.Therapy != "Early therapy" ),]
EarlyMeanArea = MeanArea[which(MeanArea$mean.Therapy != "Late therapy" ),]

plE=ggplot(EarlyMeanArea,aes(x=mean.Time/24))+
  geom_ribbon(aes(ymin=lb,ymax=ub,fill= mean.DACq ),color="grey70",alpha=0.2)+
  geom_line(aes(y=mean.Value, color = mean.DACq ))+
  facet_grid(mean.Entity ~ mean.ID,scales = "free")+
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

plL=ggplot(LateMeanArea,aes(x=mean.Time/24))+
  geom_ribbon(aes(ymin=lb,ymax=ub,fill= mean.DACq ),color="grey70",alpha=0.2)+
  geom_line(aes(y=mean.Value, color = mean.DACq ))+
  facet_grid(mean.Entity ~ mean.ID,scales = "free")+
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

SubMeanArea = DatiPlot[which(DatiPlot$Entity == "ODC_le1" & DatiPlot$Time == max(DatiPlot$Time) ),]
SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
SubMeanArea$Therapy = factor(SubMeanArea$Therapy, levels = c("No therapy","Early therapy","Late therapy"))

MeanAntigene = MeanArea[which(MeanArea$mean.Entity == "Antigen"),]

colnames(MeanAntigene) = c(colnames(SubMeanArea), "ub","lb")

pl3<-ggplot()+
  geom_boxplot(data = SubMeanArea, aes(y=Value, x=DACq, fill = rate  ))+
  geom_violin(data = SubMeanArea, aes(y=Value, x=DACq, fill = rate, col=DACq  ),alpha = .4)+
  facet_wrap(Entity~ Therapy, scales = "free")+
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
        plot.margin=unit(c(0,5,5,5),"mm") )+
  geom_line(data = MeanAntigene, aes(x=Time, y=Value, col= DACq,group = ID) )

    

#SubMeanArea = DatiPlot[which(DatiPlot$Entity == "ODC_le1" & DatiPlot$Time == max(DatiPlot$Time) ),]

SubMeanArea = DatiPlot[which(DatiPlot$Entity == "ODC_le1" & DatiPlot$Time == 130*24 ),]
SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
SubMeanArea$Therapy = factor(SubMeanArea$Therapy, levels = c("No therapy","Early therapy","Late therapy"))

pl2

ggplot(SubMeanArea)+
  geom_boxplot(aes(y=Value, x=DACq, fill = rate  ))+
  geom_violin(aes(y=Value, x=DACq, fill = rate  ),alpha = .2)+
  facet_wrap(~ Therapy, scales = "free_x")+
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



ggsave(plE,filename = paste0("Plot/DiffEarlyTherapy.pdf"),device = "pdf",width = 20, height = 20 )
ggsave(plL,filename = paste0("Plot/DiffLateTherapy.pdf"),device = "pdf",width = 20, height = 20 )
ggsave(pl2,filename = paste0("Plot/DiffTherapyB.pdf"),device = "pdf",width = 16, height = 10 )
ggsave(pl3,filename = paste0("Plot/DiffTherapyCAOS.pdf"),device = "pdf",width = 16, height = 10 )

