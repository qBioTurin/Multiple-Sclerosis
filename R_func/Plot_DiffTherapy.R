library(ggplot2)
library(cowplot)

dirs<- list.dirs()
folders <- grep("Therapy_*", dirs, value = TRUE)
#namesFol <- c( rep("Early therapy",12 ),rep("Late therapy",12 ),"No therapy","No therapy" )
namesFol <- c( rep("Early therapy",10 ),rep("Late therapy",10 ),"No therapy","No therapy" )

traceAll =  lapply( 1:length(folders), function(i)
{
  p <- readRDS(paste0("./",folders[i],"/params_MS_Model-analysys.RDS") )
  filesAll <- list.files(paste0("./",folders[i],"/"),pattern='*.trace')
 
  traceAll <- lapply(filesAll, function(x,param=p, ind = i) {
  IDsim<- as.numeric(gsub(pattern="(.trace)", gsub(pattern="([[:graph:]]+(-){1})", x=x, replacement=""), replacement="") )
    rate <- param$ini_v[10]
    param$event.list[[length(param$event.list)]][[2]]["DAC"] -> qDAC
    
    if(folders[ind] == "./Therapy_Healthy0") data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("Healthy" ), Therapy = namesFol[ind], DACq = "Healthy",rate = "Healthy" )
    else if(folders[ind] == "./Therapy_MS") data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("MS"), Therapy = namesFol[ind], DACq = "MS",rate = "MS" )
      else data.frame(read.csv(paste0('./',folders[ind],'/',x), sep="") ,ID= paste("D-P",rate,"; DAC",qDAC ), Therapy = namesFol[ind], DACq = paste(qDAC),rate =paste(rate) )
    
  } )
  
  trace<-do.call("rbind", traceAll)
})

trace<-do.call("rbind", traceAll)


time <- unique(trace$Time)
nsim <- unique(table(trace$Time))

trace$SimID = rep(1:nsim,each = length(time))

#(unique(trace$ID) )-> DACKill

#trace = trace[which(trace$ID %in% c(min(DACKill),max(DACKill))),]

Entities = c("Antigen" , "DAC" ,"Teff_out","IFNg_out", "IL17_out","Treg_out",
             "IL10_out","NK_out","BBB",
               "Teff_in" ,"IFNg_in",
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
# MeanArea$mean.DACq = factor(MeanArea$mean.DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
# MeanArea$mean.ID = factor(MeanArea$mean.ID, levels = c("Healthy","MS",
#                                                        "rate 0.015 ; DAC 50" , "rate 0.015 ; DAC 100","rate 0.015 ; DAC 200","rate 0.015 ; DAC 400",
#                                                        "rate 0.015 ; DAC 600" ,"rate 0.015 ; DAC 800",
#                                                        "rate 0.025 ; DAC 50", "rate 0.025 ; DAC 100", "rate 0.025 ; DAC 200", "rate 0.025 ; DAC 400",
#                                                        "rate 0.025 ; DAC 600" , "rate 0.025 ; DAC 800" ))
MeanArea$mean.DACq = factor(MeanArea$mean.DACq, levels = c("Healthy","MS","1000","2000","5000","10000","15000"))
MeanArea$mean.ID = factor(MeanArea$mean.ID, levels = c("Healthy","MS",
                                                       "D-P 0.01 ; DAC 1000" , "D-P 0.01 ; DAC 2000","D-P 0.01 ; DAC 5000",
                                                       "D-P 0.01 ; DAC 10000" , "D-P 0.01 ; DAC 15000",
                                                       "D-P 0.03 ; DAC 1000", "D-P 0.03 ; DAC 2000", "D-P 0.03 ; DAC 5000",
                                                       "D-P 0.03 ; DAC 10000","D-P 0.03 ; DAC 15000"))


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
        plot.margin=unit(c(0,5,5,5),"mm"),
        strip.background = element_rect(
          color="black", size=1.5, linetype="solid"
        ))+ 
  guides(colour = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))

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
        plot.margin=unit(c(0,5,5,5),"mm"),
        strip.background = element_rect(
          color="black", size=1, linetype="solid"
        ))+ 
  guides(colour = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))


source('~/Multiple-Sclerosis/R_func/Plot_boxplot.R')

#boxplot.generation(max(DatiPlot$Time) )
#boxplot.generation( 130*24 )
#boxplot.generation( 310*24 )

# ggplot(MeanArea[which(MeanArea$mean.Entity == "Antigen" ),],aes(x=mean.Time/24))+
#   geom_line(aes(y=mean.Value, color = mean.D-P, linetype = mean.DACq ))+
#   theme(axis.text=element_text(size = 15, hjust = 0.5),
#         axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
#         axis.title=element_text(size=18,face="bold"),
#         axis.line = element_line(colour="black"),
#         plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
#         legend.title = element_blank(),
#         legend.text=element_text(size=14),
#         legend.position="top",
#         legend.key=element_blank(),
#         legend.key.size = unit(.9, "cm"),
#         legend.key.width = unit(.9,"cm"),
#         panel.background = element_rect(colour = NA),
#         plot.background = element_rect(colour = NA),
#         plot.margin=unit(c(0,5,5,5),"mm"),
#         strip.background = element_rect(
#           color="black", size=1, linetype="solid"
#         ))+ 
#   guides(colour = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))




ggsave(plE,filename = paste0("Plot/DiffEarlyTherapy.pdf"),device = "pdf",width = 20, height = 20 )
ggsave(plL,filename = paste0("Plot/DiffLateTherapy.pdf"),device = "pdf",width = 20, height = 20 )

