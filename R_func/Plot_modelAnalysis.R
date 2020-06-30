library(cowplot)
library(ggplot2)
library(ggthemes)

#trace <-read.csv('./results_model_calibration/Rete_SM_newAM_SR_Laura-calibration-22340.trace', sep="")
traceH <-data.frame(read.csv('./Healthy_model_analysis/MS_Model-analysys-1.trace', sep=""), patient = "Healthy")
if(file.exists("MS_model_analysis"))
{
  traceS <-data.frame(read.csv('./MS_model_analysis/MS_Model-analysys-1.trace', sep=""), patient = "MS")
  trace <-rbind(traceH,traceS)
}else{trace <- traceH}


Entities = c("Teff_out","Treg_out","Antigen" ,
             "EffectorMemory","NK_out","BBB",
             "IL10_out","IL17_out","IFNg_out", 
             "Treg_in", "Teff_in","ODC_le1" ,
             "IL17_in","IFNg_in","IL10_in")



refH <- data.frame(Entity = Entities, ref = c(50,13,NaN,NaN,NaN,NaN , 13,8,42, 1,1,NaN,0.7,0.6,1), ID = "Healthy" , Time = c(rep(2+18/24,9),rep(4+18/24,6)) )
refS <- data.frame(Entity = Entities, ref = c(142,3,NaN,NaN,NaN,NaN , 3,25,117,1.1, 15, NaN,2,13,0), ID = "MS" , Time = c(rep(2+18/24,9),rep(4+18/24,6)) )

ref = rbind(refH,refS)

DatiPlotlist<- lapply(1:length(Entities),function(i){
  place<-Entities[i]
  
  sub_dati=trace[,c("Time",place)]
  
  dt=data.frame(sub_dati,ID=trace$patient,Ent=place)
  colnames(dt)=c("Time","Value","ID","Entity")
  return(dt)
})

DatiPlot<-do.call("rbind", DatiPlotlist)


pl<-ggplot(DatiPlot[DatiPlot$Entity %in% Entities,],aes(x=Time/24,y=Value,group=ID,col=ID))+
  geom_line()+
  facet_wrap(~ Entity,scales = "free",ncol = 3)+
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
  geom_point(aes(x=Time, y = ref,col=ID), ref)+
  scale_colour_manual(values = c("MS"="red","Healthy"="blue") )



ggsave(pl,filename = "Plot/Dynamics.pdf",device = "pdf",width = 16, height = 20 )


DataExcel <- read_table2("input/DataExcel.csv")

ref <-lapply(2:(length(DataExcel[1,])), function(i){
  if(names(DataExcel[,i]) %in% c("IFNg_out", "IL17_out" ,"IL10_out", "Teff_out","Treg_out") ) t = 2+18/24
  else t = 4+18/24
  a<- data.frame(DataExcel[,c(1,i)], Entity = names(DataExcel[,i]), time = t )
  colnames(a) <- c("ID", "ref" ,"Entity","Time")
  return(a)
})

ref <- do.call("rbind",ref)

sub_entity = unique(ref$Entity)

pl1<-ggplot()+
  geom_line(data = DatiPlot[DatiPlot$Entity %in% sub_entity,],aes(x=Time/24,y=Value,group=ID,col=ID))+
  geom_vline(data = ref, aes( xintercept = Time), linetype = "longdash", col = "gray" )+
  geom_boxplot(data = ref, aes(x = Time, y = ref,fill=ID, col= ID ),alpha=.5)+
  facet_wrap(~ Entity,scales = "free",ncol = 3)+
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
  scale_colour_manual(values = c("MS"="red","Healthy"="blue") )+
scale_fill_manual("",values = c("MS"="red","Healthy"="blue"), labels = c("Healthy Data","MS Data") )

ggsave(pl1,filename = paste0("Plot/DynmamicsViolin.pdf"),device = "pdf",width = 16, height = 20 )
