
boxplot.generation<-function(TimePoint){
  SubMeanArea = DatiPlot[which(DatiPlot$Entity == "ODC_le1" & DatiPlot$Time == TimePoint ),]
  #SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
  SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","1000","2000","5000","10000","15000"))
  #SubMeanArea$rate = factor(SubMeanArea$rate  , levels = c("Healthy","MS","0.015","0.025"))
  SubMeanArea$Therapy = factor(SubMeanArea$Therapy, levels = c("No therapy","Early therapy","Late therapy"))
  
  SubMeanArea$Therapy
  
  pl<- ggplot(SubMeanArea)+
    geom_boxplot(aes(y=Value, x=DACq, fill = rate  ))+
    #geom_violin(aes(y=Value, x=DACq, fill = rate  ),alpha = .2)+
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
          plot.margin=unit(c(0,5,5,5),"mm") )+
    scale_fill_brewer(palette="Spectral", limits = c("MS","0.01","0.03","Healthy"),breaks = c("Healthy","MS","0.01","0.03"))
  
  ggsave(pl,filename = paste0("Plot/DiffTherapyBoxplot_T",TimePoint,".pdf"),device = "pdf",width = 16, height = 10 )
  
  return(pl)
  
}

####### Area  BOX Plot
library(zoo)

Antigen<-DatiPlot[which(DatiPlot$Entity == "Antigen" ),]

n <- unique(table(Antigen$Time))

AUClist<-lapply(1:n,function(s){
  A <- Antigen[(s + (s-1)*(length(time)-1)) :(s+(s)*(length(time)-1) ),] 
  yV <- A$Value
  yU <- A$ub
  yL <- A$lb
  
  AUC_V <- sum(diff(time)*rollmean(yV,2))
  AUC_L <- sum(diff(time)*rollmean(yL,2))
  AUC_U <- sum(diff(time)*rollmean(yU,2))
  
  data.frame(AUC_V=AUC_V,AUC_U=AUC_U,AUC_L=AUC_L, ID = unique(A$ID), Therapy = unique(A$Therapy), DACq = unique(A$DACq) , rate = unique(A$rate), IdSim = paste(s) )
})


AUC <- do.call("rbind", AUClist)

#AUC$DACq = factor(AUC$DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
AUC$DACq = factor(AUC$DACq, levels = c("Healthy","MS","1000","2000","5000","10000","15000"))
AUC$Therapy = factor(AUC$Therapy, levels = c("No therapy","Early therapy","Late therapy"))

pl2 <-ggplot(AUC)+
  geom_boxplot(aes(y=AUC_V, x=DACq, fill = rate  ))+
  facet_wrap(~ Therapy, scales = "free_x")+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position="bottom",
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"),
        strip.background = element_rect(
          color="black", size=1, linetype="solid",fill = "white"
        ))+
  scale_fill_brewer(palette="Spectral", limits = c("MS","0.01","0.03","Healthy"),breaks = c("Healthy","MS","0.01","0.03"))+
  labs(x = "DAC quantity", y = "Antigen quantity in 2 year")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

pl1 <- boxplot.generation(max(DatiPlot$Time) ) +
  labs(x = "", y = "ODC irreversibly \ndamaged in 2 year")+
  theme(legend.position = "none",
        strip.background = element_rect(
          color="black", size=1, linetype="solid",fill = "white"
        )) 

pl<-  plot_grid(plotlist = list(pl1,pl2),ncol = 1,labels = c("A)","B)"))

ggsave(pl,filename = "Plot/Boxplot.pdf",device = "pdf",width = 16, height = 14 )
