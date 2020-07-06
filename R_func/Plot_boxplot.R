TimePoint = max(DatiPlot$Time)

SubMeanArea = DatiPlot[which(DatiPlot$Entity == "ODC irr. damaged" & DatiPlot$Time == TimePoint ),]
#SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","50","100","200","400","600","800"))
SubMeanArea$DACq = factor(SubMeanArea$DACq, levels = c("Healthy","MS","1000","2000","5000","10000","15000"))
#SubMeanArea$rate = factor(SubMeanArea$rate  , levels = c("Healthy","MS","0.015","0.025"))
SubMeanArea$Therapy = factor(SubMeanArea$Therapy, levels = c("No therapy","Early therapy","Late therapy"))

SubMeanArea$axis <- "ODC irr. damaged"

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
  
  data.frame(Value=AUC_V, ID = unique(A$ID), Therapy = unique(A$Therapy), DACq = unique(A$DACq) , rate = unique(A$rate) )
})

AUC <- do.call("rbind", AUClist)
AUC$DACq = factor(AUC$DACq, levels = c("Healthy","MS","1000","2000","5000","10000","15000"))
AUC$Therapy = factor(AUC$Therapy, levels = c("No therapy","Early therapy","Late therapy"))

AUC$axis <- "Antigen"
AUC$Value <- AUC$Value / 1890
data <- rbind(AUC,SubMeanArea[,-c(1,5)])

mycolors <- c("ODC irr. damaged"="blue", "Antigen"="red")

data <- data[which(data$Therapy != "No therapy"),]

pl<-ggplot(data)+
  geom_boxplot(aes(y=Value, x=DACq, fill = rate,col = axis ),size = .6)+
  facet_wrap(~ Therapy, scales = "free_x")+
  scale_y_continuous(name="ODC irreversibly \ndamaged in 2 year", 
                     sec.axis = sec_axis(~ 1890*., name="Antigen quantity in 2 year\n")) +
  scale_color_manual(name="y-axis: ", values = mycolors) +
  theme(
    axis.title.y = element_text(color = mycolors["ODC irr. damaged"]),
    axis.text.y = element_text(color = mycolors["ODC irr. damaged"]),
    axis.title.y.right = element_text(color = mycolors["Antigen"]),
    axis.text.y.right = element_text(color = mycolors["Antigen"]),
    axis.text=element_text(size = 15, hjust = 0.5),
    axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
    axis.title=element_text(size=18,face="bold"),
    axis.line = element_line(colour="black"),
    plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
    legend.text=element_text(size=18),
    legend.title =element_text(size=20,face = "bold.italic"),
    legend.position="bottom",
    legend.key=element_blank(),
    legend.key.size = unit(.9, "cm"),
    legend.key.width = unit(.9,"cm"),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    strip.text = element_text(size = 16),
    plot.margin=unit(c(0,5,5,5),"mm"),
    strip.background = element_rect(
      color="black", size=1, linetype="solid",fill = "white"        ))+
  scale_fill_manual("DP: ", values = c("#CCCCCC","#666666"), limits = c("0.01","0.03"),
                    breaks = c("0.01","0.03"))+
  labs(x = "DAC quantity")


ggsave(pl,filename = "Plot/Boxplot.pdf",device = "pdf",width = 12, height = 8 )
