library(ggplot2)
library(reshape)
setwd("C:/Users/Alberto/Documents/itn100results/input/finalInput")
data <- read.table("biomass.csv", header=TRUE, sep=';', dec='.')
colnames(data) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
data$Regime <- as.character(data$Regime)
#Then turn it back into an ordered factor
data$Regime <- factor(data$Regime, levels=unique(data$Regime))
meanValues <- data[,c(1:9)]
sdValues <- data[,c(1,10:17)]

meltmeanValues <- melt(meanValues, id.vars="Regime")
meltsdValues <- melt(sdValues, id.vars="Regime")
meltmeanValues$sd <- meltsdValues$value
meltAllData <- meltmeanValues

# limits for the errorbars, sd

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Set1")

# new try

dodge <- position_dodge(0.9)

newPlot <- ggplot(data=meltAllDataMod, aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                          labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                   "Medium demersal", "Large demersals", "Top carnivores"))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))
        

newPlot # hahaha nailed it kek

# by intensity

meltAllDataMod <- subset(meltAllData, variable!="Total")
meltAllDataMod$intensity <- rep(c("0", rep(c("1","2","3"),6)), 7)

# 10%

int10 <- ggplot(data=subset(meltAllDataMod, intensity=="0" | intensity=="1"), 
                 aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                          labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                   "Medium demersal", "Large demersals", "Top carnivores"))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


int10 

# 20%

int20 <- ggplot(data=subset(meltAllDataMod, intensity=="0" | intensity=="2"), 
                aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                          labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                   "Medium demersal", "Large demersals", "Top carnivores"))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


int20 

# 50%

int50 <- ggplot(data=subset(meltAllDataMod, intensity=="0" | intensity=="3"), 
                aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                          labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                   "Medium demersal", "Large demersals", "Top carnivores"))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


int50 

############################### total analysis because why not #############################

meltAllDataTot <- subset(meltAllData, variable=="Total")
meltAllDataTot$intensity <- c("0", rep(c("1","2","3"),6))

totPlot <- ggplot(data=meltAllDataTot, aes(x=Regime, y=value, fill=intensity, colour="black"))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,70),
                           breaks=seq(0,70,5), 
                           expand=c(0,0), labels=seq(0,70,5), "Community biomass [t]")+
        scale_fill_manual(name="Intensity",
                          values=c("white", "lightgrey", "darkgrey","black"),
                          labels=c("Unfished", "10%", "20%", "50%"))+
        scale_colour_manual(name="lmao",
                          values=rep("black",4))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


totPlot 

# tot by intensity

# 10%

totPlot10 <- ggplot(data=subset(meltAllDataTot, intensity=="0" | intensity=="1"),
                  aes(x=Regime, y=value, fill=intensity, colour="black"))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,70),
                           breaks=seq(0,70,5), 
                           expand=c(0,0), labels=seq(0,70,5), "Community biomass [t]")+
        scale_fill_manual(name="Intensity",
                          values=c("white", "lightgrey", "darkgrey","black"),
                          labels=c("Unfished", "10%", "20%", "50%"))+
        scale_colour_manual(name="lmao",
                            values=rep("black",4))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


totPlot10

# 20%

totPlot20 <- ggplot(data=subset(meltAllDataTot, intensity=="0" | intensity=="2"),
                   aes(x=Regime, y=value, fill=intensity, colour="black"))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,70),
                           breaks=seq(0,70,5), 
                           expand=c(0,0), labels=seq(0,70,5), "Community biomass [t]")+
        scale_fill_manual(name="Intensity",
                          values=c("white", "lightgrey", "darkgrey","black"),
                          labels=c("Unfished", "10%", "20%", "50%"))+
        scale_colour_manual(name="lmao",
                            values=rep("black",4))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


totPlot20

# 50%

totPlot50 <- ggplot(data=subset(meltAllDataTot, intensity=="0" | intensity=="3"),
                   aes(x=Regime, y=value, fill=intensity, colour="black"))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,70),
                           breaks=seq(0,70,5), 
                           expand=c(0,0), labels=seq(0,70,5), "Community biomass [t]")+
        scale_fill_manual(name="Intensity",
                          values=c("white", "lightgrey", "darkgrey","black"),
                          labels=c("Unfished", "10%", "20%", "50%"))+
        scale_colour_manual(name="lmao",
                            values=rep("black",4))+
        
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


totPlot50



