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

meltAllDataMod <- subset(meltAllData, variable!="Total")
meltAllDataMod$intensity <- rep(c("0", rep(c("1","2","3"),6)), 7)
cleanData <- meltAllDataMod[rev(rownames(meltAllDataMod)),] # revert order of rows
rownames(cleanData)<-1:nrow(cleanData)
cleanData[,1] = factor(cleanData[,1],levels(cleanData[,1])[c(length(cleanData[,1]):1)])


newPlot <- ggplot(data=cleanData, aes(x=Regime, y=value, fill="black"))+
        geom_bar(position=dodge, stat="identity", width=.4)+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,40),
                           breaks=seq(0,40,5), 
                           expand=c(0,0), labels=seq(0,40,5), "Community biomass [t]")+
        scale_fill_manual(values=rep("black", 7))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=6,vjust=0.5),
              axis.title.y = element_text(size=6,vjust=0.5))+
        theme(legend.title = element_text(size=6))+
        theme(axis.text.x=element_text(size=6,vjust=0.5))+
        theme(axis.text.y=element_text(size=6))+
        facet_grid(variable ~ ., scales="free")


newPlot 

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/horizontalBiomass.pdf", newPlot, 
       useDingbats=FALSE, units="cm", height = 24, width=16) # set better res pls

# please draw two separate facets because this one is correct but not informative

# smaller fish larger scale

smallFish <- ggplot(data=subset(cleanData, variable=="Smallpelagic" | variable=="Smalldemersal"), 
                  aes(x=Regime, y=value))+
        geom_bar(position=dodge, stat="identity", width=.5)+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,40),
                           breaks=seq(0,40,5), 
                           expand=c(0,0), labels=seq(0,40,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("black", "black"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))+
        facet_grid(. ~ variable)


smallFish 

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/horizontalSmall.pdf", smallFish, useDingbats=FALSE ) # set better res pls

# medium fish

mediumFish <- ggplot(data=subset(cleanData, variable=="Mediumpelagic" | variable=="Mediumdemersal"), 
                    aes(x=Regime, y=value))+
        geom_bar(position=dodge, stat="identity", width=.5)+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,10),
                           breaks=seq(0,10,2), 
                           expand=c(0,0), labels=seq(0,10,2), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("black", "black"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))+
        facet_grid(. ~ variable)


mediumFish 

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/horizontalMedium.pdf", mediumFish, useDingbats=FALSE ) # set better res pls

# larger fish smaller scale

largeFish <- ggplot(data=subset(cleanData, variable=="Largepelagic" | variable=="Largedemersal" |
                                        variable=="Toppiscivores"), 
                    aes(x=Regime, y=value))+
        geom_bar(position=dodge, stat="identity", width=.5)+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,10),
                           breaks=seq(0,10,2), 
                           expand=c(0,0), labels=seq(0,10,2), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=rep("black", 3))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))+
        facet_grid(. ~ variable)


largeFish 

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/horizontalLarge.pdf", largeFish, useDingbats=FALSE ) # set better res pls


# small pelagic

sp <- ggplot(data=subset(cleanData, variable=="Smallpelagic"),
                  aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#377EB8"),
                          labels=c("Small pelagic"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


sp

# medium pelagic

mp <- ggplot(data=subset(cleanData, variable=="Mediumpelagic"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#E41A1C"),
                          labels=c("Medium pelagic"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


mp

# large pelagic

lp <- ggplot(data=subset(cleanData, variable=="Largepelagic"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#4DAF4A"),
                          labels=c("Large pelagic"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


lp

# small demersal

sd <- ggplot(data=subset(cleanData, variable=="Smalldemersal"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#FF7F00"),
                          labels=c("Small demersal"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


sd

# medium demersal

md <- ggplot(data=subset(cleanData, variable=="Mediumdemersal"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#984EA3"),
                          labels=c("Medium demersal"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


md

# large demersal

ld <- ggplot(data=subset(cleanData, variable=="Largedemersal"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#999999"),
                          labels=c("Large demersals"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


ld

# top predators

tp <- ggplot(data=subset(cleanData, variable=="Toppiscivores"),
             aes(x=Regime, y=value, fill=variable))+
        geom_bar(position=dodge, stat="identity")+
        geom_errorbar(aes(ymin=value,ymax=value+sd),
                      position=dodge, width=0.1, size=0.3)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,50),
                           breaks=seq(0,50,5), 
                           expand=c(0,0), labels=seq(0,50,5), "Community biomass [t]")+
        scale_fill_manual(name="Functional groups",
                          values=c("#F781BF"),
                          labels=c("Top carnivores"))+
        
        coord_flip()+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12, vjust=0.5))+
        theme(axis.text.y=element_text(size=12))


tp






