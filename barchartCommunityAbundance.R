library(ggplot2)
library(reshape)
setwd("C:/Users/Alberto/Documents/itn100results/input/finalInput")
data <- read.table("abundance.csv", header=TRUE, sep=';', dec='.')
colnames(data) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
#Turn your 'treatment' column into a character vector
data$Regime <- as.character(data$Regime)
#Then turn it back into an ordered factor
data$Regime <- factor(data$Regime, levels=unique(data$Regime))
meltAllData <- melt(data, id.vars="Regime")


library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Set1")


plot <- ggplot(subset(meltAllData, variable=="Smallpelagic" | variable=="Mediumpelagic" | 
                              variable=="Largepelagic"| variable=="Smalldemersal"| 
                              variable=="Mediumdemersal"| variable=="Largedemersal" | 
                              variable=="Toppiscivores"),
               aes(x=Regime, y=value, fill=variable))+
        geom_bar(stat="identity", position="stack", aes(fill=variable), width=0.8)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,300000),
                           breaks=seq(0,300000,50000), 
                           expand=c(0,0), labels=seq(0,300000,50000), "Groups abundance [t]")+
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

plot # meh it's fine. need dev doe.
