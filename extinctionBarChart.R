setwd("C:/Users/Alberto/Documents/itn100results/input/barchartInput")
data <- read.table("extinctions.csv", header=TRUE, sep=';')
data$regime <- factor(data$regime, levels = data$regime) # keep it good for other plots

library(ggplot2)

collapsePlot <- ggplot(data=data, aes(x=regime, y=extinctions, fill=intensity, colour="black"))+
        geom_bar(stat="identity", aes(fill=factor(intensity)), position="dodge")+
        scale_x_discrete("Fishing strategy ")+
        scale_y_continuous(limits=c(0,110),
                           breaks=seq(0,100,10), 
                           expand=c(0,0), labels=seq(0,100,10), "Runs with collapse(s)/100 runs")+
        scale_fill_manual(values=c("white", "grey", "black"),
                          name="Intensity", labels=c("10%","20%","50%"))+
        scale_colour_manual(values=rep("black", 3))+
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))

collapsePlot 

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/collapseBarchart.pdf", collapsePlot, useDingbats=FALSE ) # set better res pls

