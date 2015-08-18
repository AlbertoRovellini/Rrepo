setwd("C:/Users/Alberto/Documents/itn100results/barchartInput")
data <- read.table("extinctions.csv", header=TRUE, sep=';')
data$regime <- factor(data$regime, levels = data$regime) # keep it good for other plots

library(ggplot2)

plot <- ggplot(data=data, aes(x=regime, y=extinctions, fill=factor(intensity)))+
        geom_bar(stat="identity", aes(fill=factor(intensity)), position="dodge", width=0.8)+
        scale_x_discrete("Fishing strategy ")+
        scale_y_continuous(limits=c(0,110),
                           breaks=seq(0,100,10), 
                           expand=c(0,0), labels=seq(0,100,10), "Extinctions (in 100 runs)")+
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

plot 

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/extinctions/barchart.pdf", plot, useDingbats=FALSE)

