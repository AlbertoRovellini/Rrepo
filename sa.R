# plot the results of the extinction rate.

library(reshape)
library(ggplot2)
setwd("C:/Users/Alberto/Documents/MASTER THESIS/results/sa_results")
sa <- read.table("saExtinctionsSummary.txt", header=TRUE, sep='/t')
meltsa <- melt(sa, id.vars="Step")
p <- ggplot(subset(meltsa, variable=="Mediumpelagic" | variable=="Largepelagic" |
                        variable=="Mediumdemersal" | variable=="Largedemersal" | variable=="Topcarnivore"),
            aes(x=Step, y=value, group=variable))+
        geom_line(aes(linetype=variable))+
        geom_point(aes(shape=variable, size=2))+
        scale_x_continuous("Catch range variation [%]", breaks=seq(-20,20,5),
                           limits=c(-20,20), labels=seq(-20,20,5))+
        scale_y_continuous(name="Runs with extinctions", 
                           limits=c(0,20),
                           breaks=c(0:20), labels=c(0:20))+
        #labs(title="Community weight spectrum")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))
p

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/extinctions.pdf", p, useDingbats=FALSE)
