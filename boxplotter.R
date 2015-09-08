# boxplotter(plusothershitter). 12/08/2015 AR SPACE PROGRAM AYY LMAO

library("ggplot2")
setwd("C:/Users/Alberto/Documents/itn100results/input/boxplotInput")

resultsBase <- read.table("resultsBase.csv", header=TRUE, sep=' ', dec='.') # mind the sep plz
unselective_i1 <- read.table("unselective_i1.csv", header=TRUE, sep=' ', dec='.')
unselective_i2 <- read.table("unselective_i2.csv", header=TRUE, sep=' ', dec='.')
unselective_i3 <- read.table("unselective_i3.csv", header=TRUE, sep=' ', dec='.')
size500_i1 <- read.table("size500_i1.csv", header=TRUE, sep=' ', dec='.')
size500_i2 <- read.table("size500_i2.csv", header=TRUE, sep=' ', dec='.')
size500_i3 <- read.table("size500_i3.csv", header=TRUE, sep=' ', dec='.')
class_i1 <- read.table("class_i1.csv", header=TRUE, sep=' ', dec='.')
class_i2 <- read.table("class_i2.csv", header=TRUE, sep=' ', dec='.')
class_i3 <- read.table("class_i3.csv", header=TRUE, sep=' ', dec='.')
mixed_i1 <- read.table("mixed_i1.csv", header=TRUE, sep=' ', dec='.')
mixed_i2 <- read.table("mixed_i2.csv", header=TRUE, sep=' ', dec='.')
mixed_i3 <- read.table("mixed_i3.csv", header=TRUE, sep=' ', dec='.')

#extra scenarios
size250_i1 <- read.table("size250_i1.csv", header=TRUE, sep=' ', dec='.')
size250_i2 <- read.table("size250_i2.csv", header=TRUE, sep=' ', dec='.')
size250_i3 <- read.table("size250_i3.csv", header=TRUE, sep=' ', dec='.')
mixed250_i1 <- read.table("mixed250_i1.csv", header=TRUE, sep=' ', dec='.')
mixed250_i2 <- read.table("mixed250_i2.csv", header=TRUE, sep=' ', dec='.')
mixed250_i3 <- read.table("mixed250_i3.csv", header=TRUE, sep=' ', dec='.')


# add the id of the scenario, can (should) be looped or plyr'd

resultsBase$Scen <- rep("Base", nrow(resultsBase))
unselective_i1$Scen <- rep("U_I1", nrow(unselective_i1))
unselective_i2$Scen <- rep("U_I2", nrow(unselective_i2))
unselective_i3$Scen <- rep("U_I3", nrow(unselective_i3))
size500_i1$Scen <- rep("S500_I1", nrow(size500_i1))
size500_i2$Scen <- rep("S500_I2", nrow(size500_i2))
size500_i3$Scen <- rep("S500_I3", nrow(size500_i3))
class_i1$Scen <- rep("C_I1", nrow(class_i1))
class_i2$Scen <- rep("C_I2", nrow(class_i2))
class_i3$Scen <- rep("C_I3", nrow(class_i3))
mixed_i1$Scen <- rep("M500_I1", nrow(mixed_i1))
mixed_i2$Scen <- rep("M500_I2", nrow(mixed_i2))
mixed_i3$Scen <- rep("M500_I3", nrow(mixed_i3))

#extra scenarios

size250_i1$Scen <- rep("S250_I1", nrow(size250_i1))
size250_i2$Scen <- rep("S250_I2", nrow(size250_i2))
size250_i3$Scen <- rep("S250_I3", nrow(size250_i3))
mixed250_i1$Scen <- rep("M250_I1", nrow(mixed250_i1))
mixed250_i2$Scen <- rep("M250_I2", nrow(mixed250_i2))
mixed250_i3$Scen <- rep("M250_I3", nrow(mixed250_i3))

#color of the box

resultsBase$Group <- "Base"
unselective_i1$Group <- "Unselective"
unselective_i2$Group <- "Unselective"
unselective_i3$Group <- "Unselective"
size500_i1$Group <- "Size (500g)"
size500_i2$Group <- "Size (500g)"
size500_i3$Group <- "Size (500g)"
class_i1$Group <- "Class"
class_i2$Group <- "Class"
class_i3$Group <- "Class"
mixed_i1$Group <- "Size and class (500g)"
mixed_i2$Group <- "Size and class (500g)"
mixed_i3$Group <- "Size and class (500g)"

# extra scenario

size250_i1$Group <- "Size (250g)"
size250_i2$Group <- "Size (250g)"
size250_i3$Group <- "Size (250g)"
mixed250_i1$Group <- "Size and class (250g)"
mixed250_i2$Group <- "Size and class (250g)"
mixed250_i3$Group <- "Size and class (250g)"

unselective <- rbind(resultsBase, unselective_i1, unselective_i2, unselective_i3,
                     size500_i1, size500_i2, size500_i3,
                     class_i1, class_i2, class_i3,
                     mixed_i1, mixed_i2, mixed_i3,
                     size250_i1, size250_i2, size250_i3,
                     mixed250_i1, mixed250_i2, mixed250_i3)
unselective$index <- c(rep(1, nrow(unselective[unselective$Group=="Base",])),
                       rep(2, nrow(unselective[unselective$Group=="Unselective",])),
                       rep(3, nrow(unselective[unselective$Group=="Size (500g)",])),
                       rep(4, nrow(unselective[unselective$Group=="Class",])),
                       rep(5, nrow(unselective[unselective$Group=="Size and class (500g)",])),
                       rep(6, nrow(unselective[unselective$Group=="Size (250g)",])), 
                       rep(7, nrow(unselective[unselective$Group=="Size and class (250g)",]))) 
unselective <- unselective[order(unselective$index),]
unselective$Scen <- factor(unselective$Scen, levels = unique(unselective$Scen)) # keep it good for other plots


# boxplot generic all the scenarios together, good representation, then scale down to interesting
# things (implying there are interesting things)

box <- ggplot(unselective, aes(x=Scen, y=x, color=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           breaks=seq(1,2.7, .1))+
        # the order of the colors here is manual to prevent ggplot from messing up
        scale_color_manual(name="Selectivity regime",
                          values=c("#377EB8","#FF7F00","#999999","#4DAF4A",
                                   "#F781BF","#984EA3","#E41A1C"), 
                          breaks=c("Base","Unselective","Size (500g)", "Class", "Size and class (500g)",
                                   "Size (250g)", "Size and class (250g)"))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5))+
        geom_boxplot(outlier.shape = 1)
        
box

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxplot_tot.pdf", box, useDingbats=FALSE)

##### here will be all the supposedly relevant plots for comparisons #####
# for instance, base versus the lowest I of all the approahes, then base vs all the Is of
# of one approach and so on

#########################################################################################

boxI1 <- ggplot(subset(unselective, Scen=="Base" | Scen=="U_I1" | Scen=="S500_I1" |
                               Scen=="C_I1" | Scen=="M500_I1" | Scen=="S250_I1" |
                               Scen=="M250_I1"),
                aes(x=Scen, y=x, color=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        scale_color_manual(name="Selectivity regime",
                           values=c("#377EB8","#FF7F00","#999999","#4DAF4A",
                                    "#F781BF","#984EA3","#E41A1C"), 
                           breaks=c("Base","Unselective","Size (500g)", "Class", "Size and class (500g)",
                                    "Size (250g)", "Size and class (250g)"))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)
boxI1

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxI1.pdf", boxI1, useDingbats=FALSE)


#########################################################################################

boxI2 <- ggplot(subset(unselective, Scen=="Base" | Scen=="U_I2" | Scen=="S500_I2" |
                               Scen=="C_I2" | Scen=="M500_I2" | Scen=="S250_I2" |
                               Scen=="M250_I2"),
                aes(x=Scen, y=x, color=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        scale_color_manual(name="Selectivity regime",
                           values=c("#377EB8","#FF7F00","#999999","#4DAF4A",
                                    "#F781BF","#984EA3","#E41A1C"), 
                           breaks=c("Base","Unselective","Size (500g)", "Class", "Size and class (500g)",
                                    "Size (250g)", "Size and class (250g)"))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)

boxI2

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxI2.pdf", boxI2, useDingbats=FALSE)


#########################################################################################


boxI3 <- ggplot(subset(unselective, Scen=="Base" | Scen=="U_I3" | Scen=="S500_I3" |
                               Scen=="C_I3" | Scen=="M500_I3" | Scen=="S250_I3" |
                               Scen=="M250_I3"),
                aes(x=Scen, y=x, color=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        scale_color_manual(name="Selectivity regime",
                           values=c("#377EB8","#FF7F00","#999999","#4DAF4A",
                                    "#F781BF","#984EA3","#E41A1C"), 
                           breaks=c("Base","Unselective","Size (500g)", "Class", "Size and class (500g)",
                                    "Size (250g)", "Size and class (250g)"))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)
boxI3

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxI3.pdf", boxI3, useDingbats=FALSE)


#########################################################################################

boxUvsM <- ggplot(subset(unselective, Scen=="U_I1" | Scen=="U_I2" | Scen=="U_I3" |
                               Scen=="M500_I1" | Scen=="M500_I2" |Scen=="M500_I3"),
                aes(x=Scen, y=x, fill=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        scale_fill_manual(values=c("white", "grey"))+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)
boxUvsM

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxUvsM.pdf", boxUvsM, useDingbats=FALSE)

#########################################################################################

boxS500vsS250 <- ggplot(subset(unselective, Scen=="S500_I1" | Scen=="S500_I2" | Scen=="S500_I3" |
                                 Scen=="S250_I1" | Scen=="S250_I2" |Scen=="S250_I3"),
                  aes(x=Scen, y=x, fill=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        scale_fill_manual(values=c("white", "grey"))+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)

boxS500vsS250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxS500vsS250.pdf", boxS500vsS250, useDingbats=FALSE)

#########################################################################################

boxM500vsM250 <- ggplot(subset(unselective, Scen=="M500_I1" | Scen=="M500_I2" | Scen=="M500_I3" |
                                       Scen=="M250_I1" | Scen=="M250_I2" |Scen=="M250_I3"),
                        aes(x=Scen, y=x, fill=Group, factor=Scen))+
        geom_boxplot(outlier.shape = 1)+
        scale_fill_manual(values=c("white", "grey"))+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_discrete(name="Scenario")+
        scale_y_continuous(name="Evenness index",
                           limits=c(1.0,2.7),
                           breaks=seq(1,2.7, .1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12, angle=45, hjust=1))+
        theme(axis.text.y=element_text(size=12))+
        stat_boxplot(geom = "errorbar", stat_params = list(width = 0.2))+
        geom_boxplot(outlier.shape = 1)

boxM500vsM250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/boxM500vsM250.pdf", boxM500vsM250, useDingbats=FALSE)


#########################################################################################


#########################################################################################


#########################################################################################

# global density curve

dens <- ggplot(unselective, aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        guides(colour=guide_legend(ncol=2))+
        scale_x_continuous(name="Evenness index",
                           breaks=seq(1,2.7,.1))+
        scale_y_continuous(name="Density",
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))
dens

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/density_tot.pdf", dens, useDingbats=FALSE)

#########################################################################################

densI1 <- ggplot(subset(unselective, Scen=="Base" | Scen=="U_I1" | Scen=="S500_I1" |
                                Scen=="C_I1" | Scen=="M500_I1"),
                 aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_continuous(name="Evenness index",
                           limits=c(1.0,2.8),
                           breaks=seq(1,2.8,.2))+
        scale_y_continuous(name="Density",
                           limit=c(0,7),
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))
densI1

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/densityI1.pdf", densI1, useDingbats=FALSE)

#########################################################################################

densI3 <- ggplot(subset(unselective, Scen=="Base" | Scen=="U_I3" | Scen=="S500_I3" |
                                Scen=="C_I3" | Scen=="M500_I3"),
                 aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_continuous(name="Evenness index",
                           limits=c(1.0,2.8),
                           breaks=seq(1,2.8,.2))+
        scale_y_continuous(name="Density",
                           limit=c(0,7),
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))
              
densI3
              
ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/densityI3.pdf", densI3, useDingbats=FALSE)
              

#########################################################################################

densUvsM <- ggplot(subset(unselective, Scen=="U_I1" | Scen=="U_I2" | Scen=="U_I3" |
                                Scen=="M500_I1" | Scen=="M500_I2" |Scen=="M500_I3"),
                 aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_continuous(name="Evenness index",
                           limits=c(1.0,2.8),
                           breaks=seq(1,2.8,.2))+
        scale_y_continuous(name="Density",
                           limit=c(0,7),
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

densUvsM

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/densityUvsM.pdf", densUvsM, useDingbats=FALSE)

#########################################################################################

densS500vsS250 <- ggplot(subset(unselective, Scen=="S500_I1" | Scen=="S500_I2" | Scen=="S500_I3" |
                                         Scen=="S250_I1" | Scen=="S250_I2" |Scen=="S250_I3"),
                   aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_continuous(name="Evenness index",
                           limits=c(1.0,2.8),
                           breaks=seq(1,2.8,.2))+
        scale_y_continuous(name="Density",
                           limit=c(0,7),
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

densS500vsS250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/densityS500vsS250.pdf", densS500vsS250, useDingbats=FALSE)

#########################################################################################

densM500vsM250 <- ggplot(subset(unselective, Scen=="M500_I1" | Scen=="M500_I2" | Scen=="M500_I3" |
                                        Scen=="M250_I1" | Scen=="M250_I2" |Scen=="M250_I3"),
                         aes(x=x, colour=Scen))+
        geom_density()+
        theme(panel.background = element_rect(fill = 'white'))+
        scale_x_continuous(name="Evenness index",
                           limits=c(1.0,2.8),
                           breaks=seq(1,2.8,.2))+
        scale_y_continuous(name="Density",
                           limit=c(0,7),
                           breaks=seq(1,7,1))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

densM500vsM250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/evennessIndex/densityM500vsM250.pdf", densM500vsM250, useDingbats=FALSE)


#########################################################################################
