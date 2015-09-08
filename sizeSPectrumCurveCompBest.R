# let's try for once not to write a retarded script

library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput")
parameters <- read.table("parametersLin.csv", header=TRUE, sep=';', dec='.', row.names=1)[,c(1,2)]
x <- seq(3,15,length=1000)

# even more retarded, congrats

Base <- function(x){parameters[1,1]*x+parameters[1,2]}
U_I1 <- function(x){parameters[2,1]*x+parameters[2,2]}
U_I2 <- function(x){parameters[3,1]*x+parameters[3,2]}
U_I3 <- function(x){parameters[4,1]*x+parameters[4,2]}
S500_I1 <- function(x){parameters[5,1]*x+parameters[5,2]}
S500_I2 <- function(x){parameters[6,1]*x+parameters[6,2]}
S500_I3 <- function(x){parameters[7,1]*x+parameters[7,2]}
C_I1 <- function(x){parameters[8,1]*x+parameters[8,2]}
C_I2 <- function(x){parameters[9,1]*x+parameters[9,2]}
C_I3 <- function(x){parameters[10,1]*x+parameters[10,2]}
M500_I1 <- function(x){parameters[11,1]*x+parameters[11,2]}
M500_I2 <- function(x){parameters[12,1]*x+parameters[12,2]}
M500_I3 <- function(x){parameters[13,1]*x+parameters[13,2]}
S250_I1 <- function(x){parameters[14,1]*x+parameters[14,2]}
S250_I2 <- function(x){parameters[15,1]*x+parameters[15,2]}
S250_I3 <- function(x){parameters[16,1]*x+parameters[16,2]}
M250_I1 <- function(x){parameters[17,1]*x+parameters[17,2]}
M250_I2 <- function(x){parameters[18,1]*x+parameters[18,2]}
M250_I3 <- function(x){parameters[19,1]*x+parameters[19,2]}

Base <- data.frame(x, Base(x), rep("Base", length(x)))
U_I1 <- data.frame(x, U_I1(x), rep("U_I1", length(x)))
U_I2 <- data.frame(x, U_I2(x), rep("U_I2", length(x)))
U_I3 <- data.frame(x, U_I3(x), rep("U_I3", length(x)))
S500_I1 <- data.frame(x, S500_I1(x), rep("S500_I1", length(x)))
S500_I2 <- data.frame(x, S500_I2(x), rep("S500_I2", length(x)))
S500_I3 <- data.frame(x, S500_I3(x), rep("S500_I3", length(x)))
C_I1 <- data.frame(x, C_I1(x), rep("C_I1", length(x)))
C_I2 <- data.frame(x, C_I2(x), rep("C_I2", length(x)))
C_I3 <- data.frame(x, C_I3(x), rep("C_I3", length(x)))
M500_I1 <- data.frame(x, M500_I1(x), rep("M500_I1", length(x)))
M500_I2 <- data.frame(x, M500_I2(x), rep("M500_I2", length(x)))
M500_I3 <- data.frame(x, M500_I3(x), rep("M500_I3", length(x)))
S250_I1 <- data.frame(x, S250_I1(x), rep("S250_I1", length(x)))
S250_I2 <- data.frame(x, S250_I2(x), rep("S250_I2", length(x)))
S250_I3 <- data.frame(x, S250_I3(x), rep("S250_I3", length(x)))
M250_I1 <- data.frame(x, M250_I1(x), rep("M250_I1", length(x)))
M250_I2 <- data.frame(x, M250_I2(x), rep("M250_I2", length(x)))
M250_I3 <- data.frame(x, M250_I3(x), rep("M250_I3", length(x)))

listRegimes <- list(Base, U_I1, U_I2, U_I3, S500_I1, S500_I2, S500_I3, C_I1, C_I2, C_I3, M500_I1,
                    M500_I2, M500_I3, S250_I1, S250_I2, S250_I3, M250_I1, M250_I2, M250_I3)
listRegimes <- lapply(listRegimes, function(x) {
        colnames(x)<-c("x","y","Regime")
        return(x)
        })
total <- rbind(listRegimes[[1]], listRegimes[[2]], listRegimes[[3]], listRegimes[[4]], listRegimes[[5]]
               , listRegimes[[6]], listRegimes[[7]], listRegimes[[8]], listRegimes[[9]], listRegimes[[10]]
               , listRegimes[[11]], listRegimes[[12]], listRegimes[[13]], listRegimes[[14]], listRegimes[[15]]
               , listRegimes[[16]], listRegimes[[17]], listRegimes[[18]], listRegimes[[19]]) 

totalPlot <- ggplot(data=subset(total, Regime=="Base" | Regime=="U_I1"| Regime=="U_I2"| Regime=="U_I3"
                        | Regime=="S500_I1"| Regime=="S500_I2"| Regime=="S500_I3"| Regime=="C_I1"
                        | Regime=="C_I2"| Regime=="C_I3"| Regime=="M500_I1"| Regime=="M500_I2"
                        | Regime=="M500_I3"| Regime=="S250_I1"| Regime=="S250_I2"| Regime=="S250_I3"
                        | Regime=="M250_I1"| Regime=="M250_I2"| Regime=="M250_I3"), 
            aes(x=x, y=y, color=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(0,15,1),
         limits=c(0,15), labels=c(0:15))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

totalPlot

I1Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="U_I1"| Regime=="S500_I1"| Regime=="C_I1"
                             | Regime=="M500_I1"| Regime=="S250_I1"| Regime=="M250_I1"), 
                 aes(x=x, y=y, color=Regime))+
        geom_line(size=1)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
        scale_colour_manual(name="Regime",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"))+
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
I1Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I1Comp.pdf", I1Comp, useDingbats=FALSE)

I2Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="U_I2"| Regime=="S500_I2"| Regime=="C_I2"| Regime=="M500_I2"
                                | Regime=="S250_I2"| Regime=="M250_I2"), 
                    aes(x=x, y=y, color=Regime))+
        geom_line(size=1)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
        scale_colour_manual(name="Regime",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"))+
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

I2Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I2Comp.pdf", I2Comp, useDingbats=FALSE)

I3Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="U_I3"
                                | Regime=="S500_I3"| Regime=="C_I3"| Regime=="M500_I3"| Regime=="S250_I3"
                                | Regime=="M250_I3"), 
                    aes(x=x, y=y, color=Regime))+
        geom_line(size=1)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
        scale_colour_manual(name="Regime",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"))+
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

I3Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I3Comp.pdf", I3Comp, useDingbats=FALSE)

UComp <- ggplot(data=subset(total, Regime=="Base" | Regime=="U_I1"| Regime=="U_I2"| Regime=="U_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=1)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

UComp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/UComp.pdf", UComp, useDingbats=FALSE)

S500Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="S500_I1"| Regime=="S500_I2"| Regime=="S500_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

S500Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/S500Comp.pdf", S500Comp, useDingbats=FALSE)

CComp <- ggplot(data=subset(total, Regime=="Base" | Regime=="C_I1"
                                | Regime=="C_I2"| Regime=="C_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

CComp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/CComp.pdf", CComp, useDingbats=FALSE)

M500Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="M500_I1"| Regime=="M500_I2"
                                | Regime=="M500_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

M500Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/M500Comp.pdf", M500Comp, useDingbats=FALSE)

S250Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="S250_I1"| Regime=="S250_I2"| Regime=="S250_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

S250Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/S250Comp.pdf", S250Comp, useDingbats=FALSE)

M250Comp <- ggplot(data=subset(total, Regime=="Base" | Regime=="M250_I1"| Regime=="M250_I2"| Regime=="M250_I3"), 
                    aes(x=x, y=y, linetype=Regime))+
        geom_line(size=0.7)+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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

M250Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/M250Comp.pdf", M250Comp, useDingbats=FALSE)


testerino <- S500_I3
colnames(testerino) <- c("x","y","reg")
testerino
plotino <- ggplot(data=testerino, aes(x=x, y=y))+
        geom_line()+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,10), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,11),
                           breaks=c(0:11))+
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
plotino
