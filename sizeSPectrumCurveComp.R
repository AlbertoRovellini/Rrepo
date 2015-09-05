library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput")
parameters <- read.table("parametersLin.csv", header=TRUE, sep=';', dec='.', row.names=1)[,c(1,2)]

mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}

functionList <- list()
x <- seq(3,11,0.01)
y <- seq(0,11,length.out=length(x))
z <- data.frame(x,y)
# for (i in 1:nrow(parameters)) {
#         functionList[[i]] <- function(x){parameters[i,1]*x+parameters[i,2]}
# }

# extremely inefficient and inelegant. to be recoded (supposedly plyr)
p <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # U
        stat_function(data=z, fun= function(x){parameters[2,1]*x+parameters[2,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[3,1]*x+parameters[3,2]}, linetype="dashed", color="darkgrey", size=1)+
        stat_function(data=z, fun= function(x){parameters[4,1]*x+parameters[4,2]}, linetype="dashed", color="grey", size=1)+
        # S500
        stat_function(data=z, fun= function(x){parameters[5,1]*x+parameters[5,2]}, linetype="dotted", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[6,1]*x+parameters[6,2]}, linetype="dotted", color="darkgrey", size=1)+
        stat_function(data=z, fun= function(x){parameters[7,1]*x+parameters[7,2]}, linetype="dotted", color="grey", size=1)+
        # C
        stat_function(data=z, fun= function(x){parameters[8,1]*x+parameters[8,2]}, linetype="dotdash", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[9,1]*x+parameters[9,2]}, linetype="dotdash", color="darkgrey", size=1)+
        stat_function(data=z, fun= function(x){parameters[10,1]*x+parameters[10,2]}, linetype="dotdash", color="grey", size=1)+
        # M500
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="longdash", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[12,1]*x+parameters[12,2]}, linetype="longdash", color="darkgrey", size=1)+
        stat_function(data=z, fun= function(x){parameters[13,1]*x+parameters[13,2]}, linetype="longdash", color="grey", size=1)+
        # S250
        stat_function(data=z, fun= function(x){parameters[14,1]*x+parameters[14,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[15,1]*x+parameters[15,2]}, linetype="solid", color="darkgrey", size=1)+
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="solid", color="grey", size=1)+
        # M250
        stat_function(data=z, fun= function(x){parameters[17,1]*x+parameters[17,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[18,1]*x+parameters[18,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[19,1]*x+parameters[19,2]}, linetype="dashed", color="black", size=1)+


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

p

I1Comp <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # U
        stat_function(data=z, fun= function(x){parameters[2,1]*x+parameters[2,2]}, linetype="dashed", color="black", size=1)+
        # S500
        stat_function(data=z, fun= function(x){parameters[5,1]*x+parameters[5,2]}, linetype="twodash", color="black", size=1)+
        # C
        stat_function(data=z, fun= function(x){parameters[8,1]*x+parameters[8,2]}, linetype="dotdash", color="black", size=1)+
        # M500
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="longdash", color="black", size=1)+
        # S250
        stat_function(data=z, fun= function(x){parameters[14,1]*x+parameters[14,2]}, linetype="solid", color="black", size=1)+
        # M250
        stat_function(data=z, fun= function(x){parameters[17,1]*x+parameters[17,2]}, linetype="dotted", color="black", size=1)+        
        
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

I1Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I1Comp.pdf", I1Comp, useDingbats=FALSE)

I2Comp <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # U
        stat_function(data=z, fun= function(x){parameters[3,1]*x+parameters[3,2]}, linetype="dashed", color="black", size=1)+
        # S500
        stat_function(data=z, fun= function(x){parameters[6,1]*x+parameters[6,2]}, linetype="twodash", color="black", size=1)+
        # C
        stat_function(data=z, fun= function(x){parameters[9,1]*x+parameters[9,2]}, linetype="dotdash", color="black", size=1)+
        # M500
        stat_function(data=z, fun= function(x){parameters[12,1]*x+parameters[12,2]}, linetype="longdash", color="black", size=1)+
        # S250
        stat_function(data=z, fun= function(x){parameters[15,1]*x+parameters[15,2]}, linetype="solid", color="black", size=1)+
        # M250
        stat_function(data=z, fun= function(x){parameters[18,1]*x+parameters[18,2]}, linetype="dotted", color="black", size=1)+
        
        
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

I2Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I2Comp.pdf", I2Comp, useDingbats=FALSE)

I3Comp <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # U
        stat_function(data=z, fun= function(x){parameters[4,1]*x+parameters[4,2]}, linetype="dashed", color="black", size=1)+
        # S500
        stat_function(data=z, fun= function(x){parameters[7,1]*x+parameters[7,2]}, linetype="twodash", color="black", size=1)+
        # C
        stat_function(data=z, fun= function(x){parameters[10,1]*x+parameters[10,2]}, linetype="dotdash", color="black", size=1)+
        # M500
        stat_function(data=z, fun= function(x){parameters[13,1]*x+parameters[13,2]}, linetype="longdash", color="black", size=1)+
        # S250
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="solid", color="black", size=1)+
        # M250
        stat_function(data=z, fun= function(x){parameters[19,1]*x+parameters[19,2]}, linetype="dotted", color="black", size=1)+
        
        
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

I3Comp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/I3Comp.pdf", I3Comp, useDingbats=FALSE)

U <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # U
        stat_function(data=z, fun= function(x){parameters[2,1]*x+parameters[2,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[3,1]*x+parameters[3,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[4,1]*x+parameters[4,2]}, linetype="dotted", color="black", size=1)+
        
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

U

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/UComp.pdf", U, useDingbats=FALSE)

S500 <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # S500
        stat_function(data=z, fun= function(x){parameters[5,1]*x+parameters[5,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[6,1]*x+parameters[6,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[7,1]*x+parameters[7,2]}, linetype="dotted", color="black", size=1)+
        
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

S500

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/S500Comp.pdf", S500, useDingbats=FALSE)

C <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # C
        stat_function(data=z, fun= function(x){parameters[8,1]*x+parameters[8,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[9,1]*x+parameters[9,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[10,1]*x+parameters[10,2]}, linetype="dotted", color="black", size=1)+
        
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

C

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/CComp.pdf", C, useDingbats=FALSE)

M500 <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # M500
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[12,1]*x+parameters[12,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[13,1]*x+parameters[13,2]}, linetype="dotted", color="black", size=1)+
        
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

M500

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/M500Comp.pdf", M500, useDingbats=FALSE)

S250 <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # S250
        stat_function(data=z, fun= function(x){parameters[14,1]*x+parameters[14,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[15,1]*x+parameters[15,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[11,1]*x+parameters[11,2]}, linetype="dotted", color="black", size=1)+
        
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

S250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/S250Comp.pdf", S250, useDingbats=FALSE)

M250 <- ggplot(data=z, aes(x = x, y = y))+
        # Base
        stat_function(data=z, fun= function(x){parameters[1,1]*x+parameters[1,2]}, linetype="solid", color="blue", size=1)+
        # M250
        stat_function(data=z, fun= function(x){parameters[17,1]*x+parameters[17,2]}, linetype="solid", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[18,1]*x+parameters[18,2]}, linetype="dashed", color="black", size=1)+
        stat_function(data=z, fun= function(x){parameters[19,1]*x+parameters[19,2]}, linetype="dotted", color="black", size=1)+
        
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

M250

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/graphics/spectra/M250.pdf", M250, useDingbats=FALSE)