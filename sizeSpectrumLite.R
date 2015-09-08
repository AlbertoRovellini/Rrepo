library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput/col")
ensemble <- read.table("U_I2.csv", header=TRUE, sep=' ', dec='.')

fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
#weights=1/(ensemble$sd^2)) # fitting the quadratic model to the AVERAGE bins
plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx) 
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)
pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))
est <- coef(summary(fitExperiment))[,1]
err <- coef(summary(fitExperiment))[,2]

# functions of the extreme lines

trueReg <- function(ln_length){(est[2])*ln_length+est[1]}

mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}
mmax <- function(ln_length){(est[2]+err[2])*ln_length+est[1]}
qmin <- function(ln_length){est[2]*ln_length+est[1]-err[1]}
qmax <- function(ln_length){est[2]*ln_length+est[1]+err[1]}

# nice colourful topolino plot

p <- ggplot(subset(ensemble, dom==1 | dom==2| dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, colour= factor(dom))) +
        geom_point(aes(size=2, position="jitter"), shape=1) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="blue")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_colour_manual(name="Functional groups",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                     "Medium demersal", "Large demersals", "Top carnivores"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
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

ggsave("C:/Users/Alberto/Documents/itn100results/R_output/unselective/ind/U_I2Col1.pdf", p, useDingbats=FALSE )


# serious professional I know my shit plot

pp <- ggplot(subset(ensemble, dom==1 | dom==2| dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, shape= factor(dom))) +
        geom_point(size=2) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="blue")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_shape_manual(name="Functional groups",
                            values=c(0:6),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                     "Medium demersal", "Large demersals", "Top carnivores"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
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

pp

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/sizeSpectra/mixed250_i3.pdf", p, useDingbats=FALSE )

coef(summary(fitExperiment))
summary(fitExperiment)
