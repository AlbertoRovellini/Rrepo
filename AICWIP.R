# AIC routines, model comparison.

library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput/col")
ensemble <- read.table("base.csv", header=TRUE, sep=' ', dec='.')

# ensemble$dom <- c(ensemble$dom[2:length(ensemble$dom)], NA) # to be used ONLY if not yet, check the 
# # head of the ensemble file
# levels(factor(ensemble$dom)) # check which level is missing and remove it from the plot aesthetics

# linear non-weighted fit

fitLin = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
                   #weights=ensemble$ln_freq) # fitting the quadratic model to the AVERAGE bins
#plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
predLin <- predict(fitLin,newdata=newx) 
setLin <- data.frame(newx, predLin, ymax=predLin+ensemble$sd, ymin=predLin-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)

pdf("lin.pdf", useDingbats=FALSE, width=6, height=4)
plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15))
lines(setLin$bin, setLin$predLin, col="blue")
dev.off()

estLin <- coef(summary(fitLin))[,1]
errLin <- coef(summary(fitLin))[,2]

# linear weighted fit

fitLinW = lm(ensemble$ln_freq ~ ensemble$ln_length, weights=ensemble$ln_freq) # fitting the quadratic model to the AVERAGE bins
predLinW <- predict(fitLinW,newdata=newx) 
setLinW <- data.frame(newx, predLinW, ymax=predLinW+ensemble$sd, ymin=predLinW-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)

pdf("linW.pdf", useDingbats=FALSE, width=6, height=4)
plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15))
lines(setLinW$bin, setLinW$predLin, col="blue")
dev.off()

estLinW <- coef(summary(fitLinW))[,1]
errLinW <- coef(summary(fitLinW))[,2]


# quadratic fit

fitQuad <- lm(ensemble$ln_freq ~ ensemble$ln_length+I(ensemble$ln_length^2))
predQuad <- predict(fitQuad, newdata=newx)
pdatQuad <- data.frame(newx, predQuad, ymax=predQuad+ensemble$sd, ymin=predQuad-ensemble$sd)

pdf("quad.pdf", useDingbats=FALSE, width=6, height=4)
plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15))
lines(pdatQuad$bin, pdatQuad$predQuad, col="blue")
dev.off()

estQuad <- coef(summary(fitQuad))[,1]
errQuad <- coef(summary(fitQuad))[,2]

# exponential fit 

fitExp <- lm(log(ensemble$ln_freq) ~ ensemble$ln_length)

predExp <- exp(predict(fitExp,newdata=newx)) 
pdatExp <- data.frame(newx, predExp, ymax=predExp+ensemble$sd, ymin=predExp-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)
pdatExp <- with(data.frame(predExp),
             data.frame(x = newx, y = fitExp))

pdf("exp.pdf", useDingbats=FALSE, width=6, height=4)
plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15))
lines(pdatExp$bin, pdatExp$predExp, col="blue")
dev.off()

est <- coef(summary(fitExp))[,1]
err <- coef(summary(fitExp))[,2]

# 


# functions of the extreme lines

trueReg <- function(ln_length){(est[2])*ln_length+est[1]}

mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}
mmax <- function(ln_length){(est[2]+err[2])*ln_length+est[1]}
qmin <- function(ln_length){est[2]*ln_length+est[1]-err[1]}
qmax <- function(ln_length){est[2]*ln_length+est[1]+err[1]}

# nice colourful topolino plot (complete one, for missing levels mess with the next code region)

p <- ggplot(subset(ensemble, dom==1 | dom==2 | dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, color= factor(dom))) +
        geom_point(aes(size=2, position="jitter"), shape=16, alpha=0.7) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="blue")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="log(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_color_manual(name="Functional groups",
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

ggsave("C:/Users/Alberto/Desktop/youmares/poster/graphs/weighted/S250_I1.pdf", p, useDingbats=FALSE )

###########################################################################

# use-and-trash code for the plots with missing levels

p <- ggplot(subset(ensemble, dom==1 | dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, color= factor(dom))) +
        geom_point(aes(size=2, position="jitter"), shape=16, alpha=0.7) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="blue")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="log(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_color_manual(name="Functional groups",
                           values=c("#377EB8", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                           labels=c("Small pelagic",  "Large pelagic", "Small demersal",
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

ggsave("C:/Users/Alberto/Desktop/youmares/poster/graphs/weighted/S250_I3.pdf", p, useDingbats=FALSE )

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

##############################################################################

rSquared <- c(summary(fitLin)$r.squared,
              summary(fitLinW)$r.squared,
              summary(fitQuad)$r.squared,
              summary(fitExp)$r.squared)
AIC <- c(AIC(fitLin), AIC(fitLinW), AIC(fitQuad), AIC(fitExp))
BIC <- c(BIC(fitLin), BIC(fitLinW), BIC(fitQuad), BIC(fitExp))

# multiplotter, base graphics (mere exercise indeed)

pdf("plotPanel.pdf", useDingbats=FALSE, width=7, height=5.5)
par(mfrow=c(2, 2))
plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15),
     main="Linear least squares", xlab="log(weight class [20g])",
     ylab="log(n° individuals)")
lines(setLin$bin, setLin$predLin, col="blue")
text(10, 12, paste("R sq =", round(rSquared[1],2), "\nAIC =", 
                   round(AIC[1],2), "\nBIC =", round(BIC[1],2)))

plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15),
     main="Weigted least squares", xlab="log(weight class [20g])",
     ylab="log(n° individuals)")
lines(setLinW$bin, setLinW$predLinW, col="blue")
text(10, 12, paste("R sq =", round(rSquared[2],2), "\nAIC =", 
                   round(AIC[2],2), "\nBIC =", round(BIC[2],2)))

plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15),
     main="Quadratic model", xlab="log(weight class [20g])",
     ylab="log(n° individuals)")
lines(pdatQuad$bin, pdatQuad$predQuad, col="blue")
text(10, 12, paste("R sq =", round(rSquared[3],2), "\nAIC =", 
                   round(AIC[3],2), "\nBIC =", round(BIC[3],2)))

plot(ensemble$ln_length, ensemble$ln_freq, xlim=c(0, 12), ylim=c(0, 15),
     main="Exponential model", xlab="log(weight class [20g])",
     ylab="log(n° individuals)")
lines(pdatExp$bin, pdatExp$predExp, col="blue")
text(10, 12, paste("R sq =", round(rSquared[4],2), "\nAIC =", 
                   round(AIC[4],2), "\nBIC =", round(BIC[4],2)))
dev.off()

