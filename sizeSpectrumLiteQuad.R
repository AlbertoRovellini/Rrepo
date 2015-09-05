library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput")
ensemble <- read.table("mixed250_i3.csv", header=TRUE, sep=' ', dec='.')

fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length+I(ensemble$ln_length^2), weights=1/(ensemble$sd^2)) # fitting the quadratic model to the AVERAGE bins
#plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx) 
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)
pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))
summary(fitExperiment)
est <- coef(summary(fitExperiment))[,1]
err <- coef(summary(fitExperiment))[,2]

# functions of the extreme lines

lmin <- function(ln_length) {(est[3]-err[3])*(ln_length)^2+est[2]*ln_length+est[1]}
lmax <- function(ln_length) {(est[3]+err[3])*(ln_length)^2+est[2]*ln_length+est[1]}
mmin <- function(ln_length) {est[3]*(ln_length)^2+(est[2]-err[2])*ln_length+est[1]}
mmax <- function(ln_length) {est[3]*(ln_length)^2+(est[2]+err[2])*ln_length+est[1]}
qmin <- function(ln_length) {est[3]*(ln_length)^2+est[2]*ln_length+est[1]-err[1]}
qmax <- function(ln_length) {est[3]*(ln_length)^2+est[2]*ln_length+est[1]+err[1]}

p <- ggplot(ensemble, aes(x = ln_length, y = ln_freq))+
        geom_point(shape=1, size=3) +
        geom_line(data = pdat, aes(x=bin, y=pred), colour = "blue", size=.6) +
        stat_function(data=ensemble, fun=lmin, linetype="dashed", color="grey")+ 
        stat_function(data=ensemble, fun=lmax, linetype="dashed", color="grey")+ 
        stat_function(data=ensemble, fun=mmin, linetype="dashed", color="grey")+ 
        stat_function(data=ensemble, fun=mmax, linetype="dashed", color="grey")+ 
        stat_function(data=ensemble, fun=qmin, linetype="dashed", color="grey")+ 
        stat_function(data=ensemble, fun=qmax, linetype="dashed", color="grey")+ 
        #geom_ribbon(data = pdat, mapping = aes(x=bin, y=pred, ymax = ymax, ymin = ymin),  
        #alpha = 0.4, fill = "grey60")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(2,11,1),
                           limits=c(2.99,11), labels=c(2:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,16),
                           breaks=c(0:16))+
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

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/sizeSpectra/quad/mixed250_i3.pdf", p, useDingbats=FALSE )


coef(summary(fitExperiment))
summary(fitExperiment)

