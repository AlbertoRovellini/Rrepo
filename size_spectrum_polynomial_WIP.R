# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the size spectrum of a community. single run. returns at the end
# one data frame with two entries, the slope and the intercept of the linear model used to fit the data.
# now including the option to perform lognormal transformation and regression

setwd("C:/Users/Alberto/Documents/itn_jar/out")
library(ggplot2)
data <- read.csv("start_individual.csv", header=TRUE, sep="\t", dec=",")
data <- subset(data, data$time==2000) # isolates the last time step, comment out for complete analysis
mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
breaks <- seq(0, ceiling(max(mass))+10, 5) # sets the breaks ranging over the biomass of the individuals
length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
freq <- table(cat)
freq <- cbind(freq)
freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
freq[freq<=1] <- NA 



ln_freq <- log(freq) # lognorm transformation of the frequency data
ln_length <- log(length_classes) # lognormal transformation of the length classes
df <- data.frame(ln_freq, ln_length) # builds df of a matrix and a numeric, for the plot


# test of several polynomial models

lin_fit <- lm(ln_freq~ln_length)
pol_2 <- lm(ln_freq~ln_length+I(ln_length^2))
summary(lin_fit)
summary(pol_2)


coefs <- as.numeric(coef(pol_2)) # extracts the coefficients of the quadratic model

# plotter

#plot(ln_length, ln_freq, pch=16, cex=0.6, main="Size spectrum of the community",
     xlab="ln(5 g Weight classes)", ylab="ln(abundance of super-individuals)")
#curve(coefs[1]+coefs[2]*x+coefs[3]*x^2, add=T, col="red") 
fitting <- function(ln_length){coefs[1]+coefs[2]*ln_length+coefs[3]*ln_length^2} # stores the function
# for the curve

# ggplotter

gplot <- ggplot(df, aes(x=ln_length, y=freq))+
        geom_point(shape=1)+
        stat_function(fun = fitting, geom="line", colour = "blue")+
        scale_x_continuous("ln(weight class [5g])", breaks=seq(0,12,1),
                           limits=c(0,12), labels=c(0:12), expand=c(0,0))+
        scale_y_continuous(name="ln(number of Superindividuals)", 
                           limits=c(0,10),
                           breaks=seq(0,10,2), expand=c(0,0))+
        labs(title="Community weight spectrum")+
        #theme(panel.background = element_rect(fill = 'white'))+
        #theme
        #theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(size = 0.1))+ #, panel.grid.major = element_blank())+
        theme(plot.title = element_text(size=12, vjust=2))+
        theme(axis.title.x = element_text(size=10,vjust=-0.5),
              axis.title.y = element_text(size=10,vjust=0.5))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))
       
gplot

ggsave("weight_spectrum_single.pdf", gplot, useDingbats=FALSE)

