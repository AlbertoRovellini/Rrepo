# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the size spectrum of a community. single run. returns at the end
# one data frame with two entries, the slope and the intercept of the linear model used to fit the data.
# now including the option to perform lognormal transformation and regression

# *****************CAUTION**********************
# sandbox script, good for testing, unrefined

setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individual")
data <- read.csv("start_individual.21.csv", header=TRUE, sep="\t", dec=",")
data <- subset(data, data$time==2000) # isolates the last time step, comment out for complete analysis
mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
breaks <- seq(0, ceiling(max(mass))+10, 5) # sets the breaks ranging over the biomass of the individuals
length_classes <- breaks[2:length(breaks)] # workaround, not sure if legit yet
cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
freq <- table(cat)
freq <- cbind(freq) 
freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
freq[freq<3] <- NA # lower limit of resoulution, gets rid of the outliers. kek

#ln_freq <- log(freq) # lognorm transformation of the frequency data
#ln_length <- log(length_classes) # lognormal transformation of the length classes

fit <- lm(formula = length_classes ~ freq) # fits linear model
slope <- coef(fit)["freq"] # extracts the slope
intercept <- coef(fit)["(Intercept)"] # extracts the intercept
values <- data.frame(slope, intercept) # stores slope and intercept from one run in a data frame
values

#ln_fit <- lm(formula = ln_length ~ ln_freq) # linear regression to the logdata
#ln_slope <- coef(ln_fit)["ln_freq"] # extracts the slope of the lognormal series
#ln_intercept <- coef(ln_fit)["(Intercept)"] # extracts the intercept of the lognormal series
#ln_values <- data.frame(ln_slope, ln_intercept)
#ln_values


vec <- c(slope, intercept)
ln_vec <- c(ln_slope, ln_intercept)

plot(length_classes, freq, pch=16, cex=0.6, main="Community", xlim=c(0, 2e+4))
abline(b=slope, a=intercept, untf=TRUE)

#plot(ln_length, ln_freq, pch=16, cex=0.6, main="Size spectrum of the community",
     #xlab="ln(5 g Weight classes)", ylab="ln(Relative abundance of individuals x 1000)")
#abline(b=ln_slope, a=ln_intercept)

# need to figure out who are the fuckers who determine those clusters. most likely same class.
# approach: start from the initially read file, figure out a biomass range, extract the name of the
# species, count them. 

big <- subset(data, data$biomass>11000 & data$biomass<12000) # sample around first cluster
levels(droplevels(as.factor(big$class))) # largedemersals and large grazers

bigger <- subset(data, data$biomass>15000 & data$biomass<16000) # sample around second cluster
levels(droplevels(as.factor(bigger$class))) # largedemersals and large grazers


