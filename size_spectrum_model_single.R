# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the size spectrum of a community. single run. returns at the end
# one data frame with two entries, the slope and the intercept of the linear model used to fit the data

setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individual")
data <- read.csv("start_individual.csv", header=TRUE, sep="\t", dec=",")
data <- subset(data, data$time==2000) # isolates the last time step, comment out for complete analysis
mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
breaks <- seq(0, ceiling(max(mass)), 5) # sets the breaks ranging over the biomass of the individuals
length_classes <- breaks[2:length(breaks)] # workaround, not sure if legit yet
cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
freq <- table(cat)
freq <- cbind(freq) 
freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
fit <- lm(formula = length_classes ~ freq) # fits linear model
slope <- coef(fit)["freq"] # extracts the slope
intercept <- coef(fit)["(Intercept)"] # extracts the intercept
values <- data.frame(slope, intercept) # stores slope and intercept from one run in a data frame
values
plot(length_classes, freq, log="xy", pch=16, cex=0.6, main="Community") 

# result makes perfectly sense, problem is that the minimum size for sveral classes is very high, hence the 
# clusters of individuals with similar abundance, they are most likely all from the same species.
# can be better but also way worse
