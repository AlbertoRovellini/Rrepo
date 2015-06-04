setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/03062015")
data <- read.csv("start_individual.02.csv", header=TRUE, sep="\t", dec=",")
data <- subset(data, data$time==2000) # isolates the last time step, comment out for complete analysis
mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
sub <- subset(data, data$biomass>400&data$biomass<1000) # variable
culprits<-levels(as.factor(droplevels(sub$class)))
culprits
nrow(sub)

head(sub)
