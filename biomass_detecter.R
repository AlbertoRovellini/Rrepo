setwd("C:/Users/Alberto/Documents/itn_jar/out")
data <- read.csv("start_individual.05.csv", header=TRUE, sep="\t", dec=",")
data <- subset(data, data$time=="2000.0") # isolates the last time step, comment out for complete analysis
mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
sub <- subset(data, data$biomass>exp(5.9)&data$biomass<exp(6.1)) # variable
culprits<-levels(as.factor(droplevels(sub$class)))
culprits
sub
nrow(sub)

head(sub)
old <- min(sub$birth)
olderInd <- sub[sub$birth==old,]
