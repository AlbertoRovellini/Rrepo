# script to perform exploratory tests on the individual output. this should be able to:
# 1) track one individual over the whole simulation according to its id
# 2) give snapshots at a defined timestep

setwd("C:/Users/Alberto/Documents/itn_e_test/out")
data <- read.csv("start_individual.85.csv", header = TRUE, sep="\t", dec=",")
data <- split(data, data$class)
largepel <- data$largepelagic # extract from the list one species to analyze
largepel <- largepel[with(largepel, order(largepel$id, largepel$time)), ] # order by id and timestep
# length(levels(factor(as.character(largepel$id))))
levels(factor(as.character(largepel$id)))
indtest <- subset(largepel, largepel$id == 1683739)
indtest
plot(indtest$time, indtest$biomass, type="l")
