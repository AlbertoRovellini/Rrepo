# this is a script to calculate the evenness index of the community at the end of a simulation run.
# id = 1/D, where D = sum(ps^2), with ps = relative abundance of the individuals of the sth class in the community.
# first, need to calculate ps for each class, as: ps = nind(s)/totind
# then, sum the squares of all ps
# then calculate id

setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype")
data <- read.csv("start_total.prototype.txt", header=T, sep="\t")
#data <- data[,c(1:ncol(data)-1)] # workaround, removes the last column which is an error. hopefully won't need it
last <- data[nrow(data),] # extracts the last row
last1 <- last[3:length(last)] # get rid of the first two values (time and total)
empty <- numeric(length(last)-2) # initiates the empty vector for the loop
for (i in 1:length(empty)) { # loop to calculate ps for each class of agents
        empty[i] <- last1[i]/last[2] # in 2nd position is the total number
}
ps <- unlist(empty) # turns the list into a vector
D <- sum(ps^2) # simpson's dominance index
id <- 1/D # evenness index
id
# appliable to get one index from one run. to be batched

