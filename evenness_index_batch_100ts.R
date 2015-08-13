# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the evenness index of the community at the end of a simulation run.
# id = 1/D, where D = sum(ps^2), with ps = relative abundance of the individuals of the sth class in the community.
# first, need to calculate ps for each class, as: ps = nind(s)/totind
# then, sum the squares of all ps
# then calculate id
# needs as input the "total.n" files.

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot") # points the folder with the batch output
list<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot", 
                 recursive=TRUE, pattern="*.csv") # lists all the file (might need to change to .csv)
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=FALSE, sep='\t', dec='.', skip=1902) # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)
mean_list <- lapply(data_list, colMeans) # average over the last 100 steps

evenness_index <- function(data) { # function to be applied to every element of the list, i.e. to every run
        abs <- data[seq(4, 20, 2)] # keeps only the abundance of each class
        empty <- numeric(length(abs)) # initiates the empty vector for the loop
        for (i in 1:length(empty)) { # loop to calculate ps for each class of agents
        empty[i] <- abs[i]/data[2] # in 2nd position is the total number
        }
        ps <- unlist(empty) # turns the list into a vector
        D <- sum(ps^2) # simpson's dominance index
        id <- 1/D # evenness index
        return(id)
}
ids <- sapply(mean_list, evenness_index) # applies the function to the list and returns a vector of indices

ids_scenario <- (c(mean(ids), sd(ids))) # returns mean and sd of the index across all the simulations
ids_scenario <- data.frame(ids_scenario[1], ids_scenario[2])
colnames(ids_scenario) <- c("Mean Evenness Index", "Standard deviation Evenness Index")
ids_scenario
#write.csv(ids_scenario, "evenness_scenario.csv")

# APPROVED and DOUBLECHECKED
