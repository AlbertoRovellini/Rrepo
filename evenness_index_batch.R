# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the evenness index of the community at the end of a simulation run.
# id = 1/D, where D = sum(ps^2), with ps = relative abundance of the individuals of the sth class in the community.
# first, need to calculate ps for each class, as: ps = nind(s)/totind
# then, sum the squares of all ps
# then calculate id

setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype") # points the folder with the batch output
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/prototype", 
                 recursive=TRUE, pattern="*.txt") # lists all the file (might need to change to .csv)
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)
evenness_index <- function(data) { # function to be applied to every element of the list, i.e. to every run
        last <- data[nrow(data),] # extracts the last row
        last1 <- last[3:length(last)] # get rid of the first two values (time and total)
        empty <- numeric(length(last)-2) # initiates the empty vector for the loop
        for (i in 1:length(empty)) { # loop to calculate ps for each class of agents
        empty[i] <- last1[i]/last[2] # in 2nd position is the total number
        }
        ps <- unlist(empty) # turns the list into a vector
        D <- sum(ps^2) # simpson's dominance index
        id <- 1/D # evenness index
}
ids <- sapply(data_list, evenness_index) # applies the function to the list and returns a vector of indices
ids_scenario <- c(mean(ids), sd(ids)) # returns mean and sd of the index across all the simulations