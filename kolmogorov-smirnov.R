setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot") # points the folder with the batch output
list<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot", 
                 recursive=TRUE, pattern="*.csv") # lists all the file (might need to change to .csv)
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=FALSE, sep='\t', dec='.', skip=1902, nrows=100) # custom function to read the batches of .csv keeping the header
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
ids <- sapply(mean_list, evenness_index)
write.table(ids, "C:/Users/Alberto/Documents/itn100results/boxplotInput/mixed250_i3.csv")
mean(ids)
sd(ids)
ks.test(ids, "pnorm", mean(ids), sd(ids))
x <- seq(1,3,0.02)
dist <- dnorm(x, mean=mean(ids), sd=sd(ids))
hist(ids, breaks=50)
lines(x,dist)
