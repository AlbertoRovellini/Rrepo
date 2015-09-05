# script to calculate the mean and sd of the biomass of each class at the final time step. 

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1901, nrow=100) # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
mean_data_list <- lapply(data_list, function(x) apply(x, 2, mean)) # average over the last 100 steps
mean_data_list <- lapply(mean_data_list, function(x) matrix(x, nrow=1)) # average over the last 100 steps
mean_data_list <- lapply(mean_data_list, as.data.frame) # average over the last 100 steps

matcol<-list() # empty list for the loop
for (i in c(1:length.list)) {
        matcol[[i]]<-mean_data_list[[i]][,seq(1, 21, 2)] # list of matrix containing data of interest: time and classes for each .csv
}
all.matrix <- matrix(abind(matcol, along=3), nrow=100, byrow=TRUE) # change the structure of the matrix matcol in order to use the function apply on it

# corce to data frame for input of the t-test whatsoever


colnames(all.matrix)<-c("Time","total","smallpelagic","mediumpelagic","largepelagic","smalldemersal",
                        "mediumdemersal","largedemersal",
                        "mediumgrazer","largegrazer","topcarnivores") # changes the names of the columns


write.table(all.matrix, "C:/Users/Alberto/Documents/itn100results/input/finalBiomassInput/mixed250_i3.csv")
# #write.table(mean_all, "C:/Users/Alberto/Documents/MASTER THESIS/testOutput/meanBiomass.csv")


