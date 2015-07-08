# script to calculate the mean and sd of the biomass of each class at the final time step. 

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_e/results_0001/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_e/results_0001/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=2000) # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) {
        matcol[[i]]<-data_list[[i]][,seq(1, 21, 2)] # list of matrix containing data of interest: time and classes for each .csv
}
all.matrix <- abind(matcol, along=3) # change the structure of the matrix matcol in order to use the function apply on it
mean_all <- apply(all.matrix, c(1,2), mean) # calculates the mean biomass in every position
sd_all <- apply(all.matrix, c(1,2), sd) # and its sd
sd_all[,1] <- mean_all[,1]

colnames(mean_all)<-c("Time","total","smallpelagic","mediumpelagic","largepelagic","smalldemersal",
                      "mediumdemersal","largedemersal",
                      "mediumgrazer","largegrazer","topcarnivores") # changes the names of the columns
colnames(sd_all)<-c("Time","totalSD","smallpelagicSD","mediumpelagicSD","largepelagicSD","smalldemersalSD",
                    "mediumdemersalSD","largedemersalSD",
                    "mediumgrazerSD","largegrazerSD","topcarnivoresSD") # for sd as well
mean_all<-as.data.frame(mean_all) # turns the matrix into a data frame
sd_all<-as.data.frame(sd_all) # same
comb <- merge(mean_all, sd_all, "Time")
percentages <- mean_all/mean_all[,2]*100
variation <- sd_all*100/mean_all # computes the ratio between sd and mean (in percentage)
percentages
write.table(percentages, "C:/Users/Alberto/Documents/MASTER THESIS/testOutput/percentages.csv")
write.table(mean_all, "C:/Users/Alberto/Documents/MASTER THESIS/testOutput/meanBiomass.csv")


