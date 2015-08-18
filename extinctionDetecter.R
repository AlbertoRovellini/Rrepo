# extinction detecter, because why not

setwd("C:/Users/Alberto/Documents/itn100results/unselective_I3/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/itn100results/unselective_I3/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', nrows=2001) # custom function to read the batches of .csv keeping the header
} # 2001 because the 2100 steps is an artifact for the last fishery cycle
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) { # should become a plyr function
        matcol[[i]]<-data_list[[i]][,c(4,6,8,10,12,14,20)] # list of matrix containing data of interest: time and classes for each .csv
}

extinctionDetector <- function(x) { # whaaaaat
        if (sum(colSums(x==0)!=0)) {"extinction"}else{"stable"}
}
fails <- unlist(lapply(matcol, extinctionDetector))
fails <- length(subset(fails, fails=="extinction"))
fails

write.table(fails, "C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/scenarios/class/i3/extinctions.csv")

