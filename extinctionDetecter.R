# extinction detecter, because why not

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/results500_20/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/results500_20/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) { # should become a plyr function
        matcol[[i]]<-data_list[[i]][,c(4,6,8,10,12,14,20)] # list of matrix containing data of interest: time and classes for each .csv
}

extinctionDetector <- function(x) {
        if (sum((colSums(x==0)!=0))) {"extinction"}else{"stable"}
}
fails <- unlist(lapply(matcol, extinctionDetector))
fails <- length(subset(fails, fails=="extinction"))

