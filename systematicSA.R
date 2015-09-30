# script to extract the number of extinctions from each run in the systematic sa runs
# returns a frame with the variations and the value

setwd("C:/Users/Alberto/Documents/MASTER THESIS/results/sa_results/systematicSAResults/res")
directories <- list.dirs("C:/Users/Alberto/Documents/MASTER THESIS/results/sa_results/systematicSAResults/res")
directories <- directories[2:length(directories)]
directoriesNum <- unique(na.omit(as.numeric(unlist(strsplit(unlist(directories), "[^0-9]+")))))
indices <- unlist(as.matrix(sort.int(directoriesNum, index.return=TRUE))[2])
dirFrame <- data.frame(directories, directoriesNum)
directoriesOrd <- as.character(dirFrame[order(dirFrame[,2]),][,1])
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', nrows=2000) # custom function to read the batches of .csv keeping the header
} # 2001 because the 2100 steps is an artifact for the last fishery cycle
extinctionDetector <- function(x) { # whaaaaat
        if (sum(colSums(x==0)!=0)) {"extinction"}else{"stable"}
}
listOfDirectories <- as.list(directoriesOrd)

failVector <- list()

for (i in 1:length(listOfDirectories)) { # nailed it
        setwd(listOfDirectories[[i]])
        files <- list.files(listOfDirectories[[i]], pattern="total.csv")
        length.list<-length(files)
        data_list<-lapply(files, read.special) # all the data in a huge list of data
        matcol<-list() # empty list for the loop
        for (j in c(1:length.list)) { # should become a plyr function
                matcol[[j]]<-data_list[[j]][,c(4,6,8,10,12,14,20)] # list of matrix containing data of interest: time and classes for each .csv
        }
        fails <- unlist(lapply(matcol, extinctionDetector))
        fails <- length(subset(fails, fails=="extinction"))
        failVector[[i]] <- fails
}

failVector <- unlist(failVector)
catchRangeMedPel <- c(rep(-10, 9), rep(0, 9), rep(10, 9))
catchRangeMedDem <- rep(c(rep(-10,3), rep(0, 3), rep(10, 3)), 3)
catchRangeLargeDem <- rep(c(-10, 0, 10), 9)

combinations <- data.frame(failVector, catchRangeMedPel, catchRangeMedDem, catchRangeLargeDem)
colnames(combinations) <- c("Extinctions", "Mediumpelagics", "Mediumdemersal", "Largedemersal")

combinations

write.table(combinations, "C:/Users/Alberto/Documents/itn100results/input/sa/extinctions.csv")

