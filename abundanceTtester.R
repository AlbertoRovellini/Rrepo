# script to calculate the final biomass at the last 100 time steps (average) of all the runs.

# read all the tot files per simulation, and from each extract the value of the final biomass (use as 
# template the existing scipt for final biomass calculation)

# store the 100 values per scenario into a table, 19 tables will come out

# compare those tables via t-test (need to test for assumptions first)

# open mother directory

setwd("C:/Users/Alberto/Documents/itn100results")
directories <- list.dirs("C:/Users/Alberto/Documents/itn100results", recursive=TRUE)
directoriesTot <- directories[grep("tot", directories)] # isolates the directories I need
directoriesTot <- directoriesTot[-c(10,11)] # drops two folders I don't need
listOfDirectories <- as.list(directoriesTot)

averageCalculator <- function(x) {
        data <- read.table(x, header=T, sep='\t', dec='.', skip=1900, nrows=100)
        data <- data[,c(2,4,6,8,10,12,14,20)]
        colnames(data) <- c("total", "smallpelagic","mediumpelagic", "largepelagic", "smalldemersal",
                            "mediumdemersal", "largedemersal", "toppiscivores")
        mean100 <- colMeans(data)
        return(mean100)
}

averageApplier <- function(y) {
        listOfFiles <- as.list(list.files(y)) 
        allReps <- abind(lapply(listOfFiles, averageCalculator),along=0) # build a frame with 100 mean values. need one of these per scenario
        return(allReps)
}

listOfDirectories <- as.list(directoriesTot)

scenarios <- list() # empty list for the loop

for (i in 1:length(listOfDirectories)) { # nailed it
        setwd(listOfDirectories[[i]]) # each directory of the list (19)
        listOfFiles <- as.list(list.files(listOfDirectories[[i]]))
        allReps <- abind(lapply(listOfFiles, averageCalculator),along=0) # build a frame with 100 mean values. need one of these per scenario
        allReps
        scenarios[[i]] <- allReps
}


totalBiomass <- list() # isolate the total biomass, potentially and desirably to be done for each class

for (i in 1:length(scenarios)) {
        totalBiomass[[i]]<-scenarios[[i]][,1]
}

directoryIndex <- unlist(listOfDirectories) # to understand what is what, index for future frames

# test for normality, return a string of pvalues of the shapiro-wilk test

pValuer <- function(x) {
        pVal <- x$p.value
        return(pVal)
} # function to extract the pvalue 

normTot <- lapply(totalBiomass, shapiro.test)
pnormTot <- unlist(lapply(normTot, pValuer))

pValuesShap <- data.frame(directoryIndex, pnormTot) # some are normal, some not, screw it, MWW test

# Mann-Whitney/Wilcoxon test against the baseline, total biomass

wTestSummary <- list()

for (i in 1:length(totalBiomass)) {
        wTestSummary[[i]] <- wilcox.test(totalBiomass[[10]], totalBiomass[[i]])
        wTestSummary
}

pValuesW <- unlist(lapply(wTestSummary, pValuer))
pvaluesWilcoxon <- data.frame(directoryIndex, pValuesW)
pvaluesWilcoxon


