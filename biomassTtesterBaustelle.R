setwd("C:/Users/Alberto/Documents/itn100results/unselective_i1/tot")
data <- read.csv("0000_total.csv", header=T, sep='\t', dec='.', skip=1900, nrow=100)

listOfFiles <- as.list(list.files("C:/Users/Alberto/Documents/itn100results/unselective_i1/tot")) 

averageCalculator <- function(x) { # to be applied to the file as argument of the applier
        data <- read.table(x, header=T, sep='\t', dec='.')
        data <- data[c(1901:2000),]
        data <- data[,c(3,5,7,9,11,13,15,21)]
        colnames(data) <- c("total", "smallpelagic","mediumpelagic", "largepelagic", "smalldemersal",
                            "mediumdemersal", "largedemersal", "toppiscivores")
        mean100 <- colMeans(data)
        return(mean100)
}

averageApplier <- function(y) { # to be applied to the directory, runs the function on its files
        listOfFiles <- as.list(list.files(y)) 
        allReps <- abind(lapply(listOfFiles, averageCalculator),along=0) # build a frame with 100 mean values. need one of these per scenario
        return(allReps)
}


data <- read.table("0025_total.csv", header=T, sep='\t', dec='.', skip=1900, nrow=100)
data <- data[,c(3,5,7,9,11,13,15,21)]
colnames(data) <- c("total", "smallpelagic","mediumpelagic", "largepelagic", "smalldemersal",
                    "mediumdemersal", "largedemersal", "toppiscivores")
mean100 <- colMeans(data)
mean100





a <- rnorm(1000)
shap <- shapiro.test(a)
shap
class(shap)
