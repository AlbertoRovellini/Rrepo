# Written by Alberto Rovellini, 27/05/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# script to calculate the mean slope and intercept of the size spectra of multiple output files, to be
# used on the batch output of the itn. the script calculates the slope and the intercept of the linear
# size spectrum and than takes the average and the standard deviation. it also combines the frequency
# data from each run, merges them following the longest breaks sequence and takes the average per each
# length class of the abundance of individuals in that class over all the runs. finally, it plots the
# spectrum as scatterplot, and it is supposed to fit the calculated line to it (is it even legit?)


setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individual")
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individual", 
                 recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec=',') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)

size_spectrum <- function(data) { # function to build and analyze the size spectrum of the community
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass)), 5) # sets the breaks ranging over the biomass of the individuals
        length_classes <- breaks[2:length(breaks)] # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        fit <- lm(formula = length_classes ~ freq) # fits linear model
        slope <- coef(fit)["freq"] # extracts the slope
        intercept <- coef(fit)["(Intercept)"] # extracts the intercept
        values <- data.frame(slope, intercept) # stores slope and intercept from one run in a data frame
        #return(freq)

}
frequencies <-function(data) { # function to extract the frequencies for each replicate
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass)), 5) # sets the breaks ranging over the biomass of the individuals
        length_classes <- breaks[2:length(breaks)] # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq_breaks <- data.frame(length_classes, freq)
}

spectrum_metrics <- lapply(data_list, size_spectrum) # applies the function to all the element of the data list (i.e. to each run)
slopes <- numeric(length(spectrum_metrics)) # creates an empty numeric vector to collect the slopes in the for-loop
intercepts <- numeric(length(spectrum_metrics)) # creates an empty numeric vector to collect the intercept in the for-loop
for (i in 1:length(spectrum_metrics)) { # loop to merge all the slopes together, as well as all the intercepts. retarded
        slopes[i] <- spectrum_metrics[[i]][,1]
        intercepts[i] <- spectrum_metrics[[i]][,2]
        val <- data.frame(slopes, intercepts) # frames the obtained vectors
}
population_slope <- c(mean(val[,1]), sd(val[,1])) # calculates the mean and the sd over the slopes
population_slope <- data.frame(population_slope[1], population_slope[2])
colnames(population_slope) <- c("Mean slope", "Standard deviation slope")
population_intercept <- c(mean(val[,2]), sd(val[,2])) # calculates the mean and the sd over the intercepts
population_intercept <- data.frame(population_intercept[1], population_intercept[2])
colnames(population_intercept) <- c("Mean intercept", "Standard deviation intercept")

population_slope
population_intercept # prints the results 

freqs <- lapply(data_list, frequencies) # so far so good
listbreaks<-numeric(length=length(freqs))
for (i in 1:length(freqs)) {
        listbreaks[i]<-length(freqs[[i]][,1]) 
}
maxlength <- match(max(listbreaks), listbreaks) # extracts the index of the largest break
runs <- list()
for (j in 1:length(freqs)) {
        runs[[j]] <- freqs[[j]][,2]
        runs[[j]] <- c(runs[[j]], rep(0, max(listbreaks)-length(runs[[j]])))
}
runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE) 
mean_runs <- apply(runs, 1, mean)
ensemble <- data.frame(freqs[[maxlength]][,1], runs, mean_runs) # data frame containing all the runs and the largest 
# breaks sequence. shorter lines filled with zeros. now need average and plot and fit of the lm
plot(ensemble[,1], ensemble[,2], pch=16, cex=0.6, log="xy", main="Community size spectrum",
     xlab="Length class", ylab="Abundance (x1000 individuals)")

# maybe should be done with ggplot to look a bit less horrible




