# Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to calculate the size spectrum of a community at a defined time-step, and fit a linear model
# (for now). notice that the sample data frames contain only the population at one defined time step, which is
# namely the last one of one simulation.


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
        breaks <- seq(floor(min(mass)), ceiling(max(mass)), 5) # sets the breaks ranging over the biomass of the individuals
        length_classes <- breaks[2:length(breaks)] # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq) 
        fit <- lm(formula = length_classes ~ freq) # fits linear model
        slope <- coef(fit)["freq"] # extracts the slope
        intercept <- coef(fit)["(Intercept)"] # extracts the intercept
        values <- data.frame(slope, intercept) # stores slope and intercept from one run in a data frame
        

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

# seems to be working, must find a way to plot it though