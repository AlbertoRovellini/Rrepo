# Written by Alberto Rovellini, 27/05/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# script to calculate the mean slope and intercept of the size spectra of multiple output files, to be
# used on the batch output of the itn. the script calculates the slope and the intercept of the linear
# size spectrum and than takes the average and the standard deviation. it also combines the frequency
# data from each run, merges them following the longest breaks sequence and takes the average per each
# length class of the abundance of individuals in that class over all the runs. finally, it plots the
# spectrum as scatterplot, and it is supposed to fit the calculated line to it (is it even legit?)

# 29/05/2015 code can be more compact and more efficient. one function would be enough.
# however, does the job. fitted model is still linear (and probably not even legit, as the whole
# averaging procedure)

# 03/06/2015 correction of a syntax error in the lm formula 

library(ggplot2)
library(abind)
setwd("C:/Users/Alberto/Desktop/itn_jar/out/otherAttempt2/ind")
list<-list.files("C:/Users/Alberto/Desktop/itn_jar/out/otherAttempt2/ind", 
                 recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec=',') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)
lastTimeStep <- function(data) subset(data, data$time=="2000.0") # isolates the last time step, comment out for complete analysis
data_list <- lapply(data_list, lastTimeStep)


size_spectrum <- function(data) { # function to build and analyze the size spectrum of the community
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass))+10, 20) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) 
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        freq[freq<1] <- NA # lower limit of resoulution, gets rid of the outliers. kek
        log_freq <- log(freq) # lognorm transformation of the frequency data
        log_length <- log(length_classes) # lognormal transformation of the length classes
        log_df <- data.frame(log_length, log_freq)
        pol_3 <- lm(log_freq~log_length+I(log_length^2)+I(log_length^3))
        coefs <- as.numeric(coef(pol_3)) # extracts the coefficients of the quadratic model
        coefs
        
}
frequencies <-function(data) { # function to extract the frequencies for each replicate
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass))+10, 20) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        freq[freq<1] <- NA # lower limit of resoulution, gets rid of the outliers. kek
        ln_freq <- log(freq) # lognorm transformation of the frequency data
        ln_length <- log(length_classes) # lognormal transformation of the length classes
        freq_breaks <- data.frame(ln_length, ln_freq)
        
}

spectrum_metrics <- lapply(data_list, size_spectrum) # applies the function to all the element of the data list (i.e. to each run)
spectrum_metrics <- abind(spectrum_metrics, along=2)
# for (i in 1:length(spectrum_metrics)) {
#         if (spectrum_metrics[1,i]<0) {
#                 spectrum_metrics <- spectrum_metrics[,-i]
#         }
# }
meanMetrics <- apply(spectrum_metrics, 1, mean) # gets the mean of the coefficients
sdMetrics <- apply(spectrum_metrics, 1, sd) # gets the sd of the coefficients
# meanMetricsMin <- meanMetrics-sdMetrics
# meanMetricsMax <- meanMetrics+sdMetrics

fittingCube <- function(x){meanMetrics[1]+meanMetrics[2]*x+
                meanMetrics[3]*x^2+meanMetrics[4]*x^3} # function with the coefficients
independentVariable <- seq(0,11,0.01) # some random x variable, to plot the function

# fittingMin <- function(x){meanMetricsMin[1]+meanMetricsMin[2]*x+
#                 meanMetricsMin[3]*x^2+meanMetricsMin[4]*x^3} # function with the coefficients
# 
# fittingMax <- function(x){meanMetricsMax[1]+meanMetricsMax[2]*x+
#                 meanMetricsMax[3]*x^2+meanMetricsMax[4]*x^3} # function with the coefficients

weightSpectrum <- fittingCube(independentVariable) # gets the point to draw the curve
frame <- data.frame(independentVariable,weightSpectrum) # for the dang ggplot

# weightSpectrumMin <- fittingMin(independentVariable)
# weightSpectrumMax <- fittingMax(independentVariable)

gplot <- ggplot(frame, aes(x=independentVariable, y=weightSpectrum))+ # plotting region for the spectrum
        geom_line(aes(color="blue"))+
        geom_ribbon(aes(ymax=weightSpectrumMax, ymin=weightSpectrumMin), alpha=0.5)

gplot

# for the sd as ribbon I need to compute the lims, for each point. I guess it is like, write another
# function with mean-sd and one with mean+sd coefs, and apply them, and use the outcoming series as 
# limits for the ribbon. it should in theory make sense. 

# intercept <- numeric(length(spectrum_metrics)) 
# first_coef <- numeric(length(spectrum_metrics)) 
# second_coef <- numeric(length(spectrum_metrics))
# for (i in 1:length(spectrum_metrics)) { # loop to merge 
#         intercept[i] <- spectrum_metrics[[i]][1]
#         first_coef[i] <- spectrum_metrics[[i]][2]
#         second_coef[i] <- spectrum_metrics[[i]][3]
#         val <- data.frame(intercept, first_coef, second_coef) # frames the obtained vectors
# }
# population_intercept <- c(mean(val[,1]), sd(val[,1])) # calculates the mean and the sd over the slopes
# population_intercept <- data.frame(population_intercept[1], population_intercept[2]) 
# colnames(population_intercept) <- c("Mean intercept", "Standard deviation intercept") 
# population_a <- c(mean(val[,2]), sd(val[,2]))
# population_a <- data.frame(population_a[1], population_a[2]) 
# colnames(population_a) <- c("Mean a", "Standard deviation a") 
# population_b <- c(mean(val[,3]), sd(val[,3])) 
# population_b <- data.frame(population_b[1], population_b[2]) 
# colnames(population_b) <- c("Mean b", "Standard deviation b") 
# 
# population_intercept # prints the results 
# population_a
# population_b
# population_coefs<-as.numeric(unlist(c(population_intercept[1], population_a[1], population_b[1])))

# plotter region

# freqs <- lapply(data_list, frequencies) 
# listbreaks<-numeric(length=length(freqs))
# for (i in 1:length(freqs)) {
#         listbreaks[i]<-length(freqs[[i]][,1]) 
# }
# maxlength <- match(max(listbreaks), listbreaks) # extracts the index of the largest break
# runs <- list()
# for (j in 1:length(freqs)) { # what the hell is this???
#         runs[[j]] <- freqs[[j]][,2]
#         runs[[j]] <- c(runs[[j]], rep(0, max(listbreaks)-length(runs[[j]])))
# }
# runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE)
# runs[is.na(runs)] <- 0 # gets rid of NAs, check if legit lol
# mean_runs <- apply(runs, 1, mean)
# ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs) # data frame containing all the runs and the largest 
# # breaks sequence. shorter lines filled with zeros. now need average and plot and fit of the lm
# 
# colnames(ensemble)<-c("ln_length","ln_freq")
# fitting <- function(ln_length){population_coefs[1]+population_coefs[2]*ln_length+population_coefs[3]*ln_length^2} # stores the function
# 
# #lin_extra <- lm(ensemble$ln_freq~ensemble$ln_length)
# pol_extra <- lm(ensemble$ln_freq~ensemble$ln_length+I(ensemble$ln_length^2)) # fit model to the MEAN freq-class
# pol_extraCube <- lm(ensemble$ln_freq~ensemble$ln_length+I(ensemble$ln_length^2)++I(ensemble$ln_length^3))
# summary(pol_extra)
# coefs_extra <- as.numeric(coef(pol_extra)) # extracts the coefficients of the quadratic model
# coefs_extraCube <- as.numeric(coef(pol_extraCube)) # extracts the coefficients of the quadratic model
# length_extra <- ensemble$ln_length
# fitting2 <- function(length_extra){coefs_extra[1]+coefs_extra[2]*length_extra+coefs_extra[3]*length_extra^2} # stores the function
# fittingCube <- function(ln_length){coefs_extraCube[1]+coefs_extraCube[2]*ln_length+
#                                            coefs_extraCube[3]*ln_length^2+coefs_extraCube[4]*ln_length^3} # stores the function

#fitting2 <- spectrum built on the new curve instead

# ggplotter

gplot <- ggplot(ensemble, aes(x=ln_length, y=ln_freq))+
        geom_point(shape=1)+
        stat_function(fun = fittingCube, geom="line", colour = "blue")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(0,11,1),
                           limits=c(0,11), labels=c(0:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,12),
                           breaks=c(0:12))+
        labs(title="Community weight spectrum")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

gplot

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/testOutput/Community weight spectrum.pdf", gplot, useDingbats=FALSE )
