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
setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individualPostHurricane")
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/prototype/out/individualPostHurricane", 
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
        breaks <- seq(0, ceiling(max(mass))+10, 5) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        freq[freq<3] <- NA # lower limit of resoulution, gets rid of the outliers. kek
        log_freq <- log(freq) # lognorm transformation of the frequency data
        log_length <- log(length_classes) # lognormal transformation of the length classes
        log_df <- data.frame(log_length, log_freq)
        pol_2 <- lm(log_freq~log_length+I(log_length^2))
        coefs <- as.numeric(coef(pol_2)) # extracts the coefficients of the quadratic model
        coefs
       
}
frequencies <-function(data) { # function to extract the frequencies for each replicate
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass))+10, 5) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        freq[freq<3] <- NA # lower limit of resoulution, gets rid of the outliers. kek
        ln_freq <- log(freq) # lognorm transformation of the frequency data
        ln_length <- log(length_classes) # lognormal transformation of the length classes
        freq_breaks <- data.frame(ln_length, ln_freq)
        
}

spectrum_metrics <- lapply(data_list, size_spectrum) # applies the function to all the element of the data list (i.e. to each run)
intercept <- numeric(length(spectrum_metrics)) 
first_coef <- numeric(length(spectrum_metrics)) 
second_coef <- numeric(length(spectrum_metrics))
for (i in 1:length(spectrum_metrics)) { # loop to merge 
        intercept[i] <- spectrum_metrics[[i]][1]
        first_coef[i] <- spectrum_metrics[[i]][2]
        second_coef[i] <- spectrum_metrics[[i]][3]
        val <- data.frame(intercept, first_coef, second_coef) # frames the obtained vectors
}
population_intercept <- c(mean(val[,1]), sd(val[,1])) # calculates the mean and the sd over the slopes
population_intercept <- data.frame(population_intercept[1], population_intercept[2]) # why even df??
colnames(population_intercept) <- c("Mean intercept", "Standard deviation intercept") # ayyyyyy lmao, one moar line
population_a <- c(mean(val[,2]), sd(val[,2])) # stop
population_a <- data.frame(population_a[1], population_a[2]) # pls
colnames(population_a) <- c("Mean a", "Standard deviation a") # what are u doing
population_b <- c(mean(val[,3]), sd(val[,3])) # stahp
population_b <- data.frame(population_b[1], population_b[2]) # ayyy
colnames(population_b) <- c("Mean b", "Standard deviation b") 

population_intercept # prints the results 
population_a
population_b
population_coefs<-as.numeric(unlist(c(population_intercept[1], population_a[1], population_b[1])))

# plotter region

freqs <- lapply(data_list, frequencies) 
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
runs[is.na(runs)] <- 0 # gets rid of NAs, check if legit lol
mean_runs <- apply(runs, 1, mean)
ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs) # data frame containing all the runs and the largest 
# breaks sequence. shorter lines filled with zeros. now need average and plot and fit of the lm

colnames(ensemble)<-c("ln_length","ln_freq")
fitting <- function(ln_length){population_coefs[1]+population_coefs[2]*ln_length+population_coefs[3]*ln_length^2} # stores the function

#lin_extra <- lm(ensemble$ln_freq~ensemble$ln_length)
pol_extra <- lm(ensemble$ln_freq~ensemble$ln_length+I(ensemble$ln_length^2))
summary(pol_extra)
coefs_extra <- as.numeric(coef(pol_extra)) # extracts the coefficients of the quadratic model
length_extra <- ensemble$ln_length
fitting2 <- function(length_extra){coefs_extra[1]+coefs_extra[2]*length_extra+coefs_extra[3]*length_extra^2} # stores the function

#fitting2 <- spectrum built on the new curve instead

# ggplotter

gplot <- ggplot(ensemble, aes(x=ln_length, y=ln_freq))+
        geom_point(shape=1)+
        stat_function(fun = fitting2, geom="line", colour = "blue")+
        scale_x_continuous("ln(weight class [5g])", breaks=seq(0,12,1),
                           limits=c(0,12), labels=c(0:12))+
        scale_y_continuous(name="ln(number of Superindividuals)", 
                           limits=c(0,12),
                           breaks=c(0:12))+
        labs(title="Community weight spectrum")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

gplot
# saver is still missing, dang