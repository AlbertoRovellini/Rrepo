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
setwd("C:/Users/Alberto/Documents/itn100results/size250_i3/ind/test10")
list<-list.files("C:/Users/Alberto/Documents/itn100results/size250_i3/ind/test10", 
                 recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec=',') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)
lastTimeStep <- function(data) subset(data, data$time=="2000.0") # isolates the last time step, comment out for complete analysis
data_list <- lapply(data_list, lastTimeStep)

frequencies <-function(data) { # function to extract the frequencies for each replicate
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass))+10, 20) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
        cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
        freq <- table(cat)
        freq <- cbind(freq)
        freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
        freq[freq<3] <- NA # lower limit of resoulution, gets rid of the outliers. kek
        ln_freq <- log(freq) # lognorm transformation of the frequency data
        ln_length <- log(length_classes) # lognormal transformation of the length classes
        freq_breaks <- data.frame(ln_length, ln_freq)
        freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),]
        
}

# plotter region

freqs <- lapply(data_list, frequencies) 
listbreaks<-numeric(length=length(freqs))
for (i in 1:length(freqs)) {
        listbreaks[i]<-length(freqs[[i]][,1]) 
}
maxlength <- match(max(listbreaks), listbreaks) # extracts the index of the largest break
runs <- list()
for (j in 1:length(freqs)) { # what the hell is this???
        runs[[j]] <- freqs[[j]][,2]
        runs[[j]] <- c(runs[[j]], rep(0, max(listbreaks)-length(runs[[j]])))
}
runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE)
runs[is.na(runs)] <- 0 # gets rid of NAs, check if legit lol
mean_runs <- apply(runs, 1, mean)
sd_runs <- apply(runs, 1, sd)
ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs, sd_runs) # data frame containing all the runs and the largest 
# breaks sequence. shorter lines filled with zeros. now need average and plot and fit of the lm

colnames(ensemble)<-c("ln_length","ln_freq", "sd")
ensemble$sd[ensemble$sd==0] <- NA
fitting <- function(ln_length){population_coefs[1]+population_coefs[2]*ln_length+population_coefs[3]*ln_length^2} # stores the function

# new stuff #

fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length+I(ensemble$ln_length^2), 
                   weights=1/(ensemble$sd^2))
plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx)
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd)

pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))

p <- ggplot(ensemble, aes(x = ln_length, y = ln_freq)) +
        #geom_point() +
        geom_line(data = pdat, aes(x=bin, y=pred), colour = "blue") + 
        geom_ribbon(data = pdat, mapping = aes(x=bin, y=pred, ymax = ymax, ymin = ymin),  
                    alpha = 0.4, fill = "grey60")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(0,11,1),
                           limits=c(0,11), labels=c(0:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(-2,16),
                           breaks=c(-2:16))+
        #labs(title="Community weight spectrum")+
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

p
coef(fitExperiment)
summary(fitExperiment)

#ggsave("C:/Users/Alberto/Documents/itn100results/R_output/unselective/ind/i3.pdf", p, useDingbats=FALSE )



# lines(newx$bin,predict(fitExperiment,newdata=newx)) # I do realize it's actually the exact same stuff I did
# 
# qplot(ensemble$ln_length,ensemble$ln_freq, geom='smooth', method = "lm", 
#       formula = y ~ x + I(x^2), span=1)+
#         geom_point()
# 
# 
# 
# 
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
# 
# #fitting2 <- spectrum built on the new curve instead
# 
# # ggplotter
# 
# gplot <- ggplot(ensemble, aes(x=ln_length, y=ln_freq))+
#         geom_point(shape=1)+
#         stat_function(fun = fitting2, geom="line", colour = "blue")+
#         scale_x_continuous("ln(weight class [20g])", breaks=seq(0,11,1),
#                            limits=c(0,11), labels=c(0:11))+
#         scale_y_continuous(name="ln(number of individuals)", 
#                            limits=c(0,12),
#                            breaks=c(0:12))+
#         #labs(title="Community weight spectrum")+
#         theme(panel.background = element_rect(fill = 'white'))+
#         #theme
#         theme_bw()+
#         theme(panel.grid.minor = element_blank(), 
#               panel.grid.major = element_line(linetype="dashed"))+
#         theme(plot.title = element_text(size=14, vjust=2))+
#         theme(axis.title.x = element_text(size=12,vjust=-0.5),
#               axis.title.y = element_text(size=12,vjust=0.5))+
#         theme(axis.text.x=element_text(size=12))+
#         theme(axis.text.y=element_text(size=12))
# 
# gplot

#ggsave("C:/Users/Alberto/Documents/MASTER THESIS/testOutput/test12072015/Community weight spectrum_selective500_10.pdf", gplot, useDingbats=FALSE )
