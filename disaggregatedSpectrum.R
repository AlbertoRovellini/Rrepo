
library(ggplot2)
setwd("C:/Users/Alberto/Documents/itn100results/resultsBase/ind/demo")
list<-list.files("C:/Users/Alberto/Documents/itn100results/resultsBase/ind/demo", 
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
        #freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),]
        
}

classWriter <- function(data) {
        mass <- data$biomass 
        breaks <- seq(0, ceiling(max(mass))+10, 20) # sets the breaks ranging over the biomass of the individuals
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # workaround, not sure if legit yet
        
        dataSort <- data[order(data$biomass),]
        
        dominantClasses <- list()
        bin <- list()
        occurrences <- list()
        dominant <- numeric(length=length(length_classes))
        
        
        for (i in 1:length(length_classes)) { 
                #for (j in 1:length(length_classes)) {
                bin[[i]] <- dataSort[dataSort$biomass>length_classes[i]-2 & dataSort$biomass<=length_classes[i],]
                occurrences[[i]]<-table(unlist(bin[[i]]$class))
                if (max(occurrences[[i]])==0) {
                        dominant[[i]] <- NA
                } else {
                        dominant[i] <- names(occurrences[[i]][(which(occurrences[[i]]==max(occurrences[[i]])))])
                }
                #dominant}
        }
        return(dominant)
        
#         freq_breaks$dom <- dominant # I don't even know how but somehow apparently I made it
#         
#         freq_breaks$dom <- gsub("smallpelagic", 1, freq_breaks[,3])
#         freq_breaks$dom <- gsub("mediumpelagic", 2, freq_breaks[,3])
#         freq_breaks$dom <- gsub("largepelagic", 3, freq_breaks[,3])
#         freq_breaks$dom <- gsub("smalldemersal", 4, freq_breaks[,3])
#         freq_breaks$dom <- gsub("mediumdemersal", 5, freq_breaks[,3])
#         freq_breaks$dom <- gsub("largedemersal", 6, freq_breaks[,3])
#         freq_breaks$dom <- gsub("topcarnivores", 7, freq_breaks[,3])
#         freq_breaks$dom <- as.numeric(freq_breaks$dom)
        
}

# plotter region

dominantClasses <- lapply(data_list, classWriter) # first line has a problem, will need to check

freqs <- lapply(data_list, frequencies) 

# routine to make all the frames of the same length

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

# same as above for the names

for (i in 1:length(dominantClasses)) {
        listbreaks[i]<-length(dominantClasses[[i]]) 
}
maxlength <- match(max(listbreaks), listbreaks) # extracts the index of the largest break
classNames <- list()
for (j in 1:length(dominantClasses)) { 
        classNames[[j]] <- dominantClasses[[j]]
        classNames[[j]] <- c(classNames[[j]], rep(NA, max(listbreaks)-length(classNames[[j]])))
}

classEnumerator <- function(x) {
        x <- gsub("smallpelagic", 1, x)
        x <- gsub("mediumpelagic", 2, x)
        x <- gsub("largepelagic", 3, x)
        x <- gsub("smalldemersal", 4, x)
        x <- gsub("mediumdemersal", 5, x)
        x <- gsub("largedemersal", 6, x)
        x <- gsub("topcarnivores", 7, x)
        x <- as.numeric(x)
}

dominantClasses <- lapply(classNames, classEnumerator)
refined <- list()
for (i in (1:length(runs))) {
        refined[[i]] <- data.frame(runs[[i]], dominantClasses[[i]])
        refined
}
head(refined[[1]])
head(refined[[2]])


# runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE)
# runs[is.na(runs)] <- 0 # gets rid of NAs, check if legit lol

library("abind")

all.matrix <- abind(refined, along=3)
mean_runs <- apply(all.matrix, c(1,2), mean, na.rm=TRUE) # gotcha
sd_runs <- apply(all.matrix, c(1,2), sd) # gotcha

Mode <- function(x) { 
        unlist(which.max(table(x)))
        
}

domList <- list() # workaround, creates a list of the dom vectors
for (i in 1:length(refined)) {
        domList[[i]] <- refined[[i]][,2]
}

is.integer0 <- function(x) # function to catch the integer(0) elements
{
        is.integer(x) && length(x) == 0L
}

modesDoms <- as.data.frame(abind(domList, along=2)) # binds the dom vectors in a dataframe
modesDoms[is.na(modesDoms)] <- NaN # turns the NAs in NaNs, actually useless

modesDom <- apply(modesDoms, 1, Mode) # fucks up due to Nas

modesDomMod <- list()
for (i in 1:length(modesDom)) {
        if (is.integer0(modesDom[[i]])==TRUE) {
                modesDomMod[[i]] <- NA
        } else { modesDomMod[[i]] <- modesDom[[i]]}
}
modesDomMod <- unlist(modesDomMod)


mode_runs <- apply(all.matrix, c(1,2), Mode, na.rm=TRUE)

ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs, sd_runs) # data frame containing all the runs and the largest 
# breaks sequence. shorter lines filled with zeros. now need average and plot and fit of the lm

colnames(ensemble)<-c("ln_length","ln_freq", "dom", "sd")
ensemble$sd[ensemble$sd==0] <- NA
ensemble$dom <- round(ensemble$dom)
ensemble$dom <- c(ensemble$dom[2:length(ensemble$dom)], NaN)



#fitting <- function(ln_length){population_coefs[1]+population_coefs[2]*ln_length+population_coefs[3]*ln_length^2} # stores the function



# fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length+I(ensemble$ln_length^2), 
#                    weights=1/(ensemble$sd^2))
# plot(ensemble$ln_length, ensemble$ln_freq)
# newx = data.frame(bin = ensemble$ln_length)
# pred <- predict(fitExperiment,newdata=newx)
# pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd)
# 
# pdat <- with(data.frame(pred),
#              data.frame(x = newx, y = fitExperiment))

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Set1")

write.table(ensemble, "C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput/col")


p <- ggplot(subset(ensemble, dom==1 | dom==2| dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, colour= factor(dom))) +
        geom_point(aes(size=2, position="jitter")) +
        scale_x_continuous("ln(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(3,11), labels=c(3:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_colour_manual(name="Functional groups",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                    "Medium demersal", "Large demersals", "Top carnivores"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
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


investigate <- data_list[[1]]
investigateSd <- investigate[investigate$class.=="smalldemersal",]
domDem <- dominantClasses[[1]]