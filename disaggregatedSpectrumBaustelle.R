pilot <- head(data_list[[1]],50)
mass <- pilot$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
# community, which still has to be defined anyway
breaks <- seq(0, ceiling(max(mass)), 2) # sets the breaks ranging over the biomass of the individuals
length_classes <- breaks[2:(length(breaks))] # workaround, not sure if legit yet
cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE)
freq <- table(cat)
freq <- cbind(freq)
#freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
#freq[freq<3] <- NA # lower limit of resoulution, gets rid of the outliers. kek
ln_freq <- log(freq) # lognorm transformation of the frequency data
ln_length <- log(length_classes) # lognormal transformation of the length classes
freq_breaks <- data.frame(ln_length, ln_freq)
#freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),]

pilotSort <- pilot[order(pilot$biomass),]
# occurrences<-table(unlist(pilotSort$class))
# dominant <- names(occurrences[(which(occurrences==max(occurrences)))])
# dominant

dominantClasses <- list()
bin <- list()
occurrences <- list()
dominant <- numeric(length=length(length_classes))


for (i in 1:length(length_classes)) { 
        #for (j in 1:length(length_classes)) {
        bin[[i]] <- pilotSort[pilotSort$biomass>length_classes[i]-2 & pilotSort$biomass<=length_classes[i],]
        occurrences[[i]]<-table(unlist(bin[[i]]$class))
        if (max(occurrences[[i]])==0) {
                dominant[[i]] <- NA
        } else {
        dominant[i] <- names(occurrences[[i]][(which(occurrences[[i]]==max(occurrences[[i]])))])
        }
        #dominant}
}

freq_breaks$dom <- dominant # I don't even know how but somehow apparently I made it


# if (freq_breaks$dom=="smallpelagic") {
#         freq_breaks$dom <- 1   
# } else if (freq_breaks$dom=="smalldemersal") {
#         freq_breaks$dom <- 2   
# } else if (freq_breaks$dom=="mediumdemersal") {
#         freq_breaks$dom <- 3   
# } else {
#         freq_breaks$dom <- NA   
# }



# pilotSort[pilotSort$biomass>1 & pilotSort$biomass<70,]
# pilotSort
# binTest <- pilotSort[pilotSort$biomass>length_classes[2] & pilotSort$biomass<=length_classes[2+1],]
# occurrences1<-table(unlist(binTest$class))
# dominant <- names(occurrences1[(which(occurrences1==max(occurrences1)))])

freq_breaks$dom <- gsub("smallpelagic", 1, freq_breaks[,3])
freq_breaks$dom <- gsub("mediumpelagic", 2, freq_breaks[,3])
freq_breaks$dom <- gsub("largepelagic", 3, freq_breaks[,3])
freq_breaks$dom <- gsub("smalldemersal", 4, freq_breaks[,3])
freq_breaks$dom <- gsub("mediumdemersal", 5, freq_breaks[,3])
freq_breaks$dom <- gsub("largedemersal", 6, freq_breaks[,3])
freq_breaks$dom <- gsub("topcarnivores", 7, freq_breaks[,3])
freq_breaks$dom <- as.numeric(freq_breaks$dom)



