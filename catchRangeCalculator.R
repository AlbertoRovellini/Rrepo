# enter here the catch ranges of the classes which have one. calc +-20 in steps of 5. frame and return.

mediumpelagic <- 60
largepelagic <- 125
mediumdemersal <- 60
largedemersal <- 70
topcarnivores <- 130

catchRanges <- data.frame(mediumpelagic, largepelagic, mediumdemersal, largedemersal, topcarnivores)
varFactors <- c(seq(0.8,0.95,0.05), seq(1.05,1.2,0.05))
catchRangesVar <- list()
for (i in 1:length(varFactors)) {
        catchRangesVar[[i]]<- catchRanges*varFactors[i]
}

catchRangesVar <- abind(catchRangesVar, along=1)
rownames(catchRangesVar) <- varFactors

catchRangesVar

