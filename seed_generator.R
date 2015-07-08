# script to write automatically a list of seeds to be given to the automation parameters to run batches of replicates
# with the same parameters and varying only in the seed.

string <- as.character('<value xsi:type="xs:int" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">12</value>')
gregexpr(pattern ='12', string)
seeds <- seq(1,400,2)
seedList <- list()
for (i in 1:length(seeds)) {
        seedList[[i]] <- gsub("12", as.character(seeds[i]), string)  
}
seedList <- unlist(seedList)


fileConn<-file("C:/Users/Alberto/Documents/MASTER THESIS/itn_e/parameters/output.txt")
writeLines(seedList, fileConn)
close(fileConn)

