# histogram of the revenues.
# need to represent 7 bars per subscenario, hence 12 subplots to be faceted. 
# last complete fishing cycle. biomass times CVI to have an idea of the revenue, avoid prices, find a new metric.
# need to draw one bar per species plus one total bar, the sum of all the others, if it makes sense. maybe better
# plot a line over that plot to see the trend, or draw another plot with the revenues pattern.
# time-independent. quite complex actually.

# input data: fishery files. to consider the last cycle, lazy option is to keep tha same script of the area script,
# and take the second to last line. 

# technically, I should calc the mean and sd for each cycle, and then do everything according to the theory of error
# propagation. it will be the next step though.

library(ggplot2)
library(reshape)
setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective10/fish")
list10<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective10/fish", 
                 recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list10<-length(list10)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list10 <- lapply(list10, read.special)

total10 <- list()
for (i in 1:length(data_list10)) {
        total10[[i]] <- data.frame(c(1:nrow(data_list10[[i]])), data_list10[[i]])
        colnames(total10[[i]]) <- c("Event", "Time", "Total","Smallpelagic","Mediumpelagic","Largepelagic",
                                  "Smalldemersal", "Mediumdemersal","Largedemersal","Mediumgrazer","Largegrazer",
                                  "Topcarnivore","NA1")
        drops <- c("NA1", "Time", "Mediumgrazer","Largegrazer")
        total10[[i]] <- total10[[i]][,!(names(total10[[i]]) %in% drops)]
}

grouper <- function (targetFrame) { # function to sum rows two by two
        apply(targetFrame, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, sum)) 
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total10 <- lapply(total10, grouper)
Event <- c(1:nrow(total10[[1]])) # vector with the number of years or events
eventWriter <- function(z) {z[,1]<- Event # function to substitute the first column of the frames
                            return(z)}
total10 <- lapply(total10, eventWriter)

##################################################################

library("abind")

all.matrix10 <- abind(total10, along=3)
allData10 <- as.data.frame(apply(all.matrix10, c(1,2), mean))
allData10 <- allData10[nrow(allData10)-1,]
CVI <- c(1,1,2,3,3,1,2,2,1) # build the cvi vector, nees to start from smallpel and be progressive. just a trial version now.
allData10 <- allData10*CVI
allData10[,2]<-sum(allData10[,c(3:length(allData10))])
allData10 <- allData10[,c(2:length(allData10))]
allData10$Regime <- "Unselective10"

### data 20

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective20/fish")
list20<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective20/fish", 
                 recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list20<-length(list20)
data_list20 <- lapply(list20, read.special)

total20 <- list()
for (i in 1:length(data_list20)) {
        total20[[i]] <- data.frame(c(1:nrow(data_list20[[i]])), data_list20[[i]])
        colnames(total20[[i]]) <- c("Event", "Time", "Total","Smallpelagic","Mediumpelagic","Largepelagic",
                                  "Smalldemersal", "Mediumdemersal","Largedemersal","Mediumgrazer","Largegrazer",
                                  "Topcarnivore","NA1")
        drops <- c("NA1", "Time", "Mediumgrazer","Largegrazer")
        total20[[i]] <- total20[[i]][,!(names(total20[[i]]) %in% drops)]
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total20 <- lapply(total20, grouper)
total20 <- lapply(total20, eventWriter)

##################################################################

all.matrix20 <- abind(total20, along=3)
allData20 <- as.data.frame(apply(all.matrix20, c(1,2), mean))
allData20 <- allData20[nrow(allData20)-1,]
allData20 <- allData20*CVI

allData20[,2]<-sum(allData20[,c(3:length(allData20))])
allData20 <- allData20[,c(2:length(allData20))]
allData20$Regime <- "Unselective20"

###data 50

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective50/fish")
list50<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective50/fish", 
                 recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list50<-length(list50)
data_list50 <- lapply(list50, read.special)

total50 <- list()
for (i in 1:length(data_list50)) {
        total50[[i]] <- data.frame(c(1:nrow(data_list50[[i]])), data_list50[[i]])
        colnames(total50[[i]]) <- c("Event", "Time", "Total","Smallpelagic","Mediumpelagic","Largepelagic",
                                  "Smalldemersal", "Mediumdemersal","Largedemersal","Mediumgrazer","Largegrazer",
                                  "Topcarnivore","NA1")
        drops <- c("NA1", "Time", "Mediumgrazer","Largegrazer")
        total50[[i]] <- total50[[i]][,!(names(total50[[i]]) %in% drops)]
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total50 <- lapply(total50, grouper)
total50 <- lapply(total50, eventWriter)

##################################################################

all.matrix50 <- abind(total50, along=3)
allData50 <- as.data.frame(apply(all.matrix50, c(1,2), mean))
allData50 <- allData50[nrow(allData50)-1,]
allData50 <- allData50*CVI
allData50[,2]<-sum(allData50[,c(3:length(allData50))])
allData50 <- allData50[,c(2:length(allData50))]
allData50$Regime <- "Unselective50"

allData <- rbind.data.frame(allData10, allData20, allData50) # nice one, I dind't know it


meltAllData <- melt(allData, id.vars="Regime")
plot <- ggplot(data=meltAllData, aes(x=Regime, y=value, fill=variable))+
        geom_bar(stat="identity", position="dodge", aes(fill=variable), width=0.8)+
        scale_x_discrete(labels=c(10,20,50),"Fishing intensity [% of total community biomass]")+
        scale_y_continuous(limits=c(0,7000000),
                           breaks=seq(0,7000000,500000), 
                           expand=c(0,0), labels=seq(0,7000,500), "Catch [kg]")+
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))

plot # meh it's fine. need dev doe.

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/testOutput/test12072015/fisheryHistUnselective.pdf", plot, useDingbats=FALSE )

