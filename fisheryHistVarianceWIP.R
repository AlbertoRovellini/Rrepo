# script to read and process the fishery output

library(ggplot2)
library(reshape)
library(abind)

# data from the 10 scenario
setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective10/fish")
list10<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective50/fish", 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list10<-length(list10)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list10 <- lapply(list10, read.special)

total10 <- list()
for (i in 1:length(data_list10)) {
        total10[[i]] <- data.frame(c(1:nrow(data_list10[[i]])), data_list10[[i]])
        #total10[[i]] <- total10[[i]][c(2:38),]
        total10[[i]] <- total10[[i]][,c(1,3)]
        colnames(total10[[i]]) <- c("Event", "Total")
}

all.matrix10 <- abind(total10, along=3)
allData10 <- as.data.frame(apply(all.matrix10, c(1,2), mean))

grouper <- function (x) { # function to sum rows two by two
         tapply(x, (seq_along(x)-1) %/% 5, sum)
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

allData10 <- lapply(allData10, grouper)
Event <- c(1:nrow(allData10[[1]])) # vector with the number of years or events
eventWriter <- function(z) {z[,1]<- Event # function to substitute the first column of the frames
                            return(z)}
allData10 <- lapply(allData10, eventWriter)

##################################################################

# data from the 20 scenario

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective20/fish")
list20<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective20/fish", 
                   recursive=TRUE, pattern=".csv") # lists all the file (might need to change to .csv)
length.list20<-length(list20)
data_list20 <- lapply(list20, read.special)

total20 <- list()
for (i in 1:length(data_list20)) {
        total20[[i]] <- data.frame(c(1:nrow(data_list20[[i]])), data_list20[[i]])
        #total20[[i]] <- total20[[i]][c(2:38),]
        total20[[i]] <- total20[[i]][,c(1,3)]
        colnames(total20[[i]]) <- c("Event", "Total")
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total20 <- lapply(total20, grouper)
total20 <- lapply(total20, eventWriter)

##################################################################

all.matrix20 <- abind(total20, along=3)
allData20 <- as.data.frame(apply(all.matrix20, c(1,2), mean))
#meltAll <- melt(allData, id.vars="Event", variable="variable", value="value")

# data from the 50 scenario

setwd("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective50/fish")
list50<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/itn_fixed/itn_e/resultsSlow/resultsUnselective50/fish", 
                   recursive=TRUE, pattern=".csv") # lists all the file (might need to change to .csv)
length.list50<-length(list50)
data_list50 <- lapply(list50, read.special)

total50 <- list()
for (i in 1:length(data_list50)) {
        total50[[i]] <- data.frame(c(1:nrow(data_list50[[i]])), data_list50[[i]])
        #total50[[i]] <- total50[[i]][c(2:38),]
        total50[[i]] <- total50[[i]][,c(1,3)]
        colnames(total50[[i]]) <- c("Event", "Total")
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total50 <- lapply(total50, grouper)
total50 <- lapply(total50, eventWriter)

##################################################################

all.matrix50 <- abind(total50, along=3)
allData50 <- as.data.frame(apply(all.matrix50, c(1,2), mean))
#meltAll <- melt(allData, id.vars="Event", variable="variable", value="value")

# put them together

allData <- data.frame(allData10, allData20[,2], allData50[,2])
colnames(allData) <- c("Event", "pressure10", "pressure20", "pressure50")
allDataMelt <- melt(allData, id.vars="Event")


trick<-expression(seq(0,7000,1000))


p<-ggplot(subset(allDataMelt, variable=="pressure10" | variable=="pressure20" |variable=="pressure50"),
          aes(x=Event, y=value, group=variable))+
        geom_line(aes(linetype=variable))+
        geom_point(aes(shape=variable, size=3))+
        scale_shape_manual(values=c(0,17,5))+
        scale_x_continuous("Years", breaks=seq(1,20,1),
                           limits=c(0,21), labels=seq(1,20,1), expand=c(0,0))+
        scale_y_continuous("Catch (kg)", limits=c(0,7000000),
                           breaks=seq(0,7000000,1000000), 
                           expand=c(0,0), labels=seq(0,7000,1000))+
        #coord_trans(y="log10")+
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

# biomass calculator is missing
# besides, a barplot would be maybe better for the purpose of representing fisheries
# maybe even better a cumulative plot
# CVI still missing, need graphic representation

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/testOutput/test12072015/compareUnselective.pdf", p, useDingbats=FALSE )
