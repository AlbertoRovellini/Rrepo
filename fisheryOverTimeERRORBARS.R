# script to read and process the fishery output

library(ggplot2)
library(reshape)
library(abind)

# data from the 10 scenario
setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i1/fish")
list10<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i1/fish", 
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

total10 <- abind(total10, along=3)
mean10 <- as.data.frame(apply(total10, c(1,2), mean))
sd10 <- as.data.frame(apply(total10, c(1,2), sd))

grouper <- function (targetFrame) { # function to sum rows two by two
        apply(targetFrame, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, sum)) 
}

propagation <- function(q) sqrt(sum((q)^2))

sdQuadraticGrouper <- function (target) {
        apply(target, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, propagation))
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total10 <- grouper(mean10)
sdev10 <- sdQuadraticGrouper(sd10)
Event <- c(1:nrow(total10)) # vector with the number of years or events
eventWriter <- function(z) {z[,1]<- Event # function to substitute the first column of the frames
                            return(z)}
total10 <- eventWriter(total10)
sdev10 <- eventWriter(sdev10)     


##################################################################


# data from the 20 scenario

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i2/fish")
list20<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i2/fish", 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list20<-length(list20)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list20 <- lapply(list20, read.special)

total20 <- list()
for (i in 1:length(data_list20)) {
        total20[[i]] <- data.frame(c(1:nrow(data_list20[[i]])), data_list20[[i]])
        #total20[[i]] <- total20[[i]][c(2:38),]
        total20[[i]] <- total20[[i]][,c(1,3)]
        colnames(total20[[i]]) <- c("Event", "Total")
}

total20 <- abind(total20, along=3)
mean20 <- as.data.frame(apply(total20, c(1,2), mean))
sd20 <- as.data.frame(apply(total20, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total20 <- grouper(mean20)
sdev20 <- sdQuadraticGrouper(sd20)
Event <- c(1:nrow(total20)) # vector with the number of years or events
total20 <- eventWriter(total20)
sdev20 <- eventWriter(sdev20)

# data from the 50 scenario

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i3/fish")
list50<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i3/fish", 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list50<-length(list50)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list50 <- lapply(list50, read.special)

total50 <- list()
for (i in 1:length(data_list50)) {
        total50[[i]] <- data.frame(c(1:nrow(data_list50[[i]])), data_list50[[i]])
        #total50[[i]] <- total50[[i]][c(2:38),]
        total50[[i]] <- total50[[i]][,c(1,3)]
        colnames(total50[[i]]) <- c("Event", "Total")
}

total50 <- abind(total50, along=3)
mean50 <- as.data.frame(apply(total50, c(1,2), mean))
sd50 <- as.data.frame(apply(total50, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total50 <- grouper(mean50)
sdev50 <- sdQuadraticGrouper(sd50)
Event <- c(1:nrow(total50)) # vector with the number of years or events
total50 <- eventWriter(total50)
sdev50 <- eventWriter(sdev50)

# put them together

allData <- data.frame(total10, total20[,2], total50[,2])
allData1 <- allData[c(1:(nrow(allData)-2)),]
colnames(allData1) <- c("Event", "pressure10", "pressure20", "pressure50")
allDataMelt <- melt(allData1, id.vars="Event")

allsd <- data.frame(sdev10, sdev20[,2], sdev50[,2])
allsd1 <- allsd[c(1:(nrow(allsd)-2)),]
colnames(allsd1) <- c("Event", "sd10", "sd20", "sd50")
allsdMelt <- melt(allsd1, id.vars="Event")

limits10<-aes(ymax=allData1$pressure10+allsd1$sd10, ymin=allData1$pressure10-allsd1$sd10)
limits20<-aes(ymax=allData1$pressure20+allsd1$sd20, ymin=allData1$pressure20-allsd1$sd20) 
limits50<-aes(ymax=allData1$pressure50+allsd1$sd50, ymin=allData1$pressure50-allsd1$sd50) 


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
        #errorbars
        geom_errorbar(limits10, data=subset(allDataMelt, variable=="pressure10"), width=0.3, position="dodge")+
        geom_errorbar(limits20, data=subset(allDataMelt, variable=="pressure20"), width=0.3, position="dodge")+
        geom_errorbar(limits50, data=subset(allDataMelt, variable=="pressure50"), width=0.3, position="dodge")+
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

ggsave("C:/Users/Alberto/Documents/itn100results/R_output/mixed250/fish/tot.pdf", p, useDingbats=FALSE )
