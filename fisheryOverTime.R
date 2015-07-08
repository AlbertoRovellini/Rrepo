# script to read and process the fishery output

library(ggplot2)
library(reshape)
library(abind)

# data from the 10 scenario
setwd("C:/Users/Alberto/Desktop/itn_jar/out/firstTestFishery10new/fish")
list10<-list.files("C:/Users/Alberto/Desktop/itn_jar/out/firstTestFishery10new/fish", 
                 recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list10<-length(list10)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list10 <- lapply(list10, read.special)

total10 <- list()
for (i in 1:length(data_list10)) {
        total10[[i]] <- data.frame(c(1:nrow(data_list10[[i]])), data_list10[[i]])
        total10[[i]] <- total10[[i]][c(2:38),]
        total10[[i]] <- total10[[i]][,c(1,3)]
        colnames(total10[[i]]) <- c("Event", "Total")
}

all.matrix10 <- abind(total10, along=3)
allData10 <- as.data.frame(apply(all.matrix10, c(1,2), mean))
#meltAll10 <- melt(allData10, id.vars="Event", variable="variable", value="value")

# data from the 50 scenario

setwd("C:/Users/Alberto/Desktop/itn_jar/out/firstTestFishery50new/fish")
list50<-list.files("C:/Users/Alberto/Desktop/itn_jar/out/firstTestFishery50new/fish", 
                 recursive=TRUE, pattern=".csv") # lists all the file (might need to change to .csv)
length.list50<-length(list50)
data_list50 <- lapply(list50, read.special)

total50 <- list()
for (i in 1:length(data_list50)) {
        total50[[i]] <- data.frame(c(1:nrow(data_list50[[i]])), data_list50[[i]])
        total50[[i]] <- total50[[i]][c(2:38),]
        total50[[i]] <- total50[[i]][,c(1,3)]
        colnames(total50[[i]]) <- c("Event", "Total")
}

all.matrix50 <- abind(total50, along=3)
allData50 <- as.data.frame(apply(all.matrix50, c(1,2), mean))
#meltAll <- melt(allData, id.vars="Event", variable="variable", value="value")

# put them together

allData <- data.frame(allData10, allData50[,2])
colnames(allData) <- c("Event", "pressure10", "pressure50")
allDataMelt <- melt(allData, id.vars="Event")


trick<-expression(seq(0,500,100))


p<-ggplot(subset(allDataMelt, variable=="pressure10" | variable=="pressure50"),
          aes(x=Event, y=value, group=variable))+
        geom_line(aes(linetype=variable))+
        geom_point(aes(shape=variable, size=3))+
        scale_shape_manual(values=c(0,17))+
        scale_x_continuous("Years", breaks=seq(0,38,2),
                           limits=c(1,39), labels=seq(0,38,2), expand=c(0,0))+
        scale_y_continuous(name="Catch (kg)", 
                           limits=c(0,500000),
                           breaks=seq(0,500000,100000), 
                           expand=c(0,0), labels=seq(0,500,100))+
        #coord_trans(y="log10")+
        #theme(panel.background = element_rect(fill = 'white'))+
        #theme
        #theme_bw()+
        theme(panel.grid.minor = element_blank())+#, panel.grid.major = element_blank())+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))
p

# biomass calculator is missing
# besides, a barplot would be maybe better for the purpose of representing fisheries
# maybe even better a cumulative plot
# CVI still missing, need graphic representation

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/testOutput/first_fishery.pdf", p, useDingbats=FALSE )
