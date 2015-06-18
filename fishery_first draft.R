# script to read and process the fishery output

library(ggplot2)
library(reshape)
setwd("C:/Users/Alberto/Documents/itn_jar/out")
list<-list.files("C:/Users/Alberto/Documents/itn_jar/out", 
                 recursive=TRUE, pattern="start_fishery.*") # lists all the file (might need to change to .csv)
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)

total <- list()
for (i in 1:length(data_list)) {
        total[[i]] <- data.frame(c(1:nrow(data_list[[i]])), data_list[[i]])
        colnames(total[[i]]) <- c("Event", "Time", "Total","Smallpelagic","Mediumpelagic","Largepelagic",
                                  "Smalldemersal", "Mediumdemersal","Largedemersal","Mediumgrazer","Largegrazer",
                                  "Topcarnivore","NA1")
        drops <- c("NA1", "Time")
        total[[i]] <- total[[i]][,!(names(total[[i]]) %in% drops)]
}

library("abind")

all.matrix <- abind(total, along=3)
allData <- as.data.frame(apply(all.matrix, c(1,2), mean))
meltAll <- melt(allData, id.vars="Event", variable="variable", value="value")

runOne <- total[[1]]
meltOne <- melt(runOne, id.vars="Event")
normal_scientific<-expression(0,10,10^2,10^3,10^4,10^5)


p<-ggplot(subset(meltAll, variable=="Total" | variable=="Smallpelagic" | variable=="Mediumpelagic" | 
                         variable== "Largepelagic" | variable== "Smalldemersal" | variable== "Mediumdemersal" | 
                         variable== "Largedemersal" | variable== "Mediumgrazer" | variable== "Largegrazer" | 
                         variable== "Topcarnivore"), aes(x=Event, y=value, colour=variable))+
        geom_line()+ 
        labs(title = "Populations", 
             x="Time steps", 
             y="Abundance")+
        scale_color_manual(values=c("blue","red", "darkgreen","yellow","orange", "darkgrey", "magenta", "black", "purple", "brown"), 
                           name="Groups")+
        scale_x_continuous("Years", breaks=seq(0,20,1),
                           limits=c(0,20), labels=seq(0,20,1), expand=c(0,0))+
        scale_y_continuous(name="Superindividuals", 
                           limits=c(1,50000),
                           breaks=c(0,10,100,1000,10000,100000), 
                           expand=c(0,0), labels=normal_scientific)+
        coord_trans(y="log10")+
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
# CVI still missing, need graphic representation

#ggsave("community.pdf", p, useDingbats=FALSE)