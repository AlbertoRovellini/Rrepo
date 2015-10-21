# probably the worst, most inefficient and retarded script I ever wrote.
# script is not flexible, modifications input-depending. plots omit information. long, dumb regions.
# absolutely requires deep revision and substantial editing.

# edit: 09/10/2015. I must have had a bad day when I commented this. The script does not look
# that bad. However, the whole region of data frame modification should be condensed in a loop,
# or much better embedded functions should be written and lapply'd. also, the missing classes
# should be included in the final data frame, otherwise the plot has to be edited.

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_I3/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_I3/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', nrows=2001) # custom function to read the batches of .csv keeping the header
} # 2001 because the 2100 steps is an artifact for the last fishery cycle
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) { # should become a plyr function
        matcol[[i]]<-data_list[[i]][,c(4,6,8,10,12,14,20)] # list of matrix containing data of interest: time and classes for each .csv
}

foo <- function(x) {which(x==0)[1]}
extinctionDetector <- function(x) {
        if (sum(colSums(x==0)!=0)) {c(names(colSums(x==0)[colSums(x==0)!=0]))}else{"stable"}
        output <- unlist(apply(x,2,foo))
        return(output[!is.na(output)])
}

fails <- lapply(matcol, extinctionDetector)

for (i in 1:length(fails)) {
        if (length(fails[[i]])==0) {fails[[i]] <- "empty"}
}

fails <- fails[fails != "empty"] # most autistic method I could ever think of, but it kind of works

# analysis which won't account for extinctions occurred in the same run, just when and who
global <- unlist(fails)

# separates the classes from each other according to the number of collapses

smallpelagic <- global[names(global)=="smallpelagic.nr."]
mediumpelagic <- global[names(global)=="mediumpelagic.nr."]
largepelagic <- global[names(global)=="largepelagic.nr."]
smalldemersal <- global[names(global)=="smalldemersal.nr."]
mediumdemersal <- global[names(global)=="mediumdemersal.nr."]
largedemersal <- global[names(global)=="largedemersal.nr."]
topcarnivores <- global[names(global)=="topcarnivores.nr."]

# builds a data frame per class, with the name of the class and the time step where it got extinct

smallpelagic <- data.frame(rep("smallpelagic", length(smallpelagic)), smallpelagic, row.names=seq(1:length(smallpelagic)))
mediumpelagic <- data.frame(rep("mediumpelagic", length(mediumpelagic)), mediumpelagic, row.names=seq(1:length(mediumpelagic)))
largepelagic <- data.frame(rep("largepelagic", length(largepelagic)), largepelagic, row.names=seq(1:length(largepelagic)))
smalldemersal <- data.frame(rep("smalldemersal", length(smalldemersal)), smalldemersal, row.names=seq(1:length(smalldemersal)))
mediumdemersal <- data.frame(rep("mediumdemersal", length(mediumdemersal)), mediumdemersal, row.names=seq(1:length(mediumdemersal)))
largedemersal <- data.frame(rep("largedemersal", length(largedemersal)), largedemersal, row.names=seq(1:length(largedemersal)))
topcarnivores <- data.frame(rep("topcarnivores", length(topcarnivores)), topcarnivores, row.names=seq(1:length(topcarnivores)))

# orders the data.frames with progressive time step

smallpelagic <- smallpelagic[ order(smallpelagic[,2]),]
mediumpelagic <- mediumpelagic[ order(mediumpelagic[,2]),]
largepelagic <- largepelagic[ order(largepelagic[,2]),]
smalldemersal <- smalldemersal[ order(smalldemersal[,2]),]
mediumdemersal <- mediumdemersal[ order(mediumdemersal[,2]),]
largedemersal <- largedemersal[ order(largedemersal[,2]),]
topcarnivores <- topcarnivores[ order(topcarnivores[,2]),]

# adds a column with a progressive number

smallpelagic$prog <- seq(1:nrow(smallpelagic))
mediumpelagic$prog <- seq(1:nrow(mediumpelagic))
largepelagic$prog <- seq(1:nrow(largepelagic))
smalldemersal$prog <- seq(1:nrow(smalldemersal))
mediumdemersal$prog <- seq(1:nrow(mediumdemersal))
largedemersal$prog <- seq(1:nrow(largedemersal))
topcarnivores$prog <- seq(1:nrow(topcarnivores))

# renames the columns of the data frames

colnames(smallpelagic) <- c("class", "extinctions", "prog")
colnames(mediumpelagic) <- c("class", "extinctions", "prog")
colnames(largepelagic) <- c("class", "extinctions", "prog")
colnames(smalldemersal) <- c("class", "extinctions", "prog")
colnames(mediumdemersal) <- c("class", "extinctions", "prog")
colnames(largedemersal) <- c("class", "extinctions", "prog")
colnames(topcarnivores) <- c("class", "extinctions", "prog")

# binds all the frames together for ggplot

total <- rbind(smallpelagic, mediumpelagic, largepelagic, smalldemersal, mediumdemersal, 
               largedemersal, topcarnivores) # need to remove the vectors with one element

time <- 0:2000 # x-axis

library(ggplot2)

plot <- ggplot(data=total, aes(x=time))+
        geom_step(data=total, aes(x=extinctions, y=prog, color=class), direction="hv")+
        scale_color_manual(values=c("red",  "darkgreen", "darkgrey"
                                   ), name="Class")+
        # palette: "blue","red","darkgreen",yellow","orange","purple","darkgrey"
        scale_x_continuous("Time", breaks=seq(1000,2000,100),
                           limits=c(1000,2000), labels=seq(1000,2000,100))+
        scale_y_continuous(name="Cumulative extinctions in 100 runs", 
                           limits=c(0,100),
                           breaks=seq(0,100,10), labels=seq(0,100,10))+
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
plot

ggsave("C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/extinctions/cumulative_size250_i3.pdf", plotto, useDingbats=FALSE)


write.table(fails, "C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/scenarios/class/i3/extinctions.csv")

