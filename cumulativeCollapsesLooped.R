# 09/10/2015

# looped script, much more flexible and fast. still some issues:

# 1) the loops-functions might be reduced by grouping them in one single loop or function
# 2) a bug in geom_steps does not allow to deal with 1-data strings. a NA is introduced in the frames with only
# one data point, and this is enough to make the plot work, but that data is not displayed (no steps to show).
# need to point out in the captions that only the classes reporting >1 collapses over 100 runs are represented.

setwd("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot")
library(abind)
library(reshape)
list<-list.files("C:/Users/Alberto/Documents/itn100results/mixed250_i3/tot", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec=',', nrows=2001) # custom function to read the batches of .csv keeping the header
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

fails # checkpoint, if empty move to the next batch

for (i in 1:length(fails)) {
        if (length(fails[[i]])==0) {fails[[i]] <- "empty"}
}

fails <- fails[fails != "empty"] # most autistic method I could ever think of, but it kind of works

# analysis which won't account for extinctions occurred in the same run, just when and who
global <- unlist(fails)
allClasses <- c("smallpelagic.nr.","mediumpelagic.nr.","largepelagic.nr.",
                "smalldemersal.nr.","mediumdemersal.nr.","largedemersal.nr.",
                "topcarnivores.nr.")

# separates the classes from each other according to the number of collapses

newList <- list() # elfl

for (i in 1:length(allClasses)) { # if the class has no extinction, assing a string of 1 NA
        if (sum((names(global)==allClasses[i])[TRUE])>0) {
                newList[[i]] <- global[names(global)==allClasses[i]]
        } else {
                newList[[i]] <- c(NA,NA) # 2 because of a bug in geom_smooth
        }
}

# prolong the 1-data data frames with 1 NA, due to bug in ggplot (might not work)

for (i in 1:length(newList)) {
        if (length(newList[[i]])==1) {
                newList[[i]] <- c(newList[[i]],NA)
        } else {
                newList[[i]] <- newList[[i]]
        }
}

# sum((names(global)=="mediumpelagic.nr.")[TRUE])

frameList <- list() # elfl

for (i in 1:length(allClasses)) { # builds a frame per class with the extinctions
        frameList[[i]] <- data.frame(rep(allClasses[i], length(newList[[i]])), 
                                     newList[[i]], row.names=seq(1:length(newList[[i]])))
}

# order the frames according to the extinctions

orderer <- function(x) { # where x is one of the data frames of the list
        x <- x[ order(x[,2]),]
}

frameListOrdered <- lapply(frameList, orderer)

# insert a column of progressive numbers in each frame

enumerator <- function(y) { # where y is a frame of the list
        y$prog <- seq(1:nrow(y))
        return(y)
}

frameListEnumerated <- lapply(frameListOrdered, enumerator)

# name all the columns the same way to merge the frames later

namer <- function(z) { # where y is a frame of the list
        colnames(z) <- c("class", "extinctions", "prog")
        return(z)
}

finalList <- lapply(frameListEnumerated, namer)

# merge all the frames of the list

finalFrame <- do.call("rbind", finalList)

finalFrame # final checkpoint, look for single data

time <- 0:2000 # x-axis

library(ggplot2)

collapsePlot <- ggplot(data=finalFrame,
                  aes(x=extinctions, y=prog, color=class))+
        geom_step(direction="hv", size=0.8)+
        scale_x_continuous("Time", breaks=seq(1000,2000,250),
                           limits=c(1000,2000), labels=seq(1000,2000,250))+
        scale_y_continuous(name="Cumulative extinctions \n in 100 runs", 
                           limits=c(0,100),
                           breaks=seq(0,100,10), labels=seq(0,100,10))+
        scale_color_manual(name="Class",
                           values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"))+
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

collapsePlot

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/cumulativeCollapseM250_I3.pdf", collapsePlot, useDingbats=FALSE ) # set better res pls


#write.table(fails, "C:/Users/Alberto/Documents/MASTER THESIS/results/R_output/scenarios/class/i3/extinctions.csv")

