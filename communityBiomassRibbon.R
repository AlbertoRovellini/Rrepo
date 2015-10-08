# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to average the pupolation abundances over different runs of the same batch of simulations,
# calculate their standard deviation and plot them with the error.
# needs as input the "total.n" files.

setwd("C:/Users/Alberto/Documents/itn100results/resultsBase/tot/half")
library(abind)
library(reshape)
library(ggplot2)
list<-list.files("C:/Users/Alberto/Documents/itn100results/resultsBase/tot/half", 
                 recursive=TRUE, pattern="*.csv") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', nrow=2000) # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) { # should become a plyr function
        matcol[[i]]<-data_list[[i]][,c(1,5,7,9,11,13,15,21)] # list of matrix containing data of interest: time and classes for each .csv
}
all.matrix <- abind(matcol, along=3) # change the structure of the matrix matcol in order to use the function apply on it
mean_all <- apply(all.matrix, c(1,2), mean) # calculates the mean number of individuals in every position
sd_all <- apply(all.matrix, c(1,2), sd) # and its sd
sd_all[,1] <- mean_all[,1]

colnames(mean_all)<-c("Time","smallpelagic","mediumpelagic","largepelagic","smalldemersal",
                      "mediumdemersal","largedemersal","toppiscivores") # changes the names of the columns
colnames(sd_all)<-c("Time","smallpelagicSD","mediumpelagicSD","largepelagicSD","smalldemersalSD",
                    "mediumdemersalSD","largedemersalSD", "toppiscivoresSD") # for sd as well
mean_all<-as.data.frame(mean_all) # turns the matrix into a data frame
sd_all<-as.data.frame(sd_all) # same
mean_all[,c(2:length(mean_all))] <- mean_all[,c(2:length(mean_all))] + 1 # workaround for the log scale (either this or NAs, no big difference)

normal_scientific<-expression(0,10,10^2,10^3,10^4,10^5) # notation to be used in the plot
years <- seq(0,40,5)


meltMean <- melt(mean_all, id.vars = "Time", variable.name="variable", value.name="value")
meltSd <- melt(sd_all, id.vars = "Time", variable.name="variable", value.name="value")
complete <- data.frame(meltMean, meltSd[,3])
colnames(complete) <- c("time", "class", "mean", "sd")


ribbonBiomass <- ggplot(complete, aes(x=time, y=mean, color=class))+
        geom_line(aes(color=class))+
        #geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, color=class), alpha=0.5)+
        labs(#title = "Population dynamics of the community", 
                x="Time steps", 
                y="Abundance [individuals]")+
        scale_x_discrete("t [years]", breaks=seq(0,2000,by=250), labels=years, expand=c(0,0)) +
        scale_y_continuous(name="Biomass [kg]", 
                           limits=c(1000,100000000),
                           breaks=c(1000,10000,100000,1000000,10000000,100000000), 
                           expand=c(0,0), labels=normal_scientific)+
        scale_colour_manual(name="Class",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                     "Medium demersal", "Large demersals", "Top piscivores"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
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

ribbonBiomass


ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/communityBiomass.pdf", ribbonBiomass, useDingbats=FALSE ) # set better res pls




