# # Written by Alberto Rovellini, 28/04/2015, ZMT Bremen, DE
# e-mail: alberto.rovellini@zmt-bremen.de

# this is a script to average the pupolation abundances over different runs of the same batch of simulations,
# calculate their standard deviation and plot them with the error.

setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype")
library(abind)
library(reshape)
library(ggplot2)
list<-list.files("C:/Users/Alberto/Documents/MASTER THESIS/prototype", 
                 recursive=TRUE, pattern="*.txt") #the key is the recursive argument
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t') # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) {
        matcol[[i]]<-data_list[[i]][,c(1,3,4,5,6,7)] # list of matrix containing data of interest: time and classes for each .csv
}
all.matrix <- abind(matcol, along=3) # change the structure of the matrix matcol in order to use the function apply on it
mean_all <- apply(all.matrix, c(1,2), mean) # calculates the mean number of individuals in every position
sd_all <- apply(all.matrix, c(1,2), sd) # and its sd
sd_all[,1] <- mean_all[,1]

colnames(mean_all)<-c("Time","class1","class2","class3","class4","class5") # changes the names of the columns
colnames(sd_all)<-c("Time","class1sd","class2sd","class3sd","class4sd","class5sd") # for sd as well
mean_all<-as.data.frame(mean_all) # turns the matrix into a data frame
sd_all<-as.data.frame(sd_all) # same

# mapping of the limits for the errorbar plot, each mean +- its sd. one entry per class is necessary
limits1<-aes(ymax=mean_all$class1+sd_all$class1sd, ymin=mean_all$class1-sd_all$class1sd)
limits2<-aes(ymax=mean_all$class2+sd_all$class2sd, ymin=mean_all$class2-sd_all$class2sd)
limits3<-aes(ymax=mean_all$class3+sd_all$class3sd, ymin=mean_all$class3-sd_all$class3sd)
limits4<-aes(ymax=mean_all$class4+sd_all$class4sd, ymin=mean_all$class4-sd_all$class4sd)
limits5<-aes(ymax=mean_all$class5+sd_all$class5sd, ymin=mean_all$class5-sd_all$class5sd)

comb <- merge(mean_all, sd_all, "Time") # one df with all the means and sd (necessary for the mapping of the plot)

mcomb <- melt(comb, id.vars="Time") # melt the dataset for ggplot

normal_scientific<-expression(0,10,10^2,10^3,10^4) # just a notation to be used in the plot

# plotting code
gplot <-ggplot(subset(mcomb, variable=="class1" | variable=="class2" | variable== "class3" | variable== "class4" | variable== "class5"),
          aes(x=Time,y=value, colour=variable))+
        geom_line()+ 
        labs(title = "Populations of preys,\npredators and top carnivores", 
             x="Time steps", 
             y="Abundance")+
        scale_color_manual(values=c("blue","red", "green","yellow","orange"), 
                           name="Populations")+
        scale_x_discrete("Time steps", breaks=seq(0,2000,by=250), expand=c(0,0)) +
        scale_y_continuous(name="Total number of individuals", 
                           limits=c(1,15000),
                           breaks=c(0,10,100,1000,10000), 
                           expand=c(0,0), labels=normal_scientific)+
        coord_trans(y="log10")+
        #errorbars
        geom_errorbar(limits1, alpha=0.1, data=subset(mcomb, variable=="class1"))+
        geom_errorbar(limits2, alpha=0.1, data=subset(mcomb, variable=="class2"))+
        geom_errorbar(limits3, alpha=0.1, data=subset(mcomb, variable=="class3"))+
        geom_errorbar(limits4, alpha=0.1, data=subset(mcomb, variable=="class4"))+
        geom_errorbar(limits5, alpha=0.1, data=subset(mcomb, variable=="class5"))+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))
gplot

# awful plot, seriously. I should take a look at that beautiful graphics in R thing. however, it works.
# serialized script to analyse the batch output.
# the code will need to be modified to deal with zero values
