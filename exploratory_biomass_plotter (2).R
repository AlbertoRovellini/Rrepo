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
        read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list<-lapply(list, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:length.list)) {
        matcol[[i]]<-data_list[[i]][,seq(1, 21, 2)] # list of matrix containing data of interest: time and classes for each .csv
}
all.matrix <- abind(matcol, along=3) # change the structure of the matrix matcol in order to use the function apply on it
mean_all <- apply(all.matrix, c(1,2), mean) # calculates the mean number of individuals in every position
sd_all <- apply(all.matrix, c(1,2), sd) # and its sd
sd_all[,1] <- mean_all[,1]

colnames(mean_all)<-c("Time","total","smallpelagic","mediumpelagic","largepelagic","smalldemersal",
                      "mediumdemersal","largedemersal",
                      "mediumgrazer","largegrazer","topcarnivores") # changes the names of the columns
colnames(sd_all)<-c("Time","totalSD","smallpelagicSD","mediumpelagicSD","largepelagicSD","smalldemersalSD",
                    "mediumdemersalSD","largedemersalSD",
                    "mediumgrazerSD","largegrazerSD","topcarnivoresSD") # for sd as well
mean_all<-as.data.frame(mean_all) # turns the matrix into a data frame
sd_all<-as.data.frame(sd_all) # same
mean_all[,c(2:length(mean_all))] <- mean_all[,c(2:length(mean_all))] + 1 # workaround for the log scale (either this or NAs, no big difference)

comb <- merge(mean_all, sd_all, "Time") # one df with all the means and sd (necessary for the mapping of the plot)

# mapping of the limits for the errorbar plot, each mean +- its sd. one entry per class is necessary
limits0<-aes(ymax=comb$total+comb$totalSD, ymin=comb$total)
limits1<-aes(ymax=comb$smallpelagic+comb$smallpelagicSD, ymin=comb$smallpelagic) #-comb$smallpelagicSD)
limits2<-aes(ymax=comb$mediumpelagic+comb$mediumpelagicSD, ymin=comb$mediumpelagic) #-comb$mediumpelagicSD)
limits3<-aes(ymax=comb$largepelagic+comb$largepelagicSD, ymin=comb$largepelagic) #-comb$largepelagicSD)
limits4<-aes(ymax=comb$smalldemersal+comb$smalldemersalSD, ymin=comb$smalldemersal) #-comb$smalldemersalSD)
limits5<-aes(ymax=comb$mediumdemersal+comb$mediumdemersalSD, ymin=comb$mediumdemersal) #-comb$mediumdemersalSD)
limits6<-aes(ymax=comb$largedemersal+comb$largedemersalSD, ymin=comb$largedemersal) #-comb$largedemersalSD)
limits7<-aes(ymax=comb$mediumgrazer+comb$mediumgrazerSD, ymin=comb$mediumgrazer) #-comb$mediumgrazerSD)
limits8<-aes(ymax=comb$largegrazer+comb$largegrazerSD, ymin=comb$largegrazer) #-comb$largegrazerSD)
limits9<-aes(ymax=comb$topcarnivores+comb$topcarnivoresSD, ymin=comb$topcarnivores) #-comb$topcarnivoresSD)

mcomb <- melt(comb, id.vars="Time",variable.name= "variable", 
              value.name="value") # melt the dataset for ggplot

normal_scientific<-expression(10^3,10^4,10^5,10^6,10^7,10^8) # notation to be used in the plot

# plotting code
gplot <-ggplot(subset(mcomb, variable=="total" | variable=="smallpelagic" | variable=="mediumpelagic" | 
                              variable== "largepelagic" | variable== "smalldemersal" | variable== "mediumdemersal" | 
                              variable== "largedemersal" | variable== "mediumgrazer" | variable== "largegrazer" | 
                              variable== "topcarnivores"),
               aes(x=Time,y=value, color=variable))+
        geom_line()+ 
        labs(#title = "Biomass of the classes", 
             x="Time steps", 
             y="Biomass (kg)")+ # will need to be changed to tonnes
        scale_color_manual(values=c("black", "blue", "red","green", "yellow", "orange", "purple", 
                                       "pink", "grey", "darkred"), 
                              name="Class")+
        scale_x_discrete("Time steps", breaks=seq(0,2000,by=250), expand=c(0,0)) +
        scale_y_continuous(name="Biomass (kg)", 
                           limits=c(1000,100000000),
                           breaks=c(1000,10000,100000,1000000,10000000,100000000), 
                           expand=c(0,0), labels=normal_scientific)+
        coord_trans(y="log10")+
        #errorbars
        geom_errorbar(limits0, alpha=0.1, data=subset(mcomb, variable=="total"))+
        geom_errorbar(limits1, alpha=0.1, data=subset(mcomb, variable=="smallpelagic"))+
        geom_errorbar(limits2, alpha=0.1, data=subset(mcomb, variable=="mediumpelagic"))+
        geom_errorbar(limits3, alpha=0.1, data=subset(mcomb, variable=="largepelagic"))+
        geom_errorbar(limits4, alpha=0.1, data=subset(mcomb, variable=="smalldemersal"))+
        geom_errorbar(limits5, alpha=0.1, data=subset(mcomb, variable=="mediumdemersal"))+
        geom_errorbar(limits6, alpha=0.1, data=subset(mcomb, variable=="largedemersal"))+
        geom_errorbar(limits7, alpha=0.1, data=subset(mcomb, variable=="mediumgrazer"))+
        geom_errorbar(limits8, alpha=0.1, data=subset(mcomb, variable=="largegrazer"))+
        geom_errorbar(limits9, alpha=0.1, data=subset(mcomb, variable=="topcarnivores"))+
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

gplot
ggsave("C:/Users/Alberto/Documents/itn100results/R_output/base/tot/biomass.pdf", gplot, useDingbats=FALSE ) # set better res pls



