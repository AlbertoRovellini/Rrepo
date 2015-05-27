# this is a script to deal with the output data of the simulations. first step is some exploratory analysis
# to have an idea of the dynamics in the community. the output file to be used here is the general one,
# just alike the old .csv tables. the new one will include biomass as well for each species at each time step,
# but the processing is just the same thing.

library(ggplot2)
library(scales)
library(reshape2)
setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype/spectrum")
data <- read.csv("working_tot.csv", header=T, sep="\t", dec=",")
data <- data[,c(1,4,6,8,10,12,14,16,18,20)]
normal_scientific<-expression(0,10,10^2,10^3,10^4)
colnames(data)<-c("Time","smallpelagic","mediumpelagic","largepelagic","smalldemersal","mediumdemersal", 
                  "largedemersal", "mediumgrazer","largegrazer","topcarnivores") # changes the names of the columns
data <- melt(data, id.vars="Time")
p<-ggplot(subset(data, variable=="smallpelagic" | variable=="mediumpelagic" | 
                         variable== "largepelagic" | variable== "smalldemersal" | variable== "mediumdemersal" | 
                         variable== "largedemersal" | variable== "mediumgrazers" | variable== "largegrazers" | 
                         variable== "topcarnivores"), aes(x=Time, y=value, colour=variable))+
        geom_line()+ 
        labs(title = "Populations", 
             x="Time steps", 
             y="Abundance")+
        scale_color_manual(values=c("blue","red", "green","yellow","orange", "grey", "magenta", "black", "grey"), 
                           name="Populations")+
        scale_x_discrete("Time steps", breaks=seq(0,2000,by=250), expand=c(0,0)) +
        scale_y_continuous(name="Total number of individuals", 
                           limits=c(1,50000),
                           breaks=c(0,10,100,1000,10000), 
                           expand=c(0,0), labels=normal_scientific)+
        coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(plot.title = element_text(size=16, vjust=2))+
        theme(axis.title.x = element_text(size=14,vjust=-0.5),
              axis.title.y = element_text(size=14,vjust=0.5))+
        theme(legend.title = element_text(size=14))+
        theme(axis.text.x=element_text(size=14))+
        theme(axis.text.y=element_text(size=14))
p

# horrible to look at, but it works fine. very basic version, no log workarounds (no zeroes). 
# note: this script is appliable only to one single run. this means that the script to be applied to the directory
# containing the batched output will require a preamble (best case scenario) to apply the code to every run, and then
# an averaging protocol before to get into the plotting phase




