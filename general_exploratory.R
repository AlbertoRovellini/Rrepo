# this is a script to deal with the output data of the simulations. first step is some exploratory analysis
# to have an idea of the dynamics in the community. the output file to be used here is the general one,
# just alike the old .csv tables. the new one will include biomass as well for each species at each time step,
# but the processing is just the same thing.

library(ggplot2)
library(scales)
library(reshape2)
setwd("C:/Users/Alberto/Documents/MASTER THESIS/prototype")
data <- read.csv("start_total.prototype.csv", header=T, sep="\t")
normal_scientific<-expression(0,10,10^2,10^3,10^4)
data <- melt(data, id.vars="steps.")
p<-ggplot(subset(data, variable=="class1." | variable=="class2." | variable== "class3." | variable== "class4." | variable== "class5."),
          aes(x=steps.,y=value, colour=variable))+
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

# horrible to look at, but it works fine. very basic version, no log workarounds (no zeroes). 
# note: this script is appliable only to one single run. this means that the script to be applied to the directory
# containing the batched output will require a preamble (best case scenario) to apply the code to every run, and then
# an averaging protocol before to get into the plotting phase




