library(ggplot2)
t <- 1:1000

mygrowth <- function(x) {
        asy <- 300+300*0.01
        y <- NULL
        
        for (i in 1:length(x)) {
                if (x[i]<300) {
                        y[i] <- x[i]+x[i]*0.01
                } else {
                        y[i] <- asy
                }
                y
        }
        return(y)
}

vBgrowth <- function(x) {
        q <- 0.0125
        k <- 0.01
        tzero <- -0.2
        z <- NULL
        for (i in 1:length(x)) {
                z[i] <- q*29^3 * (1-exp(-k*(x[i]-tzero)))^3
        }
        return(z)
}

ayyy <- mygrowth(t)
lmao <- vBgrowth(t)
frame <- data.frame(rep(t,2), c(ayyy,lmao), c(rep("ITN linear", length(t)), rep("vonBertalanffy", length(t))))
colnames(frame) <- c("Time","Mass","Model") 

p <- ggplot(data=frame, aes(x=Time, y=Mass, group=Model))+
        geom_line(aes(linetype=Model))+
        scale_linetype_manual(values=c("solid","longdash"))+
        scale_y_continuous(limits=c(0,400))+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

p

ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/growth.pdf", p, useDingbats=FALSE )


