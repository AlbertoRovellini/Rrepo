a <- 0:20
b <- c(3,12,13,14,19)
d <- seq(1:length(b))
df <- data.frame(b,d)
df
plotto <- ggplot(data=df, aes(x=a))+
        geom_step(aes(x=b, y=d))
plotto
