library(reshape)
setwd("C:/Users/Alberto/Documents/itn100results/input/finalAbundanceInput")

class=2

resultsBase <- read.table("resultsBase.csv", header=TRUE, sep=' ', dec='.')[,class] # mind the sep plz
unselective_i1 <- read.table("unselective_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
unselective_i2 <- read.table("unselective_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
unselective_i3 <- read.table("unselective_i3.csv", header=TRUE, sep=' ', dec='.')[,class]
size500_i1 <- read.table("size500_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
size500_i2 <- read.table("size500_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
size500_i3 <- read.table("size500_i3.csv", header=TRUE, sep=' ', dec='.')[,class]
class_i1 <- read.table("class_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
class_i2 <- read.table("class_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
class_i3 <- read.table("class_i3.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed_i1 <- read.table("mixed_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed_i2 <- read.table("mixed_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed_i3 <- read.table("mixed_i3.csv", header=TRUE, sep=' ', dec='.')[,class]

#extra scenarios
size250_i1 <- read.table("size250_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
size250_i2 <- read.table("size250_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
size250_i3 <- read.table("size250_i3.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed250_i1 <- read.table("mixed250_i1.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed250_i2 <- read.table("mixed250_i2.csv", header=TRUE, sep=' ', dec='.')[,class]
mixed250_i3 <- read.table("mixed250_i3.csv", header=TRUE, sep=' ', dec='.')[,class]

scenariosFrame <-  as.data.frame(cbind(resultsBase, unselective_i1, unselective_i2, unselective_i3,
                        size500_i1, size500_i2, size500_i3,
                        class_i1, class_i2, class_i3,
                        mixed_i1, mixed_i2, mixed_i3,
                        size250_i1, size250_i2, size250_i3,
                        mixed250_i1, mixed250_i2, mixed250_i3))
colnames(scenariosFrame) <- c("Base", "U_I1", "U_I2", "U_I3",
                              "S500_I1", "S500_I2", "S500_I3",
                              "C_I1", "C_I2", "C_I3",
                              "M_I1", "M_I2", "M_I3",
                              "S250_I1", "S250_I2", "S250_I3",
                              "M250_I1", "M250_I2", "M250_I3")
attach(scenariosFrame)


# FOLLOWS SECTION WITH SOME ROUTINES for stats

# isolates the frame of interest from the big one, for anova and kruskal

subsetFrame <- scenariosFrame[ , which(names(scenariosFrame) %in% c("M250_I1", "M250_I2", "M250_I3"))]

scenMelt <- melt(subsetFrame)

# run ANOVA, although technically I could not because both Shapiro's and Kolmogorov-Smirnov tests speak
# for deviations from normality. 
anova <- aov(value ~ variable, data=scenMelt)
summary(anova)

# runs the kruskal wallis, proper non-parametric 
kruskal <- kruskal.test(value ~ variable, data=scenMelt)
kruskal

# bonferroni factor for post-hoc tests between the samples (probably bullcrap)
bonferroni <- 0.05/ncol(subsetFrame)

# wilcoxon-mann-whitney (non-parameteric) for significant differences (H0 is equal mean)

wilcox.test(S250_I3, M250_I3)


# visual test for anova, no idea what I'm doing here
par(mfrow = c(2,2))
plot(anova) # for profis, not for me
