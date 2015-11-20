library(reshape)
setwd("C:/Users/Alberto/Documents/itn100results/input/boxplotInputNew")

class=2

resultsBase <- read.table("resultsBase.csv", header=TRUE, sep=',', dec='.') # mind the sep plz
unselective_i1 <- read.table("unselective_i1.csv", header=TRUE, sep=',', dec='.')
unselective_i2 <- read.table("unselective_i2.csv", header=TRUE, sep=',', dec='.')
unselective_i3 <- read.table("unselective_i3.csv", header=TRUE, sep=',', dec='.')
size500_i1 <- read.table("size500_i1.csv", header=TRUE, sep=',', dec='.')
size500_i2 <- read.table("size500_i2.csv", header=TRUE, sep=',', dec='.')
size500_i3 <- read.table("size500_i3.csv", header=TRUE, sep=',', dec='.')
class_i1 <- read.table("class_i1.csv", header=TRUE, sep=',', dec='.')
class_i2 <- read.table("class_i2.csv", header=TRUE, sep=',', dec='.')
class_i3 <- read.table("class_i3.csv", header=TRUE, sep=',', dec='.')
mixed_i1 <- read.table("mixed_i1.csv", header=TRUE, sep=',', dec='.')
mixed_i2 <- read.table("mixed_i2.csv", header=TRUE, sep=',', dec='.')
mixed_i3 <- read.table("mixed_i3.csv", header=TRUE, sep=',', dec='.')

#extra scenarios
size250_i1 <- read.table("size250_i1.csv", header=TRUE, sep=',', dec='.')
size250_i2 <- read.table("size250_i2.csv", header=TRUE, sep=',', dec='.')
size250_i3 <- read.table("size250_i3.csv", header=TRUE, sep=',', dec='.')
mixed250_i1 <- read.table("mixed250_i1.csv", header=TRUE, sep=',', dec='.')
mixed250_i2 <- read.table("mixed250_i2.csv", header=TRUE, sep=',', dec='.')
mixed250_i3 <- read.table("mixed250_i3.csv", header=TRUE, sep=',', dec='.')



scenariosList <-  list(resultsBase, unselective_i1, unselective_i2, unselective_i3,
                                       size500_i1, size500_i2, size500_i3,
                                       class_i1, class_i2, class_i3,
                                       mixed_i1, mixed_i2, mixed_i3,
                                       size250_i1, size250_i2, size250_i3,
                                       mixed250_i1, mixed250_i2, mixed250_i3)
pwsummary <- list()
pw <- list()
for (i in 1:length(scenariosList)) {
        pwsummary[[i]] <- wilcox.test(scenariosList[[1]][,2], scenariosList[[i]][,2])
        pw[[i]] <- pwsummary[[i]]$p.value
}

pw <- unlist(pw)
pvalues <- data.frame(c("Base", "U_I1", "U_I2", "U_I3",
                        "S500_I1", "S500_I2", "S500_I3",
                        "C_I1", "C_I2", "C_I3",
                        "M_I1", "M_I2", "M_I3",
                        "S250_I1", "S250_I2", "S250_I3",
                        "M250_I1", "M250_I2", "M250_I3"), pw)
colnames(pvalues) <- c("Scenario","wilcoxon_p")
write.csv(pvalues, "C:/Users/Alberto/Documents/itn100results/wilcoxon_IE.csv")





