#Restart R without Saving if Packages Crash

getwd()
setwd("~/Desktop/ANS198")
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
force=TRUE
install.packages("ggplot2")
library(ggplot2)
library(ggbiplot)
list.files()
SuperstoreData= read.csv("Sample - Superstore.csv")

SuperstoreData$Totaldiscount = SuperstoreData$Sales * SuperstoreData$Discount


PCADATA = prcomp(SuperstoreData[,c(18,19,21,22)], center = TRUE,scale. = TRUE)
summary(PCADATA)


ggbiplot(PCADATA, groups = SuperstoreData$Discount)

