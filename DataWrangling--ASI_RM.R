## Import Data and Load Packages

library("rcompanion")
library("car")
library(dplyr)

## Use only rows with measurements for two time periods

ASI_RMT1T2 <- ASI_RMF[ASI_RMF$Time =="T1" | ASI_RMF$Time == "T2",]
View(ASI_RMT1T2)

## Drop variables that will not be needed 

ASI_RMSubset <- ASI_RMT1T2[, c(7:15,18:20)]
View(ASI_RMSubset)

## Limit correlation matrix to continuous variables to be used in analysis

keeps <- c("C1", "C2", "C3", "C4", "Ctotal", "DHEA", "P17", "TIgA", "GIgA")
ASI_RMSubsetKeeps <- ASI_RMSubset[keeps]
View(ASI_RMSubsetKeeps)

## Run correlation matrix of all potential dependent variables

CORRMATRIX<- cor(ASI_RMSubsetKeeps)
CORRMATRIX

### Use correlation matrix to determine whether to run MANOVA or multiple ANOVAS

## Check for normality for each dependent variable by Time (T1 and T2)

## Subset for T1 and T2

ASI_RMT1<-ASI_RMSubset[ASI_RMSubset$Time =="T1",]
ASI_RMT1                  
ASI_RMT2<-ASI_RMSubset[ASI_RMSubset$Time =="T2",]
ASI_RMT2
View(ASI_RMT1)
View(ASI_RMT2)

#C1T1
plotNormalHistogram(ASI_RMT1$C1)
ASI_RMT1$C1SQRT <- sqrt(ASI_RMT1$C1)
plotNormalHistogram(ASI_RMT1$C1SQRT)
ASI_RMT1$C1SQRT<- log(ASI_RMT1$C1)
plotNormalHistogram(ASI_RMT1$C1LOG)
View(ASI_RMT1)
# Log looks best

#C1T2
plotNormalHistogram(ASI_RMT2$C1)
ASI_RMT2$C1SQRT <- sqrt(ASI_RMT2$C1)
plotNormalHistogram(ASI_RMT2$C1SQRT)
ASI_RMT1$C1LOG<- log(ASI_RMT2$C1)
plotNormalHistogram(ASI_RMT2$C1LOG)
View(ASI_RMT2)
# Log won't run 

#C2T1
plotNormalHistogram(ASI_RMT1$C2)
ASI_RMT1$C2SQRT <- sqrt(ASI_RMT1$C2)
plotNormalHistogram(ASI_RMT1$C2SQRT)
ASI_RMT1$C2LOG <- log(ASI_RMT1$C2)
plotNormalHistogram(ASI_RMT1$C2LOG)
View(ASI_RMT1)
# Log is best but still positively skewed

#C2T2
plotNormalHistogram(ASI_RMT2$C2)
ASI_RMT2$C2SQRT <- sqrt(ASI_RMT2$C2)
plotNormalHistogram(ASI_RMT2$C2SQRT)
ASI_RMT2$C2LOG <- log(ASI_RMT2$C2)
plotNormalHistogram(ASI_RMT2$C2LOG)
View(ASI_RMT2)
# Log is best but still positively skewed

#C3T1
plotNormalHistogram(ASI_RMT1$C3)
ASI_RMT1$C3SQRT<-sqrt(ASI_RMT1$C3)
plotNormalHistogram(ASI_RMT1$C3SQRT)
ASI_RMT1$C3LOG<-log(ASI_RMT1$C3)
plotNormalHistogram(ASI_RMT1$C3LOG)
View(ASI_RMT1)
# Log is best and a little positively skewed

#C3T2
plotNormalHistogram(ASI_RMT2$C3)
ASI_RMT2$C3SQRT<-sqrt(ASI_RMT2$C3)
plotNormalHistogram(ASI_RMT2$C3SQRT)
ASI_RMT2$C3LOG<-log(ASI_RMT2$C3)
plotNormalHistogram(ASI_RMT2$C3LOG)
View(ASI_RMT2)
# Even with log very positively skewed

#C4T1
plotNormalHistogram(ASI_RMT1$C4)
ASI_RMT1$C4SQRT<-sqrt(ASI_RMT1$C4)
plotNormalHistogram(ASI_RMT1$C4SQRT)
ASI_RMT1$C4LOG <- log(ASI_RMT1$C4)
plotNormalHistogram(ASI_RMT1$C4LOG)
View(ASI_RMT1)
# Log is best

#C4T2
plotNormalHistogram(ASI_RMT2$C4)
ASI_RMT2$C4SQRT<-sqrt(ASI_RMT2$C4)
plotNormalHistogram(ASI_RMT2$C4SQRT)
ASI_RMT2$C4LOG <- log(ASI_RMT2$C4)
plotNormalHistogram(ASI_RMT2$C4LOG)
View(ASI_RMT2)
# Log is best but stil very positively skewed

#CTOTALT1
plotNormalHistogram(ASI_RMT1$Ctotal)
ASI_RMT1$CtotalSQRT<-sqrt(ASI_RMT1$Ctotal)
plotNormalHistogram(ASI_RMT1$CtotalSQRT)
ASI_RMT1$CtotalLOG<-log(ASI_RMT1$Ctotal)
plotNormalHistogram(ASI_RMT1$CtotalLOG)
View(ASI_RMT1)
# Log is best

#CTOTALT2
plotNormalHistogram(ASI_RMT2$Ctotal)
ASI_RMT2$CtotalSQRT<-sqrt(ASI_RMT2$Ctotal)
plotNormalHistogram(ASI_RMT2$CtotalSQRT)
ASI_RMT2$CtotalLOG<-log(ASI_RMT2$Ctotal)
plotNormalHistogram(ASI_RMT2$CtotalLOG)
View(ASI_RMT2)
# Even log is very postitively skewed

#DHEAT1
plotNormalHistogram(ASI_RMT1$DHEA)
ASI_RMT1$DHEASQRT<- sqrt(ASI_RMT1$DHEA)
plotNormalHistogram(ASI_RMT1$DHEASQRT)
ASI_RMT1$DHEALOG<- log(ASI_RMT1$DHEA)
plotNormalHistogram(ASI_RMT1$DHEALOG)
View(ASI_RMT1)
# Log is best

#DHEAT2
plotNormalHistogram(ASI_RMT2$DHEA)
ASI_RMT2$DHEASQRT<- sqrt(ASI_RMT1$DHEA)
plotNormalHistogram(ASI_RMT2$DHEASQRT)
ASI_RMT2$DHEALOG<- log(ASI_RMT1$DHEA)
plotNormalHistogram(ASI_RMT2$DHEALOG)
View(ASI_RMT2)
# Log is best

#P17T1
plotNormalHistogram(ASI_RMT1$P17)
ASI_RMT1$P17SQRT<- sqrt(ASI_RMT1$P17)
plotNormalHistogram(ASI_RMT1$P17SQRT)
ASI_RMT1$P17LOG<-log(ASI_RMT1$P17)
plotNormalHistogram(ASI_RMT1$P17LOG)
View(ASI_RMT1)
# Log is best

#P17T2
plotNormalHistogram(ASI_RMT2$P17)
ASI_RMT2$P17SQRT<- sqrt(ASI_RMT2$P17)
plotNormalHistogram(ASI_RMT2$P17SQRT)
ASI_RMT2$P17LOG<-log(ASI_RMT2$P17)
plotNormalHistogram(ASI_RMT2$P17LOG)
View(ASI_RMT2)
# Log is still very positively skewed

#TIgAT1
plotNormalHistogram(ASI_RMT1$TIgA)
ASI_RMT1$TIgASQRT<- sqrt(ASI_RMT1$TIgA)
plotNormalHistogram(ASI_RMT1$TIgASQRT)
ASI_RMT1$TIgALOG<-log(ASI_RMT1$TIgA)
plotNormalHistogram(ASI_RMT1$TIgALOG)
View(ASI_RMT1)
# Log is very positively skewed with negative kurtosis

#TIgAT2
plotNormalHistogram(ASI_RMT2$TIgA)
ASI_RMT2$TIgASQRT<- sqrt(ASI_RMT2$TIgA)
plotNormalHistogram(ASI_RMT2$TIgASQRT)
ASI_RMT2$TIgALOG<-log(ASI_RMT2$TIgA)
plotNormalHistogram(ASI_RMT2$TIgALOG)
View(ASI_RMT2)
# Log is positively skewed with negative kurtosis

#GIgAT1
plotNormalHistogram(ASI_RMT1$GIgA)
ASI_RMT1$GIgAsqrt <- sqrt(ASI_RMT1$GIgA) 
plotNormalHistogram(ASI_RMT1$GIgAsqrt)
ASI_RMT1$GIgALOG <- log(ASI_RMT1$GIgA)
plotNormalHistogram(ASI_RMT1$GIgALOG)
View(ASI_RMT1)
# Log is best

#GIgAT2
plotNormalHistogram(ASI_RMT2$GIgA)
ASI_RMT2$GIgAsqrt <- sqrt(ASI_RMT2$GIgA) 
plotNormalHistogram(ASI_RMT2$GIgAsqrt)
ASI_RMT2$GIgALOG <- log(ASI_RMT2$GIgA)
plotNormalHistogram(ASI_RMT2$GIgALOG)
View(ASI_RMT2)
# Log is best

### Many decisions still to be made based on correlations and normality assumptions. 
### Pertinent columns will be kept and downloaded for next stage of analysis.


