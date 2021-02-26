library("rcompanion")
library("car")
keeps <- c("Time", "Age", "Gender", "C1", "C2", "C3", "C4","CTotal", "DHEA", "P17.OH", "TIgA", "GIgA", "RandID")
ASI_RMTime1 <- ASI_RMTime[keeps]
#C1 and Time
ASI_RMTime1$C1LOG<- log(ASI_RMTime1$C1)
leveneTest(C1LOG ~ Time, data = ASI_RMTime1)
C1Rmanova<- aov(C1LOG ~ Time+Error(RandID), ASI_RMTime1)
summary(C1Rmanova)
# This test shows that C1 and Time was not significant
# C2 and Time
ASI_RMTime1$C2LOG<- log(ASI_RMTime1$C2)
leveneTest(C2LOG ~ Time, data = ASI_RMTime1)
C2Rmanova<- aov(C2LOG ~ Time+Error(RandID), ASI_RMTime1)
summary(C2Rmanova)
#These tests show that C2 and Time are not significant.
#C3 and Time
ASI_RMTime1$C3LOG<- log(ASI_RMTime1$C3)
leveneTest(C3LOG ~ Time, data = ASI_RMTime1)
C3Rmanova<- aov(C3LOG ~ Time+Error(RandID), ASI_RMTime1)
summary(C3Rmanova)
#Not significant
#c4 and Time
ASI_RMTime1$C4LOG<- log(ASI_RMTime1$C4)
leveneTest(C4LOG ~ Time, data = ASI_RMTime1)
C4Rmanova<- aov(C4LOG ~ Time+Error(RandID), ASI_RMTime1)
summary(C4Rmanova)
#Not 
#CT and Time
ASI_RMTime1$CTotalLOG<- log(ASI_RMTime1$CTotal)
leveneTest(CTotalLOG ~ Time, data = ASI_RMTime1)
CTotalRmanova<- aov(CTotalLOG ~ Time+Error(RandID), ASI_RMTime1)
summary(CTotalRmanova)
#Not
#DHEA
ASI_RMTime1$DHEALOG<- log(ASI_RMTime1$DHEA)
leveneTest(DHEALOG ~ Time, data = ASI_RMTime1)
DHEARmanova<- aov(DHEALOG ~ Time+Error(RandID), ASI_RMTime1)
summary(DHEARmanova)
#NOT
#P17
ASI_RMTime1$P17.OHLOG<- log(ASI_RMTime1$P17.OH)
leveneTest(P17.OHLOG ~ Time, data = ASI_RMTime1)
P17.OHRmanova<- aov(P17.OHLOG ~ Time+Error(RandID), ASI_RMTime1)
summary(P17.OHRmanova)
#NOT
#TIGA
ASI_RMTime1$TIgALOG<- log(ASI_RMTime1$TIgA)
leveneTest(TIgALOG ~ Time, data = ASI_RMTime1)
TIgARmanova<- aov(TIgALOG ~ Time+Error(RandID), ASI_RMTime1)
summary(TIgARmanova)
#NOT
#GIGA
ASI_RMTime1$GIgALOG<- log(ASI_RMTime1$GIgA)
leveneTest(GIgALOG ~ Time, data = ASI_RMTime1)
GIgARmanova<- aov(GIgALOG ~ Time+Error(RandID), ASI_RMTime1)
summary(GIgARmanova)
#NOT
#We think that because we only have 21 participants that the power is too low to show
#any changes. We saw last night that the power is about.56 where we need it to be
#.80 at least.