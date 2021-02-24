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