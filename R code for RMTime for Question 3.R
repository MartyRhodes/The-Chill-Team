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
