E1=final_eco
R1=lm(log(E1$FDI)~E1$FEX)
summary(R1)
jarque.bera.test(residuals(R4))

R3=lm(log(E1$FDI)~log(E1$INFLATION))
summary(R3)

R4=lm(log(E1$FDI)~E1$FEX+log(E1$INFLATION))
summary(R4)
R5=lm(log(E1$FDI)~E1$FEX+E1$INFLATION)
summary(R5)
hist(residuals(R3))
#normal probability plot





E1=final_eco
R1=lm(log(E1$FEX)~E1$FDI)
summary(R1)
jarque.bera.test(residuals(R3))


B=lm(log(E1$FEX)~log(E1$INFLATION))
summary(B)

A=lm(log(E1$FEX)~E1$FDI+log(E1$INFLATION))
summary(R4)
plot(B)
hist(residuals(B))
cor(E1$FDI,E1$INFLATION)

Anova(A,B)
durbinWatsonTest(B)
skedastic::white(B)
bptest(R4)
durbinWatsonTest(A)
cor(E1$FDI,E1$INFLATION)
