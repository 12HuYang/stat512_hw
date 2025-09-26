line_1=c(18,16,31,35,22,27,39,       36,	15,       12)
line_2=c(13,      15,	33,       30,	24,       21,	35,       38,	10,       16)
line_3=c(24,      28,	42,       46,	40,       37,	52,       57,	28,       24)
y=c(line_1,line_2,line_3)
factorA=c(rep(c("1","1","2","2","3","3","4","4","5","5"),3))
factorB=c(rep("1",10),rep("2",10),rep("3",10))
yaov<-aov(y~factorA*factorB)
summary(yaov)


interaction.plot(factorA,factorB,y)

out_a=TukeyHSD(yaov,"factorA",conf.level=0.95)
out_a
out_b=TukeyHSD(yaov,"factorB",conf.level=0.95)
out_b


plot(yaov, which = 1) #produces residual versus fitted value plot
plot(yaov, which = 2) #produces normal probability of residuals

shapiro.test(residuals(yaov))#normality test

library(car)
leveneTest(y~factorA*factorB,data = dataset)




