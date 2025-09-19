y=c(2.4,
    10.6,
    12,
    4.6,
    40.9,
    47.7,
    31.9,
    36.3,
    70.5,
    96.7,
    26.4,
    76,
    127.2,
    134,
    28.1,
    78.3)
trt=c("1","1","1","1","2","2","2","2","3","3","3","3","4","4","4","4")
mean(y[trt=="1"])
mean(y[trt=="2"])
mean(y[trt=="3"])
mean(y[trt=="4"])

out=aov(formula=y~trt)
summary(out)

TukeyHSD(out,conf.level=0.95) # Tukey Honest Significant Differences
plot(TukeyHSD(out,conf.level=0.95)) # plot of CIs for differences

plot(out, which = 1) #produces residual versus fitted value plot
plot(out, which = 2) #produces normal probability of residuals
shapiro.test(residuals(out))#normality test
library(car)
leveneTest(y~trt)

library(dplyr)
q2=read_csv("./hw2_q2_data.csv")

q2
mu_a=mean(q2$`Brand A`)
mu_b=mean(q2$`Brand B`)
mu_c=mean(q2$`Brand C`)
mu_d=mean(q2$`Brand D`)
mu_a
mu_b
mu_c
mu_d
mu=mean(c(mu_a,mu_b,mu_c,mu_d))
mu
sum((q2$`Brand A`-mu)^2,(q2$`Brand B`-mu)^2,(q2$`Brand C`-mu)^2,(q2$`Brand D`-mu)^2)
sum((q2$`Brand A`-mu)^2)
sum((q2$`Brand B`-mu)^2)
sum((q2$`Brand C`-mu)^2)
sum((q2$`Brand D`-mu)^2)

q2_ss=(q2-mu)^2

SS_total=sum(q2_ss$`Brand A`)+sum(q2_ss$`Brand B`)+sum(q2_ss$`Brand C`)+sum(q2_ss$`Brand D`)
SS_total

n=10
SS_treatment=sum(c(n*(mu_a-mu)^2,n*(mu_b-mu)^2,n*(mu_c-mu)^2,n*(mu_d-mu)^2))
SS_treatment

SS_error=SS_total-SS_treatment
SS_error

df_treatment=4-1
df_error=40-4
df_total=df_treatment+df_error
MS_treatment=SS_treatment/df_treatment
MS_error=SS_error/df_error
F=MS_treatment/MS_error

MS_treatment

MS_error

F

y=c(q2$`Brand A`,q2$`Brand B`,q2$`Brand C`,q2$`Brand D`)
trt=c(rep("a",10),rep("b",10),rep("c",10),rep("d",10))
out=aov(formula=y~trt)
summary(out)

TukeyHSD(out,conf.level=0.95) # Tukey Honest Significant Differences
plot(TukeyHSD(out,conf.level=0.95)) # plot of CIs for differences

plot(out, which = 1) #produces residual versus fitted value plot
plot(out, which = 2) #produces normal probability of residuals
shapiro.test(residuals(out))#normality test
library(car)
leveneTest(y~trt)
