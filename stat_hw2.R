# Q1

ration_a=c(10,6,8,6,12,9,11,5,9,6)
ration_b=c(13,9,15,10,14,8,13,10,17,8)
ration_c=c(12,10,16,12,13,10,11,9,15,9)
ration_d=c(15,21,13,18,15,20,10,19,12,22)

ration=c(rep("a",length(ration_a)),rep("b",length(ration_b)),
         rep("c",length(ration_c)),rep("d",length(ration_d)))

dataset=list()
dataset$response=c(ration_a,ration_b,ration_c,ration_d)
dataset$ration=ration
dataset
model=lm(response ~ ration,data=dataset)
summary(model)
model
# install.packages("lsmeans")
library(lsmeans)
leastsquare = lsmeans(model,"ration")
Contrasts = list(abvscd = c(1,1,-1,-1),
                 abcvsd = c(1,1,1,-3))

lsmeans::contrast(leastsquare,Contrasts)


mu_a=mean(ration_a)
mu_b=mean(ration_b)
mu_c=mean(ration_c)
mu_d=mean(ration_d)

a1=1
a2=1
a3=-1
a4=-1

l_hat=a1*mu_a+a2*mu_b+a3*mu_c+a4*mu_d
l_hat
mse=mean(c(var(ration_a),var(ration_b),var(ration_c),var(ration_d)))
se=sqrt(mse*sum(a1^2/10+a2^2/10+a3^2/10+a4^2/10))
se

t=l_hat/se
t

alpha=0.05
df_1=1
# df_error=n-t=40-4-36
abs(t)

# table_t=(0.05,1,36)=4.17 ~ 4.08
# abs(t)>table_t -> reject Ho

SSC=(l_hat)^2/(4*(1)^2/10)
SSC
F=SSC/mse
F
# H0: l_hat =0
# Ha: l_hat != 0

a1=1
a2=1
a3=1
a4=-3

l_hat=a1*mu_a+a2*mu_b+a3*mu_c+a4*mu_d
l_hat
mse=mean(c(var(ration_a),var(ration_b),var(ration_c),var(ration_d)))
mse
se=sqrt(mse*sum(a1^2/10+a2^2/10+a3^2/10+a4^2/10))
se
t=l_hat/se
t
alpha=0.05
df_1=1
SSC=(l_hat)^2/(sum(c(a1^2/10,a2^2/10,a3^2/10,a4^2/10)))
SSC
F=SSC/mse
F

q3_table=read.csv("./hw2_q3.csv")
q3=q3_table

y=c(q3$Methodist,q3$Catholic,q3$Pentecostal)
trt=c(rep("Methodist",10),rep("Catholic",10),rep("Pentecostal",10))
out=aov(formula=y~trt)
summary(out)

TukeyHSD(out,conf.level=0.95) # Tukey Honest Significant Differences
plot(TukeyHSD(out,conf.level=0.95)) # plot of CIs for differences

plot(out, which = 1) #produces residual versus fitted value plot
plot(out, which = 2) #produces normal probability of residuals
shapiro.test(residuals(out))#normality test
library(car)
leveneTest(y~trt)
