# Q1

ration_a=c(10,6,8,6,12,9,11,5,9,6)
ration_b=c(13,9,15,10,14,8,13,10,17,8)
ration_c=c(12,10,16,12,13,10,11,9,15,9)
ration_d=c(15,21,13,18,15,20,10,19,12,22)

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
SSC/mse

# H0: l_hat =0
# Ha: l_hat != 0



