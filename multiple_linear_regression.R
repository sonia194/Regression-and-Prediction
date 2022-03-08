#problem 1
Anscombe.<-read.table("Anscombe.csv",header=TRUE)
head(Anscombe.)
Y=Anscombe.$education     
X1=Anscombe.$income      
X2=Anscombe.$young 
X3=Anscombe.$urban   

# b)plot
par(mfrow = c(2, 2)) 
plot(X1,Y,main="Education against income")
plot(X2,Y,main="Education against young")
plot(X3,Y,main="Education against urban")

# c)

#fitted model

set.seed(6810782)
Reg1=lm(Y~X1)
Reg2=lm(Y~X2)
Reg3=lm(Y~X3)
Reg4=lm(Y~X1+X2)
Reg5=lm(Y~X1+X3)
Reg6=lm(Y~X2+X3)
Reg7=lm(Y~X1+X2+X3)

summary(Reg1)
summary(Reg2)
summary(Reg3)
summary(Reg4)
summary(Reg5)
summary(Reg6)
summary(Reg7)

#plot
par(mfrow=c(2,2))
plot(X1,Y,main="Education against income")
abline(Reg1,col="blue")
plot(X2,Y,main="Education against young")
abline(Reg2,col="blue") 
plot(X3,Y,main="Education against urban")
abline(Reg3,col="blue")

#d)calculate the adjusted R squared

summary(Reg1)$adj.r.squared
summary(Reg2)$adj.r.squared
summary(Reg3)$adj.r.squared
summary(Reg4)$adj.r.squared
summary(Reg5)$adj.r.squared
summary(Reg6)$adj.r.squared
summary(Reg7)$adj.r.squared

# Question E)Calculate the 95% and 99% condence intervals for all parameters ?? of the best model chosen in d

beta_1 = coef(Reg7)[1]
beta_2 = coef(Reg7)[2]
beta_3 = coef(Reg7)[3]
beta_4 = coef(Reg7)[4]


t_0.025 = qt(0.975, df = 47)
t_0.005 = qt(0.995, df = 47)				

se_beta_1 = coef(summary(Reg7))[1,"Std. Error"]
se_beta_2 = coef(summary(Reg7))[2,"Std. Error"]
se_beta_3 = coef(summary(Reg7))[3,"Std. Error"]
se_beta_4 = coef(summary(Reg7))[4,"Std. Error"]

CI1 = c(beta_1 - t_0.025*se_beta_1, beta_1 + t_0.025*se_beta_1)
CI2 = c(beta_2 - t_0.025*se_beta_2, beta_2 + t_0.025*se_beta_2)
CI3 = c(beta_3 - t_0.025*se_beta_3, beta_3 + t_0.025*se_beta_3)
CI4 = c(beta_4 - t_0.025*se_beta_4, beta_4 + t_0.025*se_beta_4)

CI11 = c(beta_1 - t_0.005*se_beta_1, beta_1 + t_0.005*se_beta_1)
CI21 = c(beta_2 - t_0.005*se_beta_2, beta_2 + t_0.005*se_beta_2)
CI31 = c(beta_3 - t_0.005*se_beta_3, beta_3 + t_0.005*se_beta_3)
CI41 = c(beta_4 - t_0.005*se_beta_4, beta_4 + t_0.005*se_beta_4)

names(CI1) = c("lower bound", "upper bound")
names(CI2) = c("lower bound", "upper bound")
names(CI3) = c("lower bound", "upper bound")
names(CI4) = c("lower bound", "upper bound")
names(CI11) = c("lower bound", "upper bound")
names(CI21) = c("lower bound", "upper bound")
names(CI31) = c("lower bound", "upper bound")
names(CI41) = c("lower bound", "upper bound")


# Bonuspunkt				

CI1<-confint(Reg7,level=0.95)
CI1
CI2<-confint(Reg7,level=0.99)
CI2

# Question F) Use an F-test to test the joint significance of the slope parameters at ?? = 0.1,0.05,0.01 and state your null and alternative hypothesis. Also calculate the p-value of this F-tests. Interpret the slope coecients of this model. Are your expectations from a. met

summary(Reg7)
F1<-qf(0.9,df1=3,df2=47)
F2<-qf(0.95,df1=3,df2=47)
F3<-qf(0.99,df1=3,df2=47)
Fisher=summary(Reg7)$fstatistic

#calculate p value
1 - pf(summary(Reg7)$fstatistic[1], 3, 47)
