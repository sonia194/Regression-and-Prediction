# Replace 1234567 with your matriculation number (or any of your groupmembers)
MyMatrikel = 6810782

# Make sure to be in the correct working directory
# (where "W4479 - Problem 2 Data.tx" lies)
getwd()

# Run this code (please do not edit the lines below)
Data = read.table("w4479-problem 2 Data.txt",header=TRUE)
set.seed(6810782)
choose = sample(1:50, 3, replace = TRUE)
Y1 = Data[, choose[1]]
Y2 = Data[, choose[1] + 50]
Y3 = Data[, choose[1] + 100]
X1 = Data[, 151]
X2 = Data[, 152]
X3 = Data[, 153]

rm(list = c("Data", "MyMatrikel"))


# Start your own code here. Just use Y1, X1, ..., X3 for your calculations

# data transformation
#(X1,Y1)

X = log(X1)				
X_inv = 1/X1
Y = log(Y1)

#(X2,Y2)

Y.2=log(Y2)
X.2=log(X2)
X_inv.2=1/X2

# Graphical analysis

#(X1,Y1)

par(mfrow = c(2,3))				
plot(X1, Y1, main = "Model")		
plot(X, Y1, main = "lin-log")
plot(X1, Y, main = "log-lin")
plot(X_inv, Y1, main = "reciprocal")
plot(X,Y,main ="log-linear")
plot(X_inv,Y,main = "log-reciprocal")

#(X2,Y2)
par(mfrow= c(2,3))
plot(X2,Y2,main ="model2")
plot(X.2,Y.2,xlab = "logX2",ylab = "logY2")
title("Log-linear")
plot(X_inv.2,Y2,xlab = "X_inv.2",ylab = "Y2")
title("Reciprocal")
plot(X.2,Y2,xlab = "logx2",ylab = "Y2")
title("lin-log")
plot(X2,Y.2,xlab = "X2",ylab="logY2")
title("log-lin")
plot(X_inv.2,Y.2,xlab = "X_inv.2",ylab = "logY2")
title("Log reciprocal")

# RSS analysis
# (X1,Y1)

reg_1 = lm(Y1~X)
reg_2 = lm(Y~X1)
reg_3 = lm(Y1~X_inv)
reg_4 = lm(Y~X)
reg_5 = lm(Y~X_inv)

#fitted Values

Y_1 = reg_1$fitted.values
Y_2 = exp(reg_2$fitted.values)
Y_3 = reg_3$fitted.values
Y_4 = exp(reg_4$fitted.values)
Y_5 = exp(reg_5$fitted.values)



res_1 = Y1 - Y_1			
res_2 = Y1 - Y_2
res_3 = Y1 - Y_3
res_4 = Y1 - Y_4
res_5 = Y1 - Y_5

RSS = function(res) { sum(res^2) }
RSS(res_1)				
RSS(res_2)
RSS(res_3)
RSS(res_4)
RSS(res_5)




#(Y2,X2)


reg1=lm(Y.2~X.2)
reg2=lm(Y2~X_inv.2)
reg3=lm(Y2~X.2)
reg4=lm(Y.2~X2)
reg5=lm(Y.2~X_inv.2)


Y.1 = exp(reg1$fitted.values)	
Y.2 = reg2$fitted.values
Y.3 = reg3$fitted.values
Y.4 = exp(reg4$fitted.values)
Y.5 = exp(reg5$fitted.values)

res.1 = Y2 - Y.1			 
res.2 = Y2 - Y.2
res.3 = Y2 - Y.3
res.4 = Y2 - Y.4
res.5 = Y2 - Y.5

RSS = function(res) { sum(res^2) }
RSS(res.1)				 
RSS(res.2)
RSS(res.3)
RSS(res.4)
RSS(res.5)
#b) plot the best Model

par(mfrow = c(1,2))
plot(X,Y,xlab = "logX2",ylab = "logY2")
title("Log-linear")
abline(reg_4,col="red")
plot(X.2,Y2,xlab = "logx2",ylab = "Y2")
title("lin-log")
abline(reg3,col="red")

ord = order(X1) 
X1 = X1[ord]
Y1=Y1[ord]
Y_4 = Y_4[ord] 
par(mfrow = c(1,2))
plot(X1,Y1,main = "model1")
lines(X1,Y_4,col="red")
ord2= order(X2)
X2= X2[ord2]
Y2=Y2[ord2]
Y.3 = Y.3[ord2]
plot(X2,Y2, main = "model2")
lines(X2,Y.3,col="red")

#c) Test the assumptions of normal distributed error term 

# (X1,Y2)
n=length(X)
residuals = residuals(reg_4)
residuals=residuals(lm(Y~X))
Res_bar = mean(residuals)
sd = sqrt(mean((residuals - Res_bar)^2))	
c = residuals - Res_bar	
S = (1/n)*(sum(c^3))/(1/n*sum(c^2))^1.5	
K = (1/n*sum(c^4))/(1/n*sum(c^2))^2		

# again, a conventient package
library(moments)					
S = skewness(residuals)
K = kurtosis(residuals)

# Jarque-Bera Test
JB = (n/6)*(S^2 + (K-3)^2/4)			
p = 1-pchisq(JB,2)				
p; JB

par(mfrow = c(2,2))
plot(X, residuals, main = "Residual Plot")
hist(residuals)
qqnorm(residuals, ylab = "Residuals", xlab = "Quantiles of Standard Normal")
qqline(residuals,col="red")


#für (X2,Y2)
n=length(X.2)
residuals = residuals(reg3)
Res_bar = mean(residuals)
sd = sqrt(mean((residuals - Res_bar)^2))	
c = residuals - Res_bar	# centered values
S = (1/n)*(sum(c^3))/(1/n*sum(c^2))^1.5	
K = (1/n*sum(c^4))/(1/n*sum(c^2))^2		

# again, a conventient package
library(moments)					
S = skewness(residuals)
K = kurtosis(residuals)

# Jarque-Bera Test
JB = (n/6)*(S^2 + (K-3)^2/4)			
p = 1-pchisq(JB,2)				
p; JB

par(mfrow = c(2,2))
plot(X.2, residuals, main = "Residual Plot")
hist(residuals)
qqnorm(residuals, ylab = "Residuals", xlab = "Quantiles of Standard Normal")
qqline(residuals,col="red")
# d)calculate ß1, ß2 of (Y3,X3)

model<-lm(Y3~X3)
A<-matrix(c(1,2,0.3,-1),nrow=2,ncol=2)
b<-matrix(c(model$coef[1],model$coef[2]),nrow=2,ncol=1)
solve(A,b)
