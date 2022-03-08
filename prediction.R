# Problem 3
#partition
getwd()
set.seed(6810782)
Data<-read.table("engel.txt",header=TRUE)
ind<-sample(1:235,size=0.6*nrow(Data),replace=FALSE,prob=NULL)
Data_train<-Data[ind,]
Data_test<-Data[-ind,]

# Regression
reg_train<-lm(foodexp~income,data=Data_train)
summary(reg_train)

#plot
par(mfrow=c(1,1))
plot(Data_train$income,Data_train$foodexp,main="Data")
abline(reg_train,col="red")

#c
reg_test<-lm(foodexp~income,data=Data_test)


#predict
predict<-predict(reg_train,newdata=Data_test)

plot(Data_test$income,Data_test$foodexp,main="Test")
abline(reg_test,col="red")
points(Data_test$income,predict,col="green")
lines(Data_test$income,predict,col="blue")

#d)

# berechnen die Standard deviation
standard_deviation <- predict(reg_test, se.fit=TRUE)
standard_deviation<- data.frame(standard_deviation)
standard_deviation

#conf interval
conf<-predict(reg_test,interval="confidence")
conf<-data.frame(conf)


#plot
plot(Data_test$income,Data_test$foodexp,main="Data_test")
abline(reg_train,col="red")
lines(Data_test$income,conf[,"lwr"], col="blue")
lines(Data_test$income,conf[,"upr"], col="blue")
