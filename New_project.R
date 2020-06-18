install.packages('ggpmisc')
install.packages('forecast')
install.packages('glmnet')
library(tidyverse)
library(dplyr)
library(modeest)
library(xlsx)
library(ggplot2)
library(corrplot)
library(scales)
library(randomForest)
library(modelr)
library(MASS)
library(leaps)
library(ggpmisc)
library(forecast)
library(glmnet)
data<- read.csv('brooklyn.csv')

names(data)
data<-data[,-c(1,7)]
sapply(data,class)
data<-filter(data,data$SALE.PRICE>0)
data<-filter(data,data$YEAR.BUILT!=0 )
summary(data$SALE.PRICE)

fctrs <- which(sapply(data, is.factor)) 
nums <- which(sapply(data, is.numeric)) 

data$ZIP.CODE<-as.factor(data$ZIP.CODE)
data$TAX.CLASS.AT.TIME.OF.SALE<-as.factor(data$TAX.CLASS.AT.TIME.OF.SALE)
data$YEAR.BUILT<-as.factor(data$YEAR.BUILT)
unique(data$YEAR.BUILT)
sum(is.na(data))

write.csv(data,"cleaned_data.csv",row.names=F)
data<-read.csv("cleaned_data.csv")
data<-filter(data,data$SALE.PRICE>0)
data<-filter(data,data$YEAR.BUILT!=0 )


unique(data$NEIGHBORHOOD)

numerics <- which(sapply(data, is.numeric)) 
numericsNames <- names(numerics) #saving names vector for use later on
sales2_num <- data[, numericsNames]
cor_num <- cor(sales2_num, use="pairwise.complete.obs") #correlations of all numeric variables
corr_sorted <- as.matrix(sort(cor_num[,'SALE.PRICE'], decreasing = TRUE))
corr_sorted
cor_pos <- names(which(apply(corr_sorted, 1, function(x) abs(x)>0.1)))
cor_num <- cor_num[cor_pos, cor_pos]
corrplot.mixed(cor_num, tl.col="black", tl.pos = "lt")


ggplot(data=data,aes(x=TOTAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  scale_x_continuous(breaks=c(1,2,5,10),labels=comma,name = "Total Units")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1,10))+
  ylim(100000,1000000)+
  ggtitle("Total units vs Sales Price")

ggplot(data=data,aes(x=GROSS.SQUARE.FEET,y=SALE.PRICE))+
  geom_point(col='blue')+
  geom_smooth(method='lm',se=FALSE,color='black')+
  scale_x_continuous(breaks=c(100,10000),labels=comma,name="Gross Square feet")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(500,10000),ylim=c(1000,1000000))+
  ggtitle("Gross Square feet vs Sales Price")

ggplot(data=data,aes(x=RESIDENTIAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  scale_x_continuous(breaks=c(1,2,5,10,50,100),labels=comma,name="Residential units")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1,10))+
  ylim(100000,1000000)+
  ggtitle("Residential Units vs Sales Price")


avg_sales<-data%>%
  group_by(YEAR.BUILT)%>%
  summarize(avg=mean(SALE.PRICE))
ggplot(data = avg_sales, mapping=aes(x = YEAR.BUILT, y = avg,group=1))+
  geom_point()+
  geom_line()+
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  coord_cartesian(ylim=c(0,10000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,2000000,3000000,4000000,10000000),labels = comma,name="Average Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Impact of Year Built on Sales")

ggplot(data=data,aes(x=BUILDING.CLASS.CATEGORY,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=data,aes(x=TAX.CLASS.AT.TIME.OF.SALE,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data=data,aes(x=NEIGHBORHOOD,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=data,aes(x=ZIP.CODE,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,100000))+
  scale_y_continuous(breaks = c(0,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
attach(data)
ggplot(data=data,aes(x=GROSS.SQUARE.FEET,y=SALE.PRICE))+
  geom_point()+
  facet_grid(.~TAX.CLASS.AT.TIME.OF.SALE)+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1000,10000),ylim=c(100000,1000000))

data$LSALE.PRICE <- log(data$SALE.PRICE)

options(scipen=10000)
ggplot(data, aes(x = SALE.PRICE, fill = ..count..)) +
  geom_histogram(binwidth = 50000) +
  ggtitle("Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  coord_cartesian(xlim=c(0,20000000),ylim=c(0,200))+
  scale_x_continuous(breaks=c(1000000,5000000,10000000,20000000),labels=comma)+
  theme(plot.title = element_text(hjust = 0.5))
attach(data)

ggplot(data, aes(x =NEIGHBORHOOD , fill = NEIGHBORHOOD )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Neighborhood") +
  ylab("Count of houses") +
  xlab("Neighbourhood")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right",axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

ggplot(data, aes(x =TAX.CLASS.AT.TIME.OF.SALE , fill = TAX.CLASS.AT.TIME.OF.SALE )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Tax Class at time of sale") +
  ylab("Count of houses") +
  xlab("Tax Class at time of Sale")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)



ggplot(data, aes(x =BUILDING.CLASS.CATEGORY , fill = BUILDING.CLASS.CATEGORY )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Building Class category") +
  ylab("Count of houses") +
  xlab("Building Class Category")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

ggplot(data, aes(x = SALE.PRICE,fill = TAX.CLASS.AT.PRESENT)) +
  geom_histogram(position = "stack", binwidth = 50000) +
  ggtitle("Histogram of SalePrice by Tax class at present") +
  ylab("Count") +
  xlab("Tax Class at present") + 
  coord_cartesian(xlim=c(0,5000000))+
  scale_x_continuous(breaks = c(0,1000000,3000000,5000000),labels = comma)+
  theme(plot.title = element_text(hjust = 0.5),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))



ggplot(data, aes(x = SALE.PRICE,fill = TAX.CLASS.AT.TIME.OF.SALE)) +
  geom_histogram(position = "stack", binwidth = 50000) +
  ggtitle("Histogram of SalePrice by Tax class at time of Sale") +
  ylab("Count") +
  xlab("Tax class at time of Sale") + 
  coord_cartesian(xlim=c(0,5000000))+
  scale_x_continuous(breaks = c(0,1000000,3000000,5000000),labels = comma)+
  theme(plot.title = element_text(hjust = 0.5),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))
summary(data$TAX.CLASS.AT.TIME.OF.SALE)


unique(data$YEAR.BUILT)
set.seed(1)
quick_RF <- randomForest(x=data[1:13652,-c(1,6,7,8,15,17,19,18)], y=data$SALE.PRICE[1:13652], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:10,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase in MSE',title="Variable Importance") + 
  coord_flip() + theme(legend.position="none")


#Linear models
attach(data)
  data1<-read.csv('cleaned_data1.csv')
data1$ZIP.CODE<-as.factor(data1$ZIP.CODE)
data1$TAX.CLASS.AT.TIME.OF.SALE<-as.factor(data1$TAX.CLASS.AT.TIME.OF.SALE)
data1$YEAR.BUILT<-as.factor(data1$YEAR.BUILT)
sapply(data1,class)

#Forward selection
leaps<-regsubsets(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data1,nvmax = 10,method = "forward")
summary(leaps)
regsummary =summary(leaps)
regsummary$rsq
regsummary$adjr2
plot(regsummary$rsq,xlab="Number of variables", ylab="RSQ",type="l")
plot(regsummary$adjr2,xlab="Number of variables", ylab="Adjusted RSQ",type="l")

points(6, regsummary$adjr2[which.max(regsummary$adjr2)],col='black')


train = (data1$ID<10000)
Strain<-data1[train,]
Stest<-data1[!train,]
mod<-lm(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=Strain)
mod1<-lm(SALE.PRICE~TOTAL.UNITS+GROSS.SQUARE.FEET+RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=Strain)
summary(mod1)
lm_mod=summary(mod)
summary(mod1)
mean(lm_mod$residuals^2)
pred<-predict(mod,Stest,type="response")
mean((pred-Stest$SALE.PRICE)^2)

residuals <- Stest$SALE.PRICE - pred
linreg_pred <- data.frame("Predicted" = pred, "Actual" = Stest$SALE.PRICE, "Residual" = residuals)

accuracy(pred,Stest$SALE.PRICE)

plot(pred, Stest$SALE.PRICE,xlab="Predicted",ylab="Actual") 
abline(0,1)

#Random FOrest

quick_RF <- randomForest(x=Strain[,-c(1,6,7,8,15,17,19,18)], y=Strain$SALE.PRICE, ntree=100,importance=TRUE)
rf.pred <- predict(quick_RF,newdata=Stest)

Rsquare<-1 - (sum((Stest$SALE.PRICE-rf.pred)^2)/sum((Stest$SALE.PRICE-mean(Stest$SALE.PRICE))^2))
accuracy(rf.pred,Stest$SALE.PRICE)
plot(rf.pred, Stest$SALE.PRICE,xlab="Predicted",ylab="Actual") 
abline(0,1)


#Ridge 
names(Strain)
data1
attach(data1)
data2<-dplyr::select(data1,TOTAL.UNITS,GROSS.SQUARE.FEET,RESIDENTIAL.UNITS,BLOCK,LOT,LAND.SQUARE.FEET,SALE.PRICE,ID)
x=model.matrix(SALE.PRICE~.,data2)[,-7]
y<-data2$SALE.PRICE
train = (data2$ID<10000)
test = (-train)

ytest=y[test]
x = Strain[,c(4,5,10,12,13,14)]
sapply(data2, class)
y= Strain[,"SALE.PRICE"]
ridge_mod = glmnet(x=x[train,],y=y[train],alpha=0,lambda = 10^seq(10,-2,length=100))
ridge_mod$lambda.min
summary(ridge_mod)

ridge.cv =cv.glmnet(x=x[train,],y=y[train],alpha=0)
plot(ridge.cv)
opt_lambda = ridge.cv$lambda.min
coef(ridge.cv,s="lambda.min")
fit <- ridge.cv$glmnet.fit
summary(fit)
y_predicted = predict(ridge_mod, s=opt_lambda,newx = x[test,])

predict(ridge_mod, type = "coefficients", s = opt_lambda)
mean(y_predicted-ytest)^2
TSS = sum((ytest-mean(ytest))^2)
RSS = sum((y_predicted-ytest)^2)
rsq<-1-(RSS/TSS)
rsq

plot(y_predicted, ytest,xlab="Predicted",ylab="Actual") 
abline(0,1)


#Lasso

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = 10^seq(10,-2,length=100))
lasso.pred <- predict(lasso.mod, s = opt_lambda, newx = x[test,])
TSS = sum((ytest-mean(ytest))^2)
RSS = sum((lasso.pred-ytest)^2)
rsq<-1-(RSS/TSS)

plot(lasso.pred, ytest,xlab="Predicted",ylab="Actual") 
abline(0,1)

mean((lasso.pred-ytest)^2)
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = opt_lambda)
