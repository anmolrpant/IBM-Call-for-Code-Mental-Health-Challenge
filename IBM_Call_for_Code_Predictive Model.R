library(ISLR)
require(MASS)
require(faraway)
require(ISLR)
require(glmnet)
require(dplyr)
require(leaps)
data=data.frame(read.csv('IBM_finaldataset.csv'))
IBM = data[,-2]

RNGkind(sample.kind = "Rounding") 
set.seed(1)
sample=sample(1:nrow(IBM),round(0.8*nrow(IBM)),replace=F)
train = IBM[sample,]
test = IBM[-sample,]
x_train= model.matrix(train$Satisfaction.Level~.,train)
y_train= train$Satisfaction.Level
x_test=model.matrix(test$Satisfaction.Level~.,test)
y_test= test$Satisfaction.Level

#Fitting the first model "lm" with the training data. Only orig predictors
lm=lm(Satisfaction.Level~.,train)
summary(lm)
par(mfrow=c(1,1))
plot(lm,pch=19) 
vif(lm)

par(mfrow=c(1,1))
plot(IBM, pch=19)
#look for outliers


library(faraway)
cook=cooks.distance(lm)
halfnorm(cook,3)

#whatever appears to be outliers - run without them 
data_outl=train[-c(45,1785,1790),] #remove rows of outliers
lm_outl=lm(data_outl$Satisfaction.Level~.,data=data_outl)
summary(lm_outl)
##didnt see a difference removing outliers so can continue with original data 

#linear regression
pred.lm=predict(lm,data=test)
mean((pred.lm-y_test)^2) 
#mse for linear regressio is 1.101973

#log transform
lm_t=lm(log(Satisfaction.Level)~.,data=train)
summary(lm_t)
par(mfrow=c(2,2))
plot(lm_t)
vif(lm_t)
# does not seem to help much 

###### Transforming Data - y^a ######
#might need to choose new value of a 
#a = 0.5 had good results --> need to determine best value for a 
lm_pw=lm(Satisfaction.Level^.5~.,data=train)
summary(lm_pw)

par(mfrow=c(2,2))
plot(lm_pw)
vif(lm_pw)

#not better 

#box-cox transformation
library(MASS)
par(mfrow=c(1,1))
box=boxcox(lm)
lambda.t=box$x[which.max(box$y)]
lambda.t #1.030303
train$Satisfaction.Level.box=(train$Satisfaction.Level^lambda.t-1)/lambda.t
test$Satisfaction.Level.box=(test$Satisfaction.Level^lambda.t-1)/lambda.t

lm_box=lm(train$Satisfaction.Level.box~.,data = train[,-18]) #exclude initial Satisfaction.Level health if using transform
summary(lm_box)
vif(lm_box)
par(mfrow=c(1,1))
plot(lm_box)
train=train[,-19]
test=test[,-19]
##not good , no transformation needed 

##if collinearity is still occuring - need to center and add interactions 
#Center variables
data1=IBM
data1$Age_c=data1$Age-mean(data1$Age)
data1$Cred_c=data1$Cred-mean(data1$Cred)
data1$slp_c=data1$slp-mean(data1$slp)
data1$lone_c=data1$lone-mean(data1$lone)
data1$Stress_c=data1$Stress-mean(data1$Stress)
data1$PA_c=data1$PA-mean(data1$PA)

#interactions
data1$Age_Cred = data1$Age_c*data1$Cred_c #may need to add IBM$ to all other lines
data1$Age_slp = data1$Age_c*data1$slp_c
data1$Age_lone = data1$Age_c*data1$lone_c
data1$Age_Stress = data1$Age_c*data1$Stress_c  
data1$Age_PA = data1$Age_c*data1$PA_c
data1$Cred_slp = data1$Cred_c*data1$slp_c
data1$Cred_lone = data1$Age_c*data1$lone_c
data1$Cred_Stress = data1$Cred_c*data1$Stress_c  
data1$Cred_PA = data1$Cred_c*data1$PA_c
data1$slp_lone = data1$slp_c*data1$lone_c
data1$slp_Stress = data1$slp_c*data1$Stress
data1$slp_PA = data1$slp_c*data1$PA_c
data1$lone_Stress = data1$lone_c*data1$Stress_c  
data1$lone_PA = data1$lone_c*data1$PA_c
data1$Stress_PA = data1$Stress_c*data1$PA_c

sample1=sample(1:nrow(data1),round(0.8*nrow(data1)),replace=F)
train1 = data1[sample,]
test1 = data1[-sample,]
x_train1= model.matrix(train1$Satisfaction.Level~.,train1)
y_train1= train1$Satisfaction.Level
x_test1=model.matrix(test1$Satisfaction.Level~.,test1)
y_test1= test1$Satisfaction.Level
lm_c=lm(Satisfaction.Level~.,data=train1)

pred.lmc=predict(lm_c,data=test1)
mean((pred.lmc-y_test1)^1) #mse = 1.10173
vif(lm_c)
summary(lm_c)
plot(lm_c)
plot(lm)

pred.lmc.train=predict(lm_c,data=data1)
lmod=lm(Satisfaction.Level~.,data=data1)
plot(lmod$fitted.values,data1$Satisfaction.Level,pch=19,col="blue")
points(pred.lmc.train,train1$Satisfaction.Level,col="red",lwd=2)
abline(a=0,b=1)


points(lm_c$predicted,train1$Satisfaction.Level,col="red",lwd=2)
job, scholarship, loan, meal premium, GPA low, GPA medium, Sleep,lonely, stress,
physical activity anxiety 
IBM2 = data1[,-c(1,2,4,5,6,7,11,19:38)]
IBM2 = IBM2[,-12]

sample2=sample(1:nrow(IBM2),round(0.8*nrow(IBM2)),replace=F)
train4 = IBM2[sample2,]
test4 = IBM2[-sample2,]
x_train4= model.matrix(train4$Satisfaction.Level~.,train4)
y_train4= train4$Satisfaction.Level
x_test4=model.matrix(test4$Satisfaction.Level~.,test4)
y_test4= test4$Satisfaction.Level
lm_c=lm(Satisfaction.Level~.,data=train4)

pred.lmc2=predict(lm_c,data=test4)
mean((pred.lmc2-y_test4)^1) #mse = 1.10173
vif(lm_c)
summary(lm_c)
plot(lm_c)
plot(lm)

pred.lmc.train2=predict(lm_c,data=IBM2)
lmod=lm(Satisfaction.Level~.,data=IBM2)
plot(lmod$fitted.values,IBM2$Satisfaction.Level,pch=19,col="blue")
points(pred.lmc.train2,train4$Satisfaction.Level,col="red",lwd=2)
abline(a=0,b=1)



#######ridge regression#######

grid=10^seq(3,-3,length=100) # lambda ranges from 0.1 to 0.00001 
ridge.mod=glmnet(x_train1,y_train1,alpha=0,lambda=grid)  

# plot coefficent values as we change lambda
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

cv.ridge=cv.glmnet(x_train,y_train,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r #.001
mse.r

pred.ridge=predict(ridge.mod,type="response", s=bestlam.r,x_train)
mse.r_test= mean((pred.ridge-y_test)^2)
mse.r_test #1.10007

# get coefficents for best model and compare to OLS
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef

# plotfitted values for OLS and Ridge, compare with actual with actual

fit.ridge=predict(ridge.mod,s=bestlam.r, x_test)


#######lasso################

#Lasso Model
lasso.mod=glmnet(x_train,y_train,alpha=1,lambda=grid)  
plot(lasso.mod)  
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(1)
cv.lasso=cv.glmnet(x_train,y_train,alpha=1,lambda=grid)
par(mfrow=c(1,1))
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l #.002656
mse.l
cv.lasso$lambda.1se
lam.1st=cv.lasso$lambda.1se

pred.lasso=predict(lasso.mod,type="response", s=bestlam.l,x_train)
mean((pred.lasso*y_test)^2) 

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=lam.1st)
lasso.coef


require(dplyr)
require(leaps)
## all predictors - interactions, centered 
data2=data1[,-c(19:24)]
sample2=sample(1:nrow(data2),round(0.8*nrow(data2)),replace=F)
train2 = data2[sample,]
test2 = data2[-sample,]
x_train2= model.matrix(Satisfaction.Level~.,train2)
y_train1= train2$Satisfaction.Level
x_test2=model.matrix(Satisfaction.Level~.,test2)
y_test2= test2$Satisfaction.Level

######bootstrapping#######
require(randomForest)
set.seed(1)
boot.index=sample(1:dim(train)[1],dim(train)[1],replace=T)
boot.index
boot.sample=data[boot.index,]

## change mtry for predicting variables ### dont use centered or interactions 
bag.mod=randomForest(Satisfaction.Level~.,data=train,mtry=17, importance=T, ntree=5000) 
bag.mod 
#mean of squared residuals = .0921
# %var explained = 84.42
plot(bag.mod) 
varImpPlot(bag.mod,type=1,pch=19) 
importance(bag.mod)

#most important variables = loneliness, stress, GPA, covid, and financial status 

treePredictBag=predict(bag.mod,newdata=test)
msebag =mean((treePredictBag - test$Satisfaction.Level)^2)
msebag 
#mse value = .0905 

# plotfitted values for OLS and RT, compare with actual
lmod=lm(Satisfaction.Level~.,data=)
plot(lmod$fitted.values,train$Satisfaction.Level,pch=19,col="blue")
points(bag.mod$predicted,train$Satisfaction.Level,col="red",lwd=2)
abline(a=0,b=1)

######################## Random Forest #################################                      
require(caret)                                                   

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")
set.seed(1)
tunegrid=expand.grid(mtry=c(1:17))
rf_gridsearch=train(Satisfaction.Level~.,data=train, method="rf", metric="RMSE", 
                    tuneGrid=tunegrid, trControl=control)
#View(data)
print(rf_gridsearch)
plot(rf_gridsearch) 

set.seed(1)
rf.mod=randomForest(Satisfaction.Level~.,data=train,mtry=17, ntree=1000, 
                    importance=T)
rf.mod 
plot(rf.mod)
summary(rf.mod)
#MSE 0.09229
# 84.41% Variance explained
varImpPlot(rf.mod,type=1,pch=19) #again temperature is the most influencing predictor

importance(rf.mod)
treePredictrf=predict(rf.mod,newdata=test)
mserf =mean((treePredictrf - test$Satisfaction.Level)^2)
mserf #MSE .0905


# plotfitted values for OLS and RT, compare with actual
lmod=lm(Satisfaction.Level~.,data=train)
plot(lmod$fitted.values,train$Satisfaction.Level,pch=19,col="blue")
points(rf.mod$predicted,train$Satisfaction.Level,col="red",lwd=2)
abline(a=0,b=1)


######BOOSTING######
#original data no center/interactions etc. 
require(gbm)

testcontrol=trainControl(method="cv", number=5, search="grid")
set.seed(1)
tunegrid=expand.grid(n.trees=c(100,500,1000,2000,5000,7500),
                     interaction.depth=c(1,3,5),
                     shrinkage=c(0.001,0.005,0.01),
                     n.minobsinnode=c(1,3,5))
gb_gridsearch=train(Satisfaction.Level~.,data=train, 
                    method="gbm", metric="RMSE",
                    tuneGrid=tunegrid, trControl=control)
print(gb_gridsearch) #ntrees = 5000, interaction depth = 3, shrinkage = 0.001 n.m = 3
plot(gb_gridsearch)

lmod = lm(Satisfaction.Level~., train)

gb.mod = gbm (Satisfaction.Level~., data=train[,-2], distribution = "gaussian",n.trees = 5000, shrinkage = 0.01, interaction.depth = 1, n.minobsinnode=5)

summary(gb.mod,cBars=10)
pred.gb=predict(gb.mod,newdata=test,n.trees=1000)
test.mse = mean((pred.gb- y_test)^2)
test.mse 

######best subset 
best.sub.mod=regsubsets(Satisfaction.Level~.,data=train2,nvmax=32,method="exhaustive")
best.sub.sum=summary(best.sub.mod)
best.sub.sum


pred.sbs=function(obj,new,id){
  form=as.formula(obj$call[[2]])
  mat=model.matrix(form,new)
  coefi=coef(obj,id=id)
  xvars=names(coefi)
  return(mat[,xvars]%*%coefi)
}

# set up for cross validation
p=1:32
k=5  # set number of folds
set.seed(123)

# create an index with id 1-5 to assign observations to folds
folds=sample(1:k,nrow(train2),replace=T) 
folds
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,32,dimnames=list(NULL,paste(1:32)))
View(cv.err)
# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-37 predictors fit without kth fold
  best.mods=regsubsets(Satisfaction.Level~.,data=train2[folds!=j,],
                       nvmax=32,method="exhaustive")
  
  # estimate test error for all 37 models by predicting kth fold 
  for (i in 1:32){
    pred=pred.sbs(best.mods,train2[folds==j,],id=i)
    cv.err[j,i]=mean((train2$ozone.box[folds==j]-pred)^2)  # save error est
  }
}


mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE
min
oneSE.cv=apply(cv.err,2,sd) # compute standard error for each number of predictors
min1se=mse.cv[min]+oneSE.cv[min]
min1se # min1se = 0.475265
# find 1se number of predictors
for(i in 1:32){
  if(mse.cv[i]>min1se){
    min.1se=i+1
  }
}
min

# plot and put a red circle around lowest MSE, blue circle around 1se MSE
par(mfrow=c(1,1))
plot(1:32,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE",pch=19)
points(min,mse.cv[min],cex=2,col="red",lwd=2)
points(min.1se,mse.cv[min.1se],cex=2,col="blue",lwd=2)
abline(h=min1se,lty=2,col="blue") # plot 1se line

##Best nomber of predictors = 19
##Best number of predictors with an MSE within 1st= 6

best.mods=regsubsets(Satisfaction.Level~.,data=train2,nvmax=6,method="exhaustive")
summary(best.mods)

#From the summary the variables for a 19 predictor models are:
#vh, humidity, temp,ibh, ibt, seasSpring, seasSummer, seasWinter, vh_ibh, vh_dpg,wind_humidity,wind_ibh,wind_dpg, humidity_temp, humidity_dpg,  
#temp_dpg, temp_vis, dpg_ibt, ibt_vis

lmod.best19=lm(ozone.box~vh+ humidity+ temp+ibh+ ibt+ seas+ vh_ibh+ vh_dpg+wind_humidity+wind_ibh+wind_dpg+ humidity_temp+ humidity_dpg
               + temp_dpg+ temp_vis+ dpg_ibt+ ibt_vis,data=train3)
summary(lmod.best19)
vif(lmod.best19)
#There is colliniarity in some of the predictors, so we will try the best subset withi 1 st error which is with 6 predictors
# Predictors for our parsimonious model:
#humidity, temp, ibt, seas, humidity_dpg, humidity_ibt

lmod.best6=lm(ozone.box~humidity+ temp+ ibt+ seas+ humidity_dpg+ humidity_ibt,data=train3)
summary(lmod.best6)
vif(lmod.best6)
#ibt and tem has higher vif (6.51 and 4.60)

lmod.best7=lm(ozone.box~humidity+ temp+ ibt+ seas+dpg+ humidity_dpg+ humidity_ibt,data=train3)
summary(lmod.best7)


pred.lmod.best6=predict(lmod.best6,newdata = test3)
mse.best6=mean((((pred.lmod.best6*lambda.t+1)^(1/lambda.t))-((test3[,10]*lambda.t+1)^(1/lambda.t)))^2)
mse.best6 #29.19 

pred.lmod.best7=predict(lmod.best7,newdata = test3)
mse.best7=mean((((pred.lmod.best7*lambda.t+1)^(1/lambda.t))-((test3[,10]*lambda.t+1)^(1/lambda.t)))^2)
mse.best7 #29.9866 

pred.lmod.train7=predict(lmod.best7,newdata = train3)
summary(lmod.best7)
vif(lmod.best7)

par(mfrow=c(2:2))
plot(lmod.best7)



