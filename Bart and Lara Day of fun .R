#Task 2.3: Multiple regression in R
#Lara Cobler Moncunill
#November 15th, 2018




library(readr)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

################################################################################################
library(e1071)
library(partykit)
install.packages("hydroGOF")
install.packages("inum")
library(inum)
library(hydroGOF)
install.packages("intervals")
library(intervals)
###############################################################################################

#download data set
existing<- read.csv("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task3/existingproductattributes2017.2.csv")
newproducts<- read.csv("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task3/newproductattributes2017.2.csv")
str(existing)
summary(existing)

#Missing values for Ranking -> remove from the analysis
existing$BestSellersRank <- NULL
existing<- existing[-c(35:41),]

nrow(existing)



existing
 

#check outliers other variables and normality
for (i in 1:(ncol(existing))){
  if (is.numeric(existing[,i])){
    boxplot(existing[,i],main=paste("Boxplot of",colnames(existing)[i]),ylab=colnames(existing)[i])
    qqnorm(existing[,i],main=paste("Normal Q-Q Plot of",colnames(existing)[i])) #plot qqnorm
    qqline(existing[,i],col="red") #add qqnormal line in red
    hist(existing[,i],main=paste("Histogram of",colnames(existing)[i]), #make the histogram
         xlab=colnames(existing)[i])
  }
}

#2 outliers volume > 6000
#remove outliers of volume
existing_out <- existing[existing$Volume<6000,]
boxplot(existing_out$Volume,main="Boxplot of volume",ylab="Volume")

#Product number: no sense in  the analysis
existing_out$ProductNum <- NULL

###########################################################################################################
#Check the importance with caret 


controlVar<-rfeControl(functions=rfFuncs,method = "repeatedcv",
                       repeats = 3)
outcomeVar<-"Volume"

predictorsVar<-names (existing_out)[!names(existing_out)%in%outcomeVar]

pred_profile<-rfe (existing_out[,predictorsVar],existing_out[,outcomeVar],rfeControl =controlVar )
pred_profile

#Build the correlation matrix
corr_existing_out <- cor(existing_out[,2:16]) #dont include product type
corr_existing_out
#nice plot of the correlation matrix
pMat2 <- cor.mtest(existing_out, conf.level = .95)#########################for later remove product type 
corrplot(corr_existing_out,order="hclust",method="number",)
##################################################################################################


###############################################################################################
#remove variables that correlate between them (>0.5, <-0.5)
#5star,profit margin, 3star, 2star, 1star, negative service review, width and height
existing_out$x5StarReviews <- NULL
existing_out
existing_out_less <- subset(existing_out,select=c(x4StarReviews,PositiveServiceReview,Volume))
existing_out_less
#if ovverfitted we remove positive revew 
#Anova to see if the product type is related to volume
anova_res <- aov(Volume~. -ProductType,data=existing_out)
summary(anova_res)




#variable importance of the left
set.seed(333)
fit_rf <- randomForest(Volume~.,data=existing_out_less)
(VI_F=importance(fit_rf))
varImpPlot(fit_rf,type=2)
#oreder: positive service, 4x, product type, shipping w, product depth, price, recommend

#dummify the data, convert all factor or 'chr' classes to binary features
#existing_dummy <- dummyVars("~.",data=existing_out_less)
#existing_dum <- data.frame(predict(existing_dummy,newdata = existing_out_less))
#str(existing_dum)

#variable importance with dummies

#set.seed(333)
#fit_rf_d <- randomForest(Volume~.,data=existing_dum)
#(VI_F=importance(fit_rf_d))
#varImpPlot(fit_rf_d,type=2)
#First the reviews, last product types separate, finally smartphones,PC,laptop, netbook

#do one dummy column wit PC, laptops, notebooks, and smartphones
#existing_dum <- transform(existing_dum,ProductType.PLNS=ProductType.Laptop+ProductType.Netbook+ProductType.PC+ProductType.Smartphone)
#remove other dummies of the data frame
#existing_dum_PLNS <- existing_dum[,13:20]

#################################################################################################
treedummy <-ctree(Volume~., existing_dum, control = ctree_control(maxdepth = 6))
plot(treedummy)

treenodummy <-ctree(Volume~.-x5StarReviews,existing,control= ctree_control(maxdepth = 6))
plot(treenodummy)

################################################################################################
#annova
#anova_PLNS <- aov(Volume~ProductType.PLNS,data=existing_dum_PLNS)
#summary(anova_PLNS) #almost statistically significant



#Check relationship other variables vs volume and product type

#Variables order to add to modelling: 
#positive service, 4 star,product type, shipping weight, product depth, price and would recommend


# define an 75%/25% train/test split of the dataset
set.seed(123)
inTraining <- createDataPartition(existing_out_less$Volume, p = .75, list = FALSE)
#creates a vector with the rows to use for training
training <- existing_out_less[inTraining,] #subset training set
testing <- existing_out_less[-inTraining,] #subset testing set

#Check if the training and testing are equally distributed by volume
summary(training$Volume)
summary(testing$Volume)

par(mfrow=c(1,2))
boxplot(training$Volume, main="Training")
boxplot(testing$Volume, main="Testing")
plot(training$Volume, main="Training")
plot(testing$Volume, main="Testing")
##########Here to start the models##########################################################################








str(existing_dum_PLNS)

#predSCM <- predict(svm_model,newdata = testing)
#predSCM1 <- predict(svm_model1,newdata = testing)
#predSCM2 <- predict(svm_model2,newdata = testing)
#predSCM3 <- predict(svm_model3,newdata = testing)
#predSCM4 <- predict(svm_model4,newdata = testing)
#predSCM5 <- predict(svm_model5,newdata = testing)



tuned_parametersPLNS <- tune.svm(Volume ~ PositiveServiceReview, data = training, gamma = 10^(-5:-1), cost = 10^(-3:1))
#tuned_parameters1PLNS <- tune.svm(Volume ~ PositiveServiceReview, data = training, gamma = 10^(-5:-1), cost = 10^(-3:1))
#tuned_parameters2PLNS <- tune.svm(Volume ~ x4StarReviews , data = training, gamma = 10^(-5:-1), cost = 10^(-3:1))
#tuned_parameters3PLNS <- tune.svm(Volume ~ PositiveServiceReview +  x4StarReviews +ProductType.PLNS +ProductDepth,data = training, gamma = 10^(-5:-1), cost = 10^(-3:1))
#tuned_parameters4PLNS <- tune.svm(Volume ~ PositiveServiceReview +  x4StarReviews +ProductType.PLNS +ProductDepth +Recommendproduct, data = training, gamma = 10^(-5:-1), cost = 10^(-3:1))

#kernel "linear" might be overffitng 

summary(tuned_parametersPLNS)
#summary(tuned_parameters1PLNS )
#summary(tuned_parameters2PLNS )
#summary(tuned_parameters3PLNS )
#summary(tuned_parameters4PLNS )

#Models 
svm_modelPLNS <- svm(Volume ~ ., data=training,kernel="linear", gamma = 0.1, cost = 10,preProcess= c("Center","Scale"),metric="RMSE")
svm_model1PLNS <- svm(Volume ~ PositiveServiceReview, data=training,kernel="radial", gamma = 0.1, cost = 10)
svm_model2PLNS <- svm(Volume ~ PositiveServiceReview +  x4StarReviews, data=training,kernel="radial", gamma = 0.1, cost = 10)
svm_model3PLNS <- svm(Volume ~ PositiveServiceReview +  x4StarReviews +ProductType.PLNS, data=training,kernel="radial", gamma = 0.1, cost = 10)
svm_model4PLNS <- svm(Volume ~ PositiveServiceReview +  x4StarReviews +ProductType.PLNS +ProductDepth, data=training,kernel="radial", gamma = 0.1, cost = 10)
svm_model5PLNS <- svm(Volume ~ PositiveServiceReview +  x4StarReviews +ProductType.PLNS +ProductDepth +Recommendproduct, data=training,kernel="radial", gamma = 0.1, cost = 10)


svm_modelPLNS

#> my_prediction <- predict(my_model, test_set[,-1])
#test_set[,-1] removes the first column (the class column) to make the predictions only based on the features of the data. You should remove the column that labels your data.


predSVMPLNS <- predict(svm_modelPLNS,newdata = testing)
predSVM1PLNS <- predict(svm_model1PLNS,newdata = testing)
predSVM2PLNS <- predict(svm_model2PLNS,newdata = testing)
predSVM3PLNS <- predict(svm_model3PLNS,newdata = testing)
predSVM4PLNS <- predict(svm_model4PLNS,newdata = testing)
predSVM5PLNS <- predict(svm_model5PLNS,newdata = testing)

#why this is not working 


par(mfrow=c(1,1))

plot(predSVMPLNS,testing$Volume)
abline(a=0,b=1,col="blue")

postResample(predSVMPLNS,testing$Volume)
rmse(testing$Volume,predSVMPLNS)


#realtive error read about it 
realErro<- (predSVMPLNS -testing$Volume)/testing$Volume  # to calc % 
realErro

mean(realErro)





























training <- existing_dum_PLNS[inTraining,] #subset training set
testing <- existing_dum_PLNS[-inTraining,]

