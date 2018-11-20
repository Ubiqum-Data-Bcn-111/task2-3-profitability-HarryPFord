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
corr_existing_out <- cor(existing_out[,2:15]) #dont include product type
corr_existing_out
#nice plot of the correlation matrix
pMat2 <- cor.mtest(corr conf.level = .95)#########################for later remove product type 
corrplot(corr_existing_out,order="hclust",method="number",p.mat = pmat2)
##################################################################################################


###############################################################################################
#remove variables that correlate between them (>0.5, <-0.5)
#5star,profit margin, 3star, 2star, 1star, negative service review, width and height
existing_out$x5StarReviews <- NULL
existing_out
existing_out_less <- subset(existing_out,select=c(x4StarReviews,PositiveServiceReview,Volume,ShippingWeight,Price,ProductDepth))
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
#Train and Tune the SVM
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#change this to a randome range maybe like gamma = 10^(-5:-1), cost = 10^(-3:1))



# Use the expand.grid to specify the search space	
SVMgrid <- expand.grid(sigma = c(.01, .015, 0.2),
                                   C = c(0.75, 0.9, 1, 1.1, 1.25)
#for loop for atrubites maybe would be nice  
                       
svm_modelAll <- train(Volume ~., 
                      data = training, 
                      method="svmRadial", 
                      trControl=trctrl,
                      tuneGrid = SVMgrid,
                      preProc = c("scale","center"),
                      metric="RMSE",
                      verbose=FALSE)

svm_modelOne <- train(Volume ~x4StarReviews, 
                      data = training, 
                      method="svmRadial", 
                      trControl=trctrl,
                      tuneGrid = SVMgrid,
                      preProc = c("scale","center"),
                      metric="RMSE",
                      verbose=FALSE)

svm_modelTwo <- train(Volume ~x4StarReviews + PositiveServiceReview, 
                      data = training, 
                      method="svmRadial", 
                      trControl=trctrl,
                      tuneGrid = SVMgrid,
                      preProc = c("scale","center"),
                      metric= "RMSE",
                      verbose=FALSE)



svm_modelThree <- train(Volume ~x4StarReviews + PositiveServiceReview + ShippingWeight, 
                        data = training, 
                        method="svmRadial", 
                        trControl=trctrl,
                        tuneGrid = SVMgrid,
                        preProc = c("scale","center"),
                        metric= "RMSE",
                        verbose=FALSE)

svm_modelFour<- train(Volume ~x4StarReviews + PositiveServiceReview + ShippingWeight +Price, 
                       data = training, 
                       method="svmRadial", 
                       trControl=trctrl,
                       tuneGrid = SVMgrid,
                       preProc = c("scale","center"),
                       metric= "RMSE",
                       verbose=FALSE)

svm_modelFife <- train(Volume ~x4StarReviews + PositiveServiceReview + ShippingWeight +Price +ProductDepth, 
                       data = training, 
                       method="svmRadial", 
                       trControl=trctrl,
                       tuneGrid = SVMgrid,
                       preProc = c("scale","center"),
                       metric= "RMSE",
                       verbose=FALSE)




rValues <- resamples(list(svm_modelAll,svm_modelOne,svm_modelTwo,svm_modelThree,svm_modelFour,svm_modelFife))
rValues$values

summary(rValues)
bwplot(rValues,metric="RMSE",ylab =c("radial kernel"),main="SVM Models")



#> my_prediction <- predict(my_model, test_set[,-1])
#test_set[,-1] removes  ???? the first column (the class column) to make the predictions only based on the features of the data. You should remove the column that labels your data.


predSVMall <- predict(svm_modelAll,newdata = testing)
predSVMOne <- predict(svm_modelOne,newdata = testing)
predSVMtwo <- predict(svm_modelTwo,newdata = testing)
predSVMThree <- predict(svm_modelThree,newdata = testing)
predSVMFour <- predict(svm_modelFour,newdata = testing)
predSVMFife <- predict(svm_modelFife,newdata = testing)











predSVMOne
par(mfrow=c(1,1))

plot(predSVMFife,testing$Volume)
abline(a=0,b=1,col="blue")

postResample(predSVMall,testing$Volume)
postResample(predSVMOne,testing$Volume)
postResample(predSVMtwo,testing$Volume)
postResample(predSVMThree,testing$Volume)
postResample(predSVMFour,testing$Volume)
postResample(predSVMFife,testing$Volume)



#realtive error read about it 
realErroall<- (predSVMall -testing$Volume)/testing$Volume  # to calc % 
realErroOne<- (predSVMOne-testing$Volume)/testing$Volume 
realErroTwo<- (predSVMtwo -testing$Volume)/testing$Volume 
realErroThree<- (predSVMThree -testing$Volume)/testing$Volume 
realErroFour<- (predSVMFour -testing$Volume)/testing$Volume 
realErroFive<- (predSVMFife -testing$Volume)/testing$Volume 

mean(realErroall)
mean(realErroOne)
mean(realErroTwo)
mean(realErroThree)
mean(realErroFour)
mean(realErroFive)
pal=c("lightblue","mistyrose")
plot(predSVMThree,testing$Volume)
abline(a=0,b=1,col="mistyrose")




























training <- existing_dum_PLNS[inTraining,] #subset training set
testing <- existing_dum_PLNS[-inTraining,]

