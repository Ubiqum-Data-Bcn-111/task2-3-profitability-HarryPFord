# Multiple regression in R file --- dummy comment to be removed 
library(pacman)

ExistingProducts<- read.csv("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task3/existingproductattributes2017.2.csv")
NewProducts<- read.csv("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task3/newproductattributes2017.2.csv")

str(ExistingProducts)
str(NewProducts)

#dummy files 

DummySet<- dummyVars("~.", data=ExistingProducts)
DummySet
readData<-data.frame(predict(DummySet,newdata = ExistingProducts))
str((readData))
summary(DummySet)
#removing NA
str(readData) 
summary(readData)
readData$BestSellersRank<-NULL

#corrData 
corrData<- cor(readData)
corrData
######################################################################
library(corrplot)



#this might be a waste of time 
#tl.cex - size of labels , tl(text label control) method is method 
corrplot(corrData, tl.cex = 0.5,method = "number",tl.pos="n")


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(corrData)
head(p.mat[, 1:5])

corrplot(corrData, order="hclust", 
         p.mat = p.mat, sig.level = 0.01,tl.cex = 0.5)
##################################################################
BestAttributes <- rpart( ProductType~. -x5StarReviews,data=ExistingProducts,method = "anova")
rpart.plot(BestAttributes, type=1,digits = 5, fallen.leaves = FALSE)

tree=rpart(target,data = ExistingProducts,method = "anova")


##################TESTING TRANIG SET########################
set.seed(123)


#creating testing and traning set 
#inTrain <- createDataPartition(
#  y=S$brand,
#  p=0.7,
#  list=FALSE
#)

#training <- SurveyComplete[inTrain,]
#testing <- SurveyComplete[-inTrain,]

trainSize<-round(nrom())


###################Models##################################

LinearModelTest<-lm(Volume ~.)
