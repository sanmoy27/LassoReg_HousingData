setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\Project\\data\\lassoLinear")
library(MASS)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)

trainData<-read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData<-read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
dim(trainData)
dim(testData)

trainData$Id<-NULL
testData$Id<-NULL
testData$SalePrice<-NA

houseData<-trainData

dim(houseData)
summary(houseData)

cat("\014")
detectCl<-function(x){
  return(class(x))
}
lapply(houseData, detectCl)


detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(houseData, detectNAs)






cat("\014")
houseData$MSSubClass[which(is.na(houseData$MSSubClass))]<-median(houseData$MSSubClass, na.rm=TRUE)
houseData$MSZoning[which(is.na(houseData$MSZoning))]<-"RL"
houseData$LotFrontage[which(is.na(houseData$LotFrontage))]<-median(houseData$LotFrontage, na.rm=TRUE)
houseData$Alley[which(is.na(houseData$Alley))]<-"Pave"
houseData$Utilities[which(is.na(houseData$Utilities))]<-"AllPub"
houseData$MasVnrArea[which(is.na(houseData$MasVnrArea))]<-median(houseData$MasVnrArea, na.rm=TRUE)
houseData$BsmtQual[which(is.na(houseData$BsmtQual))]<-"Gd"
houseData$BsmtCond[which(is.na(houseData$BsmtCond))]<-"TA"
houseData$BsmtExposure[which(is.na(houseData$BsmtExposure))]<-"No"
houseData$BsmtFinType1[which(is.na(houseData$BsmtFinType1))]<-"Unf"
houseData$BsmtFinSF1[which(is.na(houseData$BsmtFinSF1))]<-median(houseData$BsmtFinSF1, na.rm=TRUE)
houseData$BsmtFinType2[which(is.na(houseData$BsmtFinType2))]<-"Unf"
houseData$BsmtFinSF2[which(is.na(houseData$BsmtFinSF2))]<-median(houseData$BsmtFinSF2, na.rm=TRUE)
houseData$TotalBsmtSF[which(is.na(houseData$TotalBsmtSF))]<-median(houseData$TotalBsmtSF, na.rm=TRUE)
houseData$Electrical[which(is.na(houseData$Electrical))]<-"Mix"
houseData$BsmtFullBath[which(is.na(houseData$BsmtFullBath))]<-median(houseData$BsmtFullBath, na.rm=TRUE)
houseData$BsmtHalfBath[which(is.na(houseData$BsmtHalfBath))]<-median(houseData$BsmtHalfBath, na.rm=TRUE)
houseData$TotRmsAbvGrd[which(is.na(houseData$TotRmsAbvGrd))]<-median(houseData$TotRmsAbvGrd, na.rm=TRUE)
houseData$FireplaceQu[which(is.na(houseData$FireplaceQu))]<-"Gd"
houseData$GarageType[which(is.na(houseData$GarageType))]<-"Attchd"
houseData$GarageYrBlt[which(is.na(houseData$GarageYrBlt))]<-median(houseData$GarageYrBlt, na.rm=TRUE)
houseData$GarageFinish[which(is.na(houseData$GarageFinish))]<-"TA"
houseData$GarageCars[which(is.na(houseData$GarageCars))]<-median(houseData$GarageCars, na.rm=TRUE)
houseData$GarageArea[which(is.na(houseData$GarageArea))]<-median(houseData$GarageArea, na.rm=TRUE)
houseData$GarageQual[which(is.na(houseData$GarageQual))]<-"TA"
houseData$GarageCond[which(is.na(houseData$GarageCond))]<-"TA"
houseData$PoolQC[which(is.na(houseData$PoolQC))]<-"Ex"
houseData$Fence[which(is.na(houseData$Fence))]<-"MnPrv"
houseData$MiscFeature[which(is.na(houseData$MiscFeature))]<-"Shed"
houseData$SaleType[which(is.na(houseData$SaleType))]<-"COD"
houseData$MasVnrType[which(is.na(houseData$MasVnrType))]<-"None"
houseData$KitchenQual[which(is.na(houseData$KitchenQual))]<-"TA"
houseData$GarageFinish[which(is.na(houseData$GarageFinish))]<-"Unf"
#houseData$SalePrice[which(is.na(houseData$SalePrice))]<-median(houseData$SalePrice, na.rm=TRUE)


houseData$YearRemodAdd<-as.integer(houseData$YearRemodAdd)
houseData$PavedDrive<-as.factor(houseData$PavedDrive)
houseData$GarageCars<-as.factor(houseData$GarageCars)
houseData$GarageFinish<-as.factor(houseData$GarageFinish)
houseData$Street<-as.factor(houseData$Street)
houseData$Alley<-as.factor(houseData$Alley)
houseData$LotShape<-as.factor(houseData$LotShape)
houseData$LandContour<-as.factor(houseData$LandContour)

houseData$Utilities<-as.factor(houseData$Utilities)
houseData$LotConfig<-as.factor(houseData$LotConfig)
houseData$LandSlope<-as.factor(houseData$LandSlope)
houseData$BldgType<-as.factor(houseData$BldgType)
houseData$HouseStyle<-as.factor(houseData$HouseStyle)
houseData$RoofStyle<-as.factor(houseData$RoofStyle)
houseData$RoofMatl<-as.factor(houseData$RoofMatl)
houseData$MasVnrType<-as.factor(houseData$MasVnrType)
houseData$ExterQual<-as.factor(houseData$ExterQual)
houseData$ExterCond<-as.factor(houseData$ExterCond)
houseData$Foundation<-as.factor(houseData$Foundation)
houseData$BsmtQual<-as.factor(houseData$BsmtQual)
houseData$BsmtCond<-as.factor(houseData$BsmtCond)
houseData$BsmtExposure<-as.factor(houseData$BsmtExposure)
houseData$BsmtFinType1<-as.factor(houseData$BsmtFinType1)
houseData$BsmtFinType2<-as.factor(houseData$BsmtFinType2)
houseData$Heating<-as.factor(houseData$Heating)
houseData$HeatingQC<-as.factor(houseData$HeatingQC)
houseData$CentralAir<-as.factor(houseData$CentralAir)
houseData$Electrical<-as.factor(houseData$Electrical)
houseData$KitchenQual<-as.factor(houseData$KitchenQual)
houseData$Functional<-as.factor(houseData$Functional)
houseData$FireplaceQu<-as.factor(houseData$FireplaceQu)
houseData$GarageType<-as.factor(houseData$GarageType)
houseData$GarageQual<-as.factor(houseData$GarageQual)
houseData$GarageCond<-as.factor(houseData$GarageCond)
houseData$PoolQC<-as.factor(houseData$PoolQC)
houseData$Fence<-as.factor(houseData$Fence)
houseData$MiscFeature<-as.factor(houseData$MiscFeature)
houseData$SaleCondition<-as.factor(houseData$SaleCondition)

## Fitting Lasso Regression ###
set.seed(100) # set seed to replicate results
split<-sample.split(houseData$SalePrice, SplitRatio=0.8)
train_set<-subset(houseData, split==TRUE)
test_set<-subset(houseData, split==FALSE)

set.seed(1234)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))






lasso_mod <- train(SalePrice~., data=houseData, method="glmnet", trControl= my_control, tuneGrid=lassoGrid)
max(lasso_mod$results$Rsquared)

cat("\014")
lassoPred<-predict(lasso_mod, newdata = test_set)
lassoPred


