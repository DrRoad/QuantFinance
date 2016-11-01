# install.packages("pROC")
# install.packages("corrplot")
# install.packages("FSelector")
# install.packages("randomForest")
# install.packages("kernlab")
# install.packages("rpart")
#install.packages("rJava")
library(quantmod)
library(TTR)
library(caret)
library(corrplot)
library(pROC)
library(FSelector)
setwd("C:\\UM\\R\\R for QF\\predictive modeling for algorithmic trading")
#list.files()
df_stock = read.csv("BAJAJ-AUTO 5 Yr data.csv")
df_index = read.csv("NIFTY 5 Yr data.csv")
# View(df_stock)
price = df_stock$Last-df_stock$Open
class = ifelse(price>0,"UP","DOWN")

forceindex = (df_stock$Last - df_stock$Open)*df_stock$Vol
forceindex = c(NA,head(forceindex,-1))


# buy and sell signal indicators(william's %R and RSI)

# %R = (Highest High - Close)/(Highest High - Lowest Low) * -100
# Lowest Low = lowest low for the look-back period
# Highest High = highest high for the look-back period
# %R is multiplied by -100 correct the inversion and move the decimal.         

willr5 = WPR(df_stock[,c('High','Low','Last')],n=5)
willr5 = c(NA,head(willr5,-1))
# View(willr5)
willr10 = WPR(df_stock[,c('High','Low','Last')],n=10)
willr10 = c(NA,head(willr10,-1))
willr15 = WPR(df_stock[,c('High','Low','Last')],n=15)
willr15 = c(NA,head(willr15,-1))
# RSI = 100 - 100 / (1 + RS)
# Where RS = Average gain of up periods during the specified time frame / Average loss of down periods during the specified time frame/

RSI5 = RSI(df_stock$Last,n=5,maType = "WMA")
RSI5 = c(NA,head(RSI5,-1))
RSI10 = RSI(df_stock$Last,n=10,maType = "WMA")
RSI10 = c(NA,head(RSI10,-1))
RSI15 = RSI(df_stock$Last,n=15,maType = "WMA")
RSI15 = c(NA,head(RSI15,-1))

#price change indicators (ROC and Momentum)
#ROC = [(Close - Close n periods ago) / (Close n periods ago)] * 100
ROC5 = ROC(df_stock$Last,n=5,type='discrete')*100
ROC5 = c(NA,head(ROC5,-1))
ROC10 = ROC(df_stock$Last,n=5,type='discrete')*100
ROC10 = c(NA,head(ROC5,-1))
MOM5 = momentum(df_stock$Last,n=5,na.pad = TRUE)
MOM5 = c(NA,head(MOM5,-1))
MOM10 = momentum(df_stock$Last,n=10,na.pad = TRUE)
MOM10 = c(NA,head(MOM10,-1))
MOM5Index = momentum(df_stock$Last,n=5,na.pad = TRUE)
MOM5Index = c(NA,head(MOM5Index,-1))
MOM10Index = momentum(df_stock$Last,n=10,na.pad = TRUE)
MOM10Index = c(NA,head(MOM10Index,-1))

#volatility signal indicator (ATR  average true range)
# Method 1: Current High less the current Low
# Method 2: Current High less the previous Close (absolute value)
# Method 3: Current Low less the previous Close (absolute value)
# Current ATR = [(Prior ATR x 13) + Current TR] / 14
ATR5 = ATR(df_stock[,c('High','Low','Last')],n=5,maType = "WMA")[,1]
ATR5 = c(NA,head(ATR5,-1))
ATR10 = ATR(df_stock[,c('High','Low','Last')],n=10,maType = "WMA")[,1]
ATR10 = c(NA,head(ATR10,-1))
ATR5Index = ATR(df_index[,c('High','Low','Last')],n=5,maType = "WMA")[,1]
ATR5Index = c(NA,head(ATR5Index,-1))
ATR10Index = ATR(df_index[,c('High','Low','Last')],n=10,maType = "WMA")[,1]
ATR10Index = c(NA,head(ATR10Index,-1))
dataset = data.frame(class,forceindex,willr5,willr10,willr15,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,ATR5,ATR10,MOM5Index,MOM10Index,ATR5Index,ATR10Index)
dataset = na.omit(dataset)

print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y),percentage=prop.table(table(y)*100))
summary(dataset)

correlations = cor(dataset[,c(2:18)])
print(head(correlations))
corrplot(correlations,method="circle")

set.seed(5)
weights = random.forest.importance(class~.,dataset,importance.type = 1)
print(weights)

subset=cutoff.k(weights,10)
dataset_rf = data.frame(class,forceindex,willr5,willr10,RSI5,RSI10,ROC5,MOM10,MOM10Index)
dataset_rf = na.omit(dataset_rf)

trainControl = trainControl(method="cv",number = 10)
metric = 'Accuracy'

fit.knn = train(class~.,data=dataset_rf,method="knn",metric=metric,preProc=c("range"),trControl=trainControl)
fit.cart = train(class~.,data=dataset_rf,method="rpart",metric=metric,preProc=c("range"),trControl=trainControl)
fit.nb = train(class~.,data=dataset_rf,method="nb",metric=metric,preProc=c("range"),trControl=trainControl)
fit.svm = train(class~.,data=dataset_rf,method="svmRadial",metric=metric,preProc=c("range"),trControl=trainControl)

results = resamples(list(KNN=fit.knn,CART=fit.cart,NB=fit.nb,SVM=fit.svm))
summary(results)
dotplot(results)

set.seed(5)
grid = expand.grid(.k=seq(1,10,by=1))
fit.knn = train(class~.,data=dataset_rf,method="knn",metric=metric,tuneGrid=grid,prepProc=c("range"),trControl=trainControl)
print(fit.knn)