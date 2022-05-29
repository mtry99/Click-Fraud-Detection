##### LIBRARIES #####

library(rpart)


##### PREPARE DATASET #####

### Original dataset
Data1 <- read.csv("train_sample.csv")

# we use count since across ip, device, app, channel, the is_attributed field will be the same
Consolidated <- count(Data1, c("ip", "device", "app", "channel", "os", "is_attributed"))

Data <- Consolidated


Data$is_attributed <- ifelse(Data$is_attributed == 1, TRUE,FALSE)

Data$is_attributed <- as.factor(Data$is_attributed)
Data$channel <- as.factor(Data$channel)
Data$app <- as.factor(Data$app)
Data$device <- as.factor(Data$device)
Data$os <- as.factor(Data$os)

### Train and test set
SampleSize <- round(0.75*nrow(Data),0)
SampleIndex <- sample(1:nrow(Data),size=SampleSize)
TrainSet <- Data[SampleIndex,]
TestSet <- Data[-SampleIndex,]


##### INDUCTION TREE MODEL #####

fit1 <- rpart(is_attributed~ip + channel + app + device + os + freq, data=TrainSet, method="class", cp=0.00867)
printcp(fit1)
plotcp(fit1)
print(fit1)
plot(fit1, uniform=TRUE, main="Classification Tree")
text(fit1, use.n=TRUE, all=TRUE, cex=.8)

### Train error
trainPrediction <- predict(fit1,TrainSet, type = "class")
mean(TrainSet$is_attributed!=trainPrediction)

### Test error
testPrediction <- predict(fit1, TestSet, type="class")
mean(TestSet$is_attributed!=testPrediction)

### Base error rates
#base error rate for Train Set
mean(TrainSet$is_attributed == TRUE)
#base error rate for Test Set
mean(TestSet$is_attributed == TRUE)

### Accuracy rates
LogitTestFNrate <- sum(testPrediction!=TestSet$is_attributed & TestSet$is_attributed==TRUE)/sum(TestSet$is_attributed==TRUE)
LogitTestFNrate

LogitTestFNrate <- sum(testPrediction!=TestSet$is_attributed & TestSet$is_attributed==FALSE)/sum(TestSet$is_attributed==FALSE)
LogitTestFNrate

