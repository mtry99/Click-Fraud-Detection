##### PREPARE DATASET #####

### Original dataset
# run this section for original dataset
Data <- read.csv("train_sample.csv")

### Smote dataset
# run this section for smote dataset
Data <- read.csv("smoteDataset.csv")
Data$X <- NULL

### Base error rate
baseErrorRate <- nrow(Data[Data$is_attributed==1,]) / nrow(Data)
baseErrorRate

Data$is_attributed <- ifelse(Data$is_attributed == 1, TRUE,FALSE)

### Train and test set
SampleSize <- round(0.75*nrow(Data),0)
SampleIndex <- sample(1:nrow(Data),size=SampleSize)
TrainSet <- Data[SampleIndex,]
TestSet <- Data[-SampleIndex,]

##### LOGISTIC REGRESSION MODEL #####

Fit1 <-glm(is_attributed ~ app+ device + channel + ip + os,
           family = "binomial",data = TrainSet)
Fit1 <- step(Fit1)
summary(Fit1)

### Find best cut
X <- c(seq(0,1,0.01))
ErrorVector <- c(0:100)
Y=0
for (cut in X)
{
  LogitTrainPrediction3 <- Fit1$fitted.values>cut
  LogitTrainError <- mean(LogitTrainPrediction3!=TrainSet$is_attributed)
  Y=Y+1
  ErrorVector[Y] <- LogitTrainError
}  
# best cut determined to be 0.5
plot(X,ErrorVector,main="Error versus cut")
summary(ErrorVector)
length(which(Fit1$fitted<0.02))
length(which(Fit1$fitted<0.001))
length(which(Fit1$fitted<0.0001))
length(which(Fit1$fitted<0.00000000000000000000001))
BaseErrorRate <- mean(TrainSet$is_attributed==FALSE)

### Train error and base error rate
cut <- 0.5

baseErrorRate <- nrow(TrainSet[TrainSet$is_attributed==1,]) / nrow(TrainSet)
baseErrorRate

LogitTrainPrediction3 <- Fit1$fitted.values>cut
summary(LogitTrainPrediction3)
LogitTrainError <- mean(LogitTrainPrediction3!=TrainSet$is_attributed)
LogitTrainError

### Test error and base error rate
baseErrorRate <- nrow(TestSet[TestSet$is_attributed==1,]) / nrow(TestSet)
baseErrorRate

LogitTestPrediction3 <- predict(Fit1,TestSet,type = "response")>cut
summary(LogitTestPrediction3)
LogitTestError <- mean(LogitTestPrediction3 != TestSet$is_attributed)
LogitTestError


### Accuracy rates
# false negative rate
fn <- sum(LogitTestPrediction3 != TestSet$is_attributed &
            TestSet$is_attributed == 1) / sum(TestSet$is_attributed == 1)
fn

# false positive rate
fp <- sum(LogitTestPrediction3 != TestSet$is_attributed &
            TestSet$is_attributed == 0) / sum(TestSet$is_attributed == 0)
fp
