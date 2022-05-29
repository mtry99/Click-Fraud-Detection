##### LIBRARIES #####

library(neuralnet)


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

### Scale features
minimums <- apply(Data[,1:5], 2, min)
ranges <- apply(Data[,1:5],2,max) - apply(Data[,1:5],2,min)

DataScaled <- scale(Data[,1:5],minimums,ranges)
DataScaled <- as.data.frame(DataScaled)

DataScaled$is_attributed <- as.factor(Data$is_attributed)

### Create test and train sets
SampleSize <- round(0.75*nrow(DataScaled))
SampleIndex <- sample(1:nrow(DataScaled),size=SampleSize)

TrainSet <- DataScaled[SampleIndex,]
TestSet <- DataScaled[-SampleIndex,]


##### NEURAL NETWORK MODEL: MODEL 1 #####

#### Model 1: 4,2 hidden layer ####
### Train neural network 
Fit1 <- neuralnet(is_attributed~ip + app + device + os + channel,
                  data=TrainSet,hidden = c(4,2),act.fct = "logistic",threshold = 0.05,
                  stepmax = 10000,rep=1,linear.output = FALSE)
plot(Fit1)

### Base error rates
# train base error rate
baseTrainRate <- mean(TrainSet$is_attributed == 1)
baseTrainRate
# test base rate error
baseTestRate <- mean(TestSet$is_attributed == 1)
baseTestRate

#### Train error
PredictionsTrain <- compute(Fit1,TrainSet)
PredictedClass <- apply(PredictionsTrain$net.result,1, which.max)
PredictedClass <- ifelse(PredictedClass == 1, 0, 1)
TrainError <- mean(PredictedClass!=TrainSet$is_attributed)
print(TrainError)

### Test error
PredictionsTest <- compute(Fit1,TestSet)
PredictedClass2 <- apply(PredictionsTest$net.result,1, which.max)
PredictedClass2 <- ifelse(PredictedClass2 == 1, 0, 1)
TestError <- mean(PredictedClass2 != TestSet$is_attributed)
print(TestError)

### Accuracy rates
# FP Rate for test set
FPrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 0)/ sum(TestSet$is_attributed == 0)
print(FPrate2)

# FN Rate for test set
FNrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 1)/ sum(TestSet$is_attributed == 1)
print(FNrate2)

##### NEURAL NETWORK MODEL: MODEL 2 #####

#### Model 2: 5,5,5,5 hidden layer ####
Fit2 <- neuralnet(is_attributed~ip + app + device + os + channel,
                  data=TrainSet,hidden = c(5,5,5,5),act.fct = "logistic",threshold = 0.05,
                  stepmax = 10000,rep=1,linear.output = FALSE) 
plot(Fit2)

### Base error rates
# train base error rate
baseTrainRate
# test base rate error
baseTestRate

#### Train error
PredictionsTrain <- compute(Fit2,TrainSet)
PredictedClass <- apply(PredictionsTrain$net.result,1, which.max)
PredictedClass <- ifelse(PredictedClass == 1, 0, 1)
TrainError <- mean(PredictedClass!=TrainSet$is_attributed)
print(TrainError)

### Test error
PredictionsTest <- compute(Fit2,TestSet)
PredictedClass2 <- apply(PredictionsTest$net.result,1, which.max)
PredictedClass2 <- ifelse(PredictedClass2 == 1, 0, 1)
TestError <- mean(PredictedClass2 != TestSet$is_attributed)
print(TestError)

### Accuracy rates
# FP Rate for test set
FPrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 0)/ sum(TestSet$is_attributed == 0)
print(FPrate2)

# FN Rate for test set
FNrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 1)/ sum(TestSet$is_attributed == 1)
print(FNrate2)