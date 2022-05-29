
#-------------------------------c(5,5,5,5)
library(performanceEstimation)
Data1<- read.csv("train_sample.csv")

# we use count since across ip, device, app, channel, the is_attributed field will be the same
Consolidated <- count(Data1, c("ip", "device", "app", "channel", "os", "is_attributed"))

Data <- Consolidated


#scale the first 5 features

list <- c(1,2,3,4,5,7)
minimums <- apply(Data[,list], 2, min)
print (minimums)

ranges <- apply(Data[,list],2,max) - apply(Data[,list],2,min)
print (ranges)

DataScaled <- scale(Data[,list],minimums,ranges)
DataScaled <- as.data.frame(DataScaled)

DataScaled$is_attributed <- as.factor(Data$is_attributed)
#create train and test sets

str(DataScaled)
SampleSize <- round(0.75*nrow(DataScaled))
SampleIndex <- sample(1:nrow(DataScaled),size=SampleSize)

TrainSet <- DataScaled[SampleIndex,]
TestSet <- DataScaled[-SampleIndex,]

#train the neural net

library(neuralnet)
Fit1 <- neuralnet(is_attributed~ip + app + device + os + channel + freq,
                  data=TrainSet,hidden = c(5,5,5,5),act.fct = "logistic",threshold = 0.05,
                  stepmax = 10000,rep=1,linear.output = FALSE) 

plot(Fit1)



#train base rate error
baseErrorRate <- mean(TrainSet$is_attributed == 1)
baseErrorRate
#test base rate error
baseErrorRate1 <- mean(TestSet$is_attributed == 1)
baseErrorRate1

#get train error

PredictionsTrain <- compute(Fit1,TrainSet)
PredictedClass <- apply(PredictionsTrain$net.result,1, which.max)
PredictedClass[1:5]
PredictedClass <- ifelse(PredictedClass == 1, 0, 1)
TrainError <- mean(PredictedClass!=TrainSet$is_attributed)
print(TrainError)


#get test error

PredictionsTest <- compute(Fit1,TestSet)
PredictedClass2 <- apply(PredictionsTest$net.result,1, which.max)
PredictedClass2 <- ifelse(PredictedClass2 == 1, 0, 1)
TestError <- mean(PredictedClass2 != TestSet$is_attributed)
print(TestError)

#FP Rate for train set
FPrate <- sum(PredictedClass != TrainSet$is_attributed 
              & TrainSet$is_attributed == 0)/ sum(TrainSet$is_attributed == 0)
print(FPrate)

#FP Rate for test set
FPrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 0)/ sum(TestSet$is_attributed == 0)
print(FPrate2)

#FN Rate for test set
FNrate2 <- sum(PredictedClass2 != TestSet$is_attributed 
               & TestSet$is_attributed == 1)/ sum(TestSet$is_attributed == 1)
print(FNrate2)

