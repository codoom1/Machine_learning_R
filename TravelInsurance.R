#load packages 
library(e1071)
library(caret)
library(klaR)

#import dataset
library(readr)
TravelInsurancePrediction <- read_csv(file.choose())
View(TravelInsurancePrediction)
str(TravelInsurancePrediction)

#changing character variables to factor
attach(TravelInsurancePrediction)

TravelInsurancePrediction$`Employment Type` <- factor(TravelInsurancePrediction$`Employment Type`)
TravelInsurancePrediction$GraduateOrNot <- factor(TravelInsurancePrediction$GraduateOrNot)
TravelInsurancePrediction$ChronicDiseases <- factor(TravelInsurancePrediction$ChronicDiseases)
TravelInsurancePrediction$FrequentFlyer <- factor(TravelInsurancePrediction$FrequentFlyer)
TravelInsurancePrediction$EverTravelledAbroad <- factor(TravelInsurancePrediction$EverTravelledAbroad)
TravelInsurancePrediction$TravelInsurance<- factor(TravelInsurancePrediction$TravelInsurance)

str(TravelInsurancePrediction)

# define an 80%/20% train/test split of the dataset
set.seed(1235)

trainIndex <- sample(2,nrow(TravelInsurancePrediction),replace = TRUE, prob = c(0.8,0.2) )
trainData <- TravelInsurancePrediction[trainIndex==1,]
testData <- TravelInsurancePrediction[trainIndex==2,]

View(trainData)
View(testData)


# train a support vector machine model
svm_model <- svm(TravelInsurance ~ .,data = trainData)
print(svm_model)
summary(svm_model)

# test model with testData and predict
svm_pred <- predict(svm_model, testData)  
print(svm_pred) 

#check accuracy
svm_table <- table(svm_pred,testData$TravelInsurance)
print(svm_table)

svm_result <- confusionMatrix(svm_table)
print(svm_result)

#Resampling Methods 

#Define training control

#Repeated cross validation
fitControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(1235)
svm_model1 <- train(TravelInsurance ~ .,data = TravelInsurancePrediction, trControl = fitControl1, method = "svmRadial")
print(svm_model1)


#cross validation
fitControl2 <- trainControl(method = "cv", number = 10)
set.seed(1235)
svm_model2 <- train(TravelInsurance ~ .,data = TravelInsurancePrediction, trControl = fitControl2, method = "svmRadial")
print(svm_model2)


#Leave one out cross validation
fitControl3 <- trainControl(method = "loocv", number = 10)
set.seed(1235)
svm_model3 <- train(TravelInsurance ~ .,data = TravelInsurancePrediction, trControl = fitControl3, method = "svmRadial")
print(svm_model3)


#Bootstrapping
fitControl4 <- trainControl(method = "boot", number = 10)
set.seed(1235)
svm_model4 <- train(TravelInsurance ~ .,data = TravelInsurancePrediction, trControl = fitControl4, method = "svmRadial")
print(svm_model4)


#exploring dataset
library(ggplot2)
library(ggthemes)

#Age
ggplot(data = TravelInsurancePrediction, aes(Age,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#GraduateOrNot
ggplot(data = TravelInsurancePrediction, aes(GraduateOrNot,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#Employment Type
ggplot(data = TravelInsurancePrediction, aes(`Employment Type`,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#Chronic Diseases
ggplot(data = TravelInsurancePrediction, aes(ChronicDiseases,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#FrequentFlyer
ggplot(data = TravelInsurancePrediction, aes(FrequentFlyer,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#EverTravelledAbroad
ggplot(data = TravelInsurancePrediction, aes(EverTravelledAbroad,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

#TravelInsurance
ggplot(data = TravelInsurancePrediction, aes(TravelInsurance,fill= TravelInsurance))+
  geom_bar(stat="count") + 
  theme_economist()

