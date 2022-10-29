#Packages
library(caret) #Classification And Regression Trees
library(randomForest) #Random Forest
library(rpart);library(e1071) #Decision Trees
library(neuralnet) #Artificial Neural Network
library(MASS) #Discriminant Data Analysis
library(hmeasure)
library(gbm)
library(xgboost) #Gradient Boosting Methods
library(C50) #C50  

#Data importing and cleaning
#Loading libraries
library(caret) #Classification and Regression Training
library(ggplot2) #Data Visualization
library(ggthemes) #Extra themes of the ggplot
#library(dplyr) #Data manipulation
library(corrplot)
library(caTools) #Tools for moving statistics like AUC, ROC etc
#library(rattle) #Grapical user interface for data science
#library(RColorBrewer) #Color brewwer oarlettes
#library(rpart.plot) #Regression Trees

#importing the heart dataset
hd <- read.csv(file.choose())

head(hd) #viewing the first 6 observations
attach(hd)
View(hd)

colSums(is.na(hd)) #checking for NA values

#Correlation Plot
dd=hd[,-c(1,3,4)]
head(dd)
corrplot(cor(dd),method = "circle", order = "FPC")


#diplaying the number of rows, columns and the structure of the dataset
str(hd)
dim(hd) #view number of rows & columns

#converting all categorical features as factors
hd$GENDER<-factor(hd$GENDER)
hd$Disease<-factor(hd$Disease)
hd$EDUCATION<-factor(hd$EDUCATION)

str(hd) #checking the structure of the data again after converting all caterical features as factors

dd1=hd
summary(dd1)
View(dd1)

#Histogram plots
par(mfrow = c(2,3))
hist(dd1$AGE)
hist(dd1$SYSTOLIC)
hist(dd1$CHOL)
hist(dd1$DIASTOLIC)
hist(dd1$WC)
#hist(dd1$BMI)








######################Data Exploralatory####################
#Number of observations: DIABETIC and NON-DIABETIC
# Disease
ggplot(dd1,aes(Disease, fill=Disease)) +
  geom_bar(stat="count") + 
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("Disease Bar chart plot")

# Diesease with Age
ggplot(dd1,aes(AGE, fill=Disease)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~Disease, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  xlab("AGE") +
  ylab("Density / Count") +
  ggtitle("Age Histogram")

# Disease with GENDER
ggplot(dd1,aes(Disease, fill=Disease)) +
  geom_bar(stat="count") +
  facet_wrap(~GENDER, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("GENDER Bar chart plot")

# Disease with EDUCATION
ggplot(dd1,aes(Disease, fill=Disease)) +
  geom_bar(stat="count") +
  facet_wrap(~EDUCATION, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("EDUCATION Bar chart plot")

#Disease with BMI
ggplot(dd1,aes(BMI, fill=Disease)) +
  geom_histogram(aes(y=..density..), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~Disease, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  xlab("BODY MASS INDEX") +
  ylab("Density / Count") +
  ggtitle("BMI Histogram")





#disease with WC
ggplot(dd1,aes(WC, fill=Disease)) +
  geom_histogram(aes(y=..density..), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~Disease, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("Waist Circumference Histogram") +
  xlab("WC") +
  ylab("Density / Count")

#disease with Hc
ggplot(dd1,aes(HC, fill=Disease)) +
  geom_histogram(aes(y=..density..), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~Disease, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("springgreen2","firebrick2")) +
  ggtitle("Hip Circumference Histogram") +
  xlab("HC") +
  ylab("Density / Count")



#Data Splitting
set.seed(1237)
tr.dat <- sample(nrow(dd1), .90*nrow(dd1), replace = FALSE)
TrainSet <- dd1[tr.dat,]
ValidSet <- dd1[-tr.dat,]

#Checking the proportions of the splitted data
#The splitting proportion is 80/20 percent
prop.table(table(dd1$Disease))
prop.table(table(TrainSet$Disease))
prop.table(table(ValidSet$Disease))


TrainSet$Disease<-make.names(TrainSet$Disease)
set.seed(142)
TrainSet$Disease<-as.factor(TrainSet$Disease)

#Tuning Parameters
fitControl <- trainControl(method = "repeatedcv", number = 10,
                            repeats = 10, classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "grid", 
                           preProcOptions = c("center", "scale"))

#Random Forest Model
randf <- caret::train(Disease ~ ., data = TrainSet, method = "rf",
                  trControl = fitControl, metric = "ROC")
randf
varImp(randf)



#Predicting
pred.rf <- predict(randf,ValidSet)  
head(pred.rf)
#levels(pred.rf)[1] <- "Diabetes"
tab.rf <-table(ValidSet$Disease, pred.rf)
tab.rf

head(ValidSet$Disease)
head(dd1)

res.randf <- caret::confusionMatrix(ValidSet$Disease, pred.rf)
F1 <- res.randf$byClass[-c(1:4,9:11)]



#Decision Tree
dt <- caret::train(Disease ~., data = TrainSet, method = "rpart", 
                   trControl = fitControl, metric = "ROC")
dt

varImp(dt)

pred.dt <- predict(dt,ValidSet)
head(pred.dt)
#levels(pred.dt)[1] <- "Diabetes"

tab.dt<-table(ValidSet$Disease, pred.dt)
head(tab.dt)
res.dt<-caret::confusionMatrix(ValidSet$Disease, pred.dt)
 summary(dd1$Disease)

F2 <- res.dt$byClass[-c(1:4,9:11)]


#C50
#c5 <- caret::train(ValidSet, TrainSet, method = "C5.0",
 #                  trControl = fitControl, metric = "ROC")
#c5
##varImp(c5)

#pred.c5 <- predict(c5,ValidSet)
##tab.c5<-table(ValidSet$target, pred.c5)
#res.c5<-caret::confusionMatrix(tab.c5, positive="Heart Disease")
#F3 <- res.c5$byClass[-c(1:4,9:11)]



#Artificial Neural Networks
ann <- caret::train(Disease ~., data = TrainSet, method = "nnet", 
                   trControl = fitControl, metric = "ROC")
ann
varImp(ann)

pred.ann <- predict(ann,ValidSet)
#levels(pred.ann)[2] <- "Heart Disease"
tab.ann<-table(ValidSet$Disease, pred.ann)
res.ann<-caret::confusionMatrix(tab.ann)
F4 <- res.ann$byClass[-c(1:4,9:11)]



#Support Vector Machine: Linear
svml <- svm(Disease ~., data = TrainSet, type = "C-classification",
            kernel = "linear")
svml
pred.svml <- predict(svml,ValidSet)
#levels(pred.svml)[2] <- "Heart Disease"
tab.svml<-table(ValidSet$Disease, pred.svml)
res.svml<-caret::confusionMatrix(tab.svml)
F5 <- res.svml$byClass[-c(1:4,9:11)]


#Support Vector Machine: Polynomial
svmp <- svm(Disease ~., data = TrainSet, type = "C-classification", 
            kernel = "polynomial")
svmp
pred.svmp <- predict(svmp,ValidSet)
#levels(pred.svmp)[2] <- "Heart Disease"
tab.svmp<-table(ValidSet$Disease, pred.svmp)
res.svmp<-caret::confusionMatrix(tab.svmp)
F6 <- res.svmp$byClass[-c(1:4,9:11)]

#Support Vector Machine: Radial
svmr <- svm(Disease ~., data = TrainSet, type = "C-classification", 
            kernel = "radial")
svmr
pred.svmr <- predict(svmr,ValidSet)
#levels(pred.svmr)[2] <- "Heart Disease"
tab.svmr<-table(ValidSet$Disease, pred.svmr)
res.svmr<-caret::confusionMatrix(tab.svmr)
F7 <- res.svmr$byClass[-c(1:4,9:11)]

#Linear Discriminant Analysis
lida <- caret::train(Disease ~., data = TrainSet, method = "lda", 
                    trControl = fitControl, metric = "ROC")
lida
pred.lida <- predict(lida,ValidSet)
#levels(pred.lida)[2] <- "Heart Disease"
tab.lida<-table(ValidSet$Disease, pred.lida)
res.lida<-caret::confusionMatrix(tab.lida)
F8 <- res.lida$byClass[-c(1:4,9:11)]


#Quadratic Discriminant Analysis
#qqda <- caret::train(Disease ~., data = TrainSet, method = "qda", 
 #                    trControl = fitControl, metric = "ROC")
#qqda
#pred.qqda <- predict(qqda,ValidSet)
#levels(pred.qqda)[2] <- "Heart Disease"
#tab.qqda<-table(ValidSet$target, pred.qqda)
#res.qqda<-caret::confusionMatrix(tab.qqda, positive="Heart Disease")
#F9 <- res.qqda$byClass[-c(1:4,9:11)]

#Naive Bayes
nav <- caret::train(Disease ~., data = TrainSet, method ="naive_bayes"  , 
                     trControl = fitControl, metric = "ROC")
nav
pred.nav <- predict(nav,ValidSet)
#levels(pred.lida)[2] <- "Heart Disease"
tab.nav<-table(ValidSet$Disease, pred.nav)
res.nav<-caret::confusionMatrix(tab.nav)
F9 <- res.nav$byClass[-c(1:4,9:11)]



#Stochastic Gradient Boost
sgb <- train(Disease ~., data = TrainSet, method = "gbm", 
              metric = "ROC", trControl = fitControl)
sgb
pred.sgb <- predict(sgb,ValidSet)
#levels(pred.sgb)[2] <- "Heart Disease"
tab.sgb<-table(ValidSet$Disease, pred.sgb)
res.sgb<-caret::confusionMatrix(tab.sgb)
F10 <- res.sgb$byClass[-c(1:4,9:11)]



#K Nearest Neighbor
knn <- train(Disease ~., data = TrainSet, method = "knn", 
             metric = "ROC", trControl = fitControl)
knn
pred.knn <- predict(knn,ValidSet)
#levels(pred.sgb)[2] <- "Heart Disease"
tab.knn<-table(ValidSet$Disease, pred.knn)
res.knn<-caret::confusionMatrix(tab.knn)
F11 <- res.knn$byClass[-c(1:4,9:11)]

#Logistic regression
lr <- train(Disease ~., data = TrainSet, method = "glm", 
             metric = "ROC", trControl = fitControl)
lr
pred.lr <- predict(lr,ValidSet)
#levels(pred.sgb)[2] <- "Heart Disease"
tab.lr<-table(ValidSet$Disease, pred.lr)
res.lr<-caret::confusionMatrix(tab.lr)
F12 <- res.slr$byClass[-c(1:4,9:11)]



#Measuring Accuracy
Accuracy <- round(data.frame(RF=res.randf$overall[1:2], DT=res.dt$overall[1:2], 
                         ANN=res.ann$overall[1:2],
                       SVM=res.svml$overall[1:2], LDA=res.lida$overall[1:2],
                        SGB=res.sgb$overall[1:2],LR=res.lr$overall[1:2],
                       SGB=res.sgb$overall[1:2],SGB=res.sgb$overall[1:2]),4)

#F Measure
F1_score <- data.frame(RF=F1, DT=F2, ANN=F4, SVML=F5,
                       SVMP=F6, SVMR=F7, LQA=F8, SGB=F10)













#############################################################
s1 <- dd1[sample(nrow(dd1),30, replace = T),]
#Tuning Parameters
fitControl <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 10, classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "grid", 
                           preProcOptions = c("center", "scale"))
#Empty Bins
n_s1 <- 5
ar=ad=ac=an=asl=asp=asr=asg=rep(0,n_s1)


for(i in 1:n_s1){
  #Data Splitting
  tr.dat <- sample(nrow(s1), .8*nrow(s1), replace = FALSE)
  trSet <- s1[tr.dat,]
  valSet <- s1[-tr.dat,]
  trSet$Disease<-make.names(trSet$Disease)
  set.seed(142)
  trSet$Disease<-as.factor(trSet$Disease)
  
  #Random Forest Model
  randf <- caret::train(Disease ~ ., data = trSet, method = "rf",
                        trControl = fitControl, metric = "ROC", replace = T,
                        importance = T)
  
  #Predicting
  pred.rf <- predict(randf,valSet)
  #levels(pred.rf)[2] <- "Heart Disease"
  tab.rf <-table(valSet$Disease, pred.rf)
  tab.df.rf <- as.data.frame(tab.rf)
  res.randf <- caret::confusionMatrix(tab.rf)
  ar[i] <- res.randf$overall[1]

  #Decision Tree
  dt <- caret::train(Disease ~., data = trSet, method = "rpart", 
                     trControl = fitControl, metric = "ROC")
  
  pred.dt <- predict(dt,valSet)
  #levels(pred.dt)[2] <- "Heart Disease"
  tab.dt<-table(valSet$Disease, pred.dt)
  res.dt <-caret::confusionMatrix(tab.dt)
  ad[i] <- res.dt$overall[1]
  
  #Support Vector Machine
  svmd <- svm(Disease ~., data = trSet, type = "C-classification", 
              cross = 3, kernel = "linear", metric = "ROC")
  
  pred.svmd <- predict(svmd,valSet)
  #levels(pred.svmd)[2] <- "Heart Disease"
  tab.svmd<-table(valSet$Disease, pred.svmd)
  res.svmd<-caret::confusionMatrix(tab.svmd)
  as[i] <- res.svmd$overall[1]
  
  #Artificial Neural Networks
  ann <- caret::train(Disease ~., data = trSet, method = "nnet", 
                      trControl = fitControl, metric = "ROC")
  
  pred.ann <- predict(ann,valSet)
  #levels(pred.ann)[2] <- "Heart Disease"
  tab.ann<-table(valSet$Disease, pred.ann)
  res.ann<-caret::confusionMatrix(tab.ann)
  an[i] <- res.ann$overall[1]
  
}





#Data Splitting
set.seed(1237)
tr.dat <- sample(nrow(s1), .60*nrow(s1), replace = FALSE)
TrainSet <- heart.ds[tr.dat,]
ValidSet <- heart.ds[-tr.dat,]


#Checking the proportions of the splitted data
#The splitting proportion is 80/20 percent
prop.table(table(s1$target))
prop.table(table(s1$target))
prop.table(table(s1$target))

