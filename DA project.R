library("dplyr")
library("superml")
library("corrplot")

data <- read.csv("C:/Users/Taha/Desktop/SEM6/Adv DA/ClaMP_Integrated-5184.csv")
head(data,30)
#understanding the data
summary(data)

#numeric columns of dataset
df_num<-data %>% dplyr::select(where(is.numeric))
dim(df_num)
#categorical columns of dataset
df_str<-data %>% select_if(~!is.numeric(.x))
dim(df_str)
colnames(df_str)
round(apply(data,2,sd))

#label encoding the categorical variable
length(unique(df_str$packer_type))
label <- LabelEncoder$new()
data$packer_type <- label$fit_transform(data$packer_type)
unique(data$packer_type)
#null values
lapply(data,function(x) { length(which(is.na(x)))})

#correlation plot
M <- cor(data)
corrplot(M, method = "circle",type="lower")
colnames(data)

#dropping variables with high correlation
df = subset(data, select = -c(e_cblp,e_cp,SizeOfCode,SizeOfInitializedData
                              ,MajorSubsystemVersion,MinorOperatingSystemVersion,
                              MajorImageVersion,AddressOfEntryPoint,BaseOfData,
                              SizeOfStackReserve,SizeOfHeapReserve,sus_sections,
                              non_sus_sections))
M <- cor(df)
corrplot(M, method = "circle",type="lower")

#find outliers
for(i in 1:ncol(df)){
  lower_bound <- quantile(df[,i], 0.025)
  upper_bound <- quantile(df[,i], 0.975)
  outlier_ind <- which(df[,i] < lower_bound | df[,i] > upper_bound)
  print(paste(i,"--",length(outlier_ind),"--",colnames(df[i])))
}

#remove outliers
for(i in c(1,2,3,4,5,6,23,24,25,29,30,31,32,33,34,51,52,53,54)){
  row.names(df) <- NULL
  #boxplot(df[,i],main=colnames(df[i]))
  lower_bound <- quantile(df[,i], 0.025)
  upper_bound <- quantile(df[,i], 0.975)
  outlier_ind <- which(df[,i] < lower_bound | df[,i] > upper_bound)
  print(paste(i,"--",length(outlier_ind),"--",colnames(df[i]),dim(df)[1]))
  df<-df[-outlier_ind, ]
}

#distribution plots
library(purrr)
library(tidyr)
library(ggplot2)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

library(caret)
set.seed(101)
data<-df
#data<-scale(subset(df, select = -c(class)))
#data<-as.data.frame(data)
#data<-cbind.data.frame(data,df$class)
#data<-data[,which(unlist(lapply(data, function(x) !all(is.na(x)))))]

dim(data)
head(data)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]
#Logistic Regression
mylogit <- glm(class ~ ., data = train, family = "binomial")
summary(mylogit)
logitResult=predict(mylogit, newdata = test, type = "response")
confusionMatrix(table(round(logitResult), test$class))


#SVM
library(e1071)
classifier = svm(formula = class ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel='polynomial',
                 na.action =
                   na.omit, scale = TRUE)
print(classifier)
y_pred = predict(classifier, newdata = test)
confusionMatrix(table(y_pred, test$class))


#Naive Bayes
NB=naiveBayes(as.factor(class)~ ., data=train)
NB
y_pred = predict(NB, newdata = test)
confusionMatrix(as.factor(test$class),as.factor(y_pred))

#Decision Tree
library(party)
library(magrittr)
DTmodel<- ctree(as.factor(class) ~ ., train)
plot(DTmodel)
y_pred<-predict(DTmodel, test) 
confusionMatrix(as.factor(test$class),as.factor(y_pred))

#Ensemble
##Bagging
###Random Forest
library(randomForest)
library(caret)
model = randomForest( class~ ., data=train)
print(model)
y_pred = predict(model, newdata = test)
confusionMatrix(as.factor(test$class),as.factor(round(y_pred)))

##Boosting 
###XGBoost
library("xgboost")

dtrain <- xgb.DMatrix(label = train$class, data = as.matrix(train))
dtest <- xgb.DMatrix(label = test$class, data = as.matrix(test))
xgb_model <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
y_pred<-predict(xgb_model,dtest)
confusionMatrix(as.factor(test$class),as.factor(round(y_pred)))

##Stacking
row.names(train) <- NULL
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)
#Defining the predictors and outcome
predictors<-colnames(train[-57])
outcomeName<-"class"
model_rf<-train(train[,predictors],train[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
#Predicting using random forest model
test$pred_rf<-predict(object = model_rf,test[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(as.factor(test$class),as.factor(round(test$pred_rf)))
#Training the knn model
model_knn<-train(train[,predictors],train[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
test$pred_knn<-predict(object = model_knn,test[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(as.factor(test$class),as.factor(round(test$pred_knn)))

#Training the Logistic regression model
model_lr<-train(train[,predictors],train[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting using lr
test$pred_lr<-predict(object = model_lr,test[,predictors])

#Checking the accuracy of the lr
confusionMatrix(as.factor(test$class),as.factor(round(test$pred_lr)))


#Taking average of predictions
test$pred_avg<-(test$pred_rf+test$pred_knn+test$pred_lr)/3

#Splitting into binary classes at 0.5
test$pred_avg<-as.factor(ifelse(test$pred_avg>0.5,'Y','N'))

#The majority vote
test$pred_majority<-as.factor(ifelse(test$pred_rf>=0.5 & test$pred_knn>=0.5,'Y',
                                     ifelse(test$pred_rf>=0.5 & test$pred_lr>=0.5,'Y',
                                            ifelse(test$pred_knn>=0.5 & test$pred_lr>=0.5,'Y','N'))))

#Taking weighted average of predictions
test$pred_weighted_avg<-(test$pred_rf*0.25)+(test$pred_knn*0.25)+(test$pred_lr*0.5)

#Splitting into binary classes at 0.5
test$pred_weighted_avg<-as.factor(ifelse(test$pred_weighted_avg>0.5,'Y','N'))

#Predicting the out of fold prediction probabilities for training data
train$OOF_pred_rf<-model_rf$pred$obs[order(model_rf$pred$rowIndex)]
train$OOF_pred_knn<-model_knn$pred$obs[order(model_knn$pred$rowIndex)]
train$OOF_pred_lr<-model_lr$pred$obs[order(model_lr$pred$rowIndex)]
#Predicting probabilities for the test data
test$OOF_pred_rf<-predict(model_rf,test[predictors])
test$OOF_pred_knn<-predict(model_knn,test[predictors])
test$OOF_pred_lr<-predict(model_lr,test[predictors])


#Predictors for top layer models 
predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_lr') 

#GBM as top layer model 
model_gbm<-  train(train[,predictors_top],train[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)

#predict using GBM top layer model
test$gbm_stacked<-predict(model_gbm,test[,predictors_top])

confusionMatrix(as.factor(test$class),as.factor(round(test$gbm_stacked)))
