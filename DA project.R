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

#Random Forest
library(randomForest)
library(caret)
model = randomForest( class~ ., data=train)
print(model)
y_pred = predict(model, newdata = test)
confusionMatrix(as.factor(test$class),as.factor(round(y_pred)))

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

#Bagging

#boosting


