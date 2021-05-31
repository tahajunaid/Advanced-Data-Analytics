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


#remove variables that do not follow Normal Distribution
#df = subset(data, select = -c(FH_char1,FH_char11,FH_char13,FH_char14,
#                            SectionAlignment,FileAlignment,OH_DLLchar5,
#                            OH_DLLchar6, OH_DLLchar10))

#correlation plot
M <- cor(df)
corrplot(M, method = "circle",type="lower")
colnames(df)

#dropping variables with high correlation
df = subset(df, select = -c(e_cblp,e_cp,FH_char0,FH_char2,FH_char3,SizeOfCode,
                            SizeOfInitializedData,MajorSubsystemVersion,
                            MajorOperatingSystemVersion,MinorImageVersion,
                            AddressOfEntryPoint,BaseOfCode,OH_DLLchar0,OH_DLLchar2,
                            sus_sections,SizeOfHeapReserve,SizeOfStackReserve,
                            non_sus_sections,packer))
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
for(i in c(4,5,8,15,17,18,21,23,13,34,36,37,38,39,40)){
  row.names(df) <- NULL
  #boxplot(df[,i],main=colnames(df[i]))
  lower_bound <- quantile(df[,i], 0.025)
  upper_bound <- quantile(df[,i], 0.975)
  outlier_ind <- which(df[,i] < lower_bound | df[,i] > upper_bound)
  print(paste(i,"--",length(outlier_ind),"--",colnames(df[i]),dim(df)[1]))
  df<-df[-outlier_ind, ]
}


library(caret)
set.seed(101)
data<-df
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]
#Logistic Regression
mylogit <- glm(class ~ ., data = data, family = "binomial")
summary(mylogit)
logitResult=predict(mylogit, newdata = test, type = "response")
confusionMatrix(table(round(logitResult), test$class))


