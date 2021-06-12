#multilinear regression

data <- read.csv("C:/Users/Taha/Desktop/vehicle.csv")
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
length(unique(df_str$State))
label <- LabelEncoder$new()
data$State <- label$fit_transform(data$State)
unique(data$State)
#null values
lapply(data,function(x) { length(which(is.na(x)))})

plot(data, col="navy", main="Matrix Scatterplot")

M <- cor(data)
corrplot(M, method = "circle",type="lower")
colnames(data)

#dropping variables with high correlation
#df = subset(data, select = -c(lc))
M <- cor(data)
corrplot(M, method = "circle",type="lower")
library(caret)
set.seed(101)

#find outliers
for(i in 1:ncol(data)){
  lower_bound <- quantile(data[,i], 0.025)
  upper_bound <- quantile(data[,i], 0.975)
  outlier_ind <- which(data[,i] < lower_bound | data[,i] > upper_bound)
  print(paste(i,"--",length(outlier_ind),"--",colnames(data[i])))
}

#remove outliers
for(i in 1:ncol(data)){
  row.names(data) <- NULL
  #boxplot(df[,i],main=colnames(df[i]))
  lower_bound <- quantile(data[,i], 0.025)
  upper_bound <- quantile(data[,i], 0.975)
  outlier_ind <- which(data[,i] < lower_bound | data[,i] > upper_bound)
  #print(paste(i,"--",length(outlier_ind),"--",colnames(df[i]),dim(df)[1]))
  data<-data[-outlier_ind, ]
}
#data = subset(data, select = -c(Vehicle))


dim(data)
head(data)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

model <- lm(lc~lh+Mileage, data = train)
summary(model)
y_pred<-predict(model,test)
mean((abs(y_pred-test$lc)/test$lc)*100)
plot(y_pred,test$lc)
