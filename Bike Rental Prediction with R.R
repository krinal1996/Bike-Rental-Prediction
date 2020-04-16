getwd()

#Loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
      
#install.packages(x)
lapply(x, require, character.only = TRUE)      

#Load dataset
df = read.csv('day.csv')

#EXPLORING DATA
head(df)
summary(df)
str(df)
dim(df)

df$dteday <- format(as.Date(df$dteday,format = "%Y-%m-%d"))
df$instant <- NULL
hist(df$cnt)

#Converting to proper dtype
cat_var = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday','weathersit')
num_var = c('temp', 'atemp', 'hum', 'windspeed','casual','registered')

#Data Type Conversion Function
typ_conv = function(df,var,type){
  df[var] = lapply(df[var], type)
  return(df)
}

df = typ_conv(df,cat_var, factor)

#Missing Values Analysis
apply(df,2,function(x){sum(is.na(x))})

hist(df$casual)
hist(df$registered)
hist(df$cnt)

#Correlation Plot
corrgram(df[,c('temp','atemp','hum','windspeed','cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

df = subset(df, select=-c(temp,hum,windspeed))

#Chi-Square Test
#Checking dependency among different categorical variables
cat_var = c('dteday','season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday','weathersit')
cat_df = df[,cat_var]

for (i in cat_var){
  for (j in cat_var){
    print(i)
    print(j)
    print(chisq.test(table(cat_df[,i], cat_df[,j]))$p.value)
  }
}

#anova test
anova_res = aov(cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit,data = df)
summary(anova_res)

df = subset(df, select=-c(mnth,holiday,dteday))

library(usdm)
vif(df)

df = subset(df, select=-c(workingday))

#Feature Scaling
#Normalization of cnt
df$cnt = (df$cnt - min(df$cnt)) / (max(df$cnt) - min(df$cnt))
df$casual = (df$casual - min(df$casual)) / (max(df$casual) - min(df$casual))

#sampling
set.seed(40)
t_index = sample(1:nrow(df), 0.7*nrow(df))
train = df[t_index,]
test = df[-t_index,]

library(DataCombine)
rmExcept(c("test","train",'df'))

library(caret)
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}

#Linear model
lr_model = lm(cnt ~. ,data=train)
summary(lr_model)
#R-squared:  0.8153

#predictions on Train data set
LR_predict = predict(lr_model, test[,-7])
plot(test$cnt, LR_predict,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'LR model')
mape(test$cnt, LR_predict)
#mape: 17.40673


#Decison tree
library(rpart.plot)
library(rpart)

set.seed(40)
#model
dt_model = rpart(cnt~. , data = train, method = "anova")
summary(dt_model)
plt = rpart.plot(dt_model, type = 5, digits = 2, fallen.leaves = TRUE)
DT_Predict = predict(dt_model, test[,-7])
plot(test$cnt, DT_Predict,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'DT model')
mape(test$cnt, DT_Predict)
#MAPE: 23.68175

#Random forest
library(randomForest)
library(inTrees)
set.seed(40)
#model
rf_model = randomForest(cnt ~. , train, importance = TRUE, ntree = 500)
rf_model

RF_predict = predict(rf_model, test[,-7])
plot(test$cnt, RF_predict,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'RF model')

mape(test$cnt, RF_predict)
#Mape: 14.33421


#Hence depending on the various Mape score of different Machine Learning model, we select Random Forrest 
