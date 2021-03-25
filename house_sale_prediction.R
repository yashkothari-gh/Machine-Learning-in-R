df<-read.csv("C:/Users/yash/Documents/R files/house_sale.csv",header = TRUE)
str(df)       #BASIC INFORMATION ABOUT DATA
summary(df)   #EXTENDED DATA DICTIONARY
#Use boxplots on quantitative variables to find outliers.Example below
boxplot(df$n_hot_rooms)
#Now we would begin with outliers treatment
#capping and flooring method
uv<-quantile(df$n_hot_rooms,0.99)*3
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)
lv<-quantile(df$rainfall,0.01)*0.3
df$rainfall[df$rainfall<lv]<-lv
summary(df$rainfall)
#now variables with missing values need to be treated
#we will replace missing values with mean of that variable
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)
#perform variable transformation
df$avg_dist<-(df$dist1+df$dist2+df$dist3+df$dist4)/4
df2=df  #creating a copy of dataset
df<-df[,-6:-9]   #removing like variables
df<-df[,-13]     #removing bus_ter column as it consits of one level and is thus useless.
library(dummies) #for creation of dummy variables
df<-dummy.data.frame(df) #it will create dummy variables in case of categorical variables
#removing useless columns formed after dummy variable creation
df<-df[,-8]  
df<-df[,-13]
#Now we create a logistic regression model and train it using our preprocessed data
glm.fit = glm(Sold~.,data = df,family = binomial)
summary(glm.fit)
#predicting values of sold variable and storing it in glm.probs
glm.probs=predict(glm.fit,type = "response")
glm.probs[1:10]

#creating predictor array with two classes "YES and "NO and setting threshold=0.5
glm.pred = rep("NO",506)
glm.pred[glm.probs>0.5]="YES"
table(glm.pred,df$Sold)
#accuracy=68.3%

#creating predictor array with two classes "YES and "NO and setting threshold=0.8
glm.pred1 = rep("NO",506)
glm.pred1[glm.probs>0.8]="YES"
table(glm.pred1,df$Sold)
#accuracy=60%

#creating predictor array with two classes "YES and "NO and setting threshold=0.7
glm.pred2 = rep("NO",506)
glm.pred2[glm.probs>0.7]="YES"
table(glm.pred2,df$Sold)
#accuracy=64.22%

#now splitting training and testing set
#then training model using training set
#and then testing model using testing set

library(caTools)
split = sample.split(df,SplitRatio = 0.8)
train_set = subset(df, split==TRUE)
test_set = subset(df, split==FALSE)
train.fit =glm(Sold~.,train_set,family = binomial)
glm.probs1=predict(train.fit,test_set,type = "response")
test.pred = rep("NO",120)
test.pred[glm.probs1>0.5]="YES"
table(test.pred,test_set$Sold)
#accuracy=68.33%