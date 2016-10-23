setwd("E:/R_programming")
#train_data<-read.csv("/media/dharma/Local Disk/R_programming/Dataset/Black_friday/train.csv")
#test_data<-read.csv("/media/dharma/Local Disk/R_programming/Dataset/Black_friday/test-comb.csv")
train_data<-read.csv("E:/R_programming/Dataset/Black_friday/train.csv")
test_data<-read.csv("E:/R_programming/Dataset/Black_friday/test-comb.csv")
summary(train_data)
data.Female_below25<-train_data[train_data$Gender=="F"&train_data$Age=="18-25"
                                &train_data$Purchase>10000,]
test_data[,1]<-NULL
test_data$Comb<-NULL
test_data$Purchase<-as.integer(rep(0,dim(test_data)[1]))
combine.traintest<-rbind(train_data,test_data)
levels(combine.traintest$Age)
levels(combine.traintest$Age)[7]<-"55-80"
levels(combine.traintest$Age)
levels(combine.traintest$Stay_In_Current_City_Years)
levels(combine.traintest$Stay_In_Current_City_Years)[5]<-"4"
levels(combine.traintest$Stay_In_Current_City_Years)

#treating outliers seprately 
plot(combine.traintest$Gender,combine.traintest$Product_Category_1)
V<-plot(combine.traintest$Gender,combine.traintest$Product_Category_1)
O<-V$out
outlierData<-combine.traintest[O,]
plot(combine.traintest$Gender,combine.traintest$Product_Category_2)
plot(combine.traintest$Gender,combine.traintest$Product_Category_3)
model_outlier<-glm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1
          +Product_Category_2+Product_Category_3,data=outlierData)
test_outlier<-outlierData[rep(0,dim(outlierData))]

predict_out<-predict(model_outlier,test_outlier)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
attach(combine.traintest)
Product_category2fit<-rpart(Product_Category_2~Gender+Age+
                              Occupation+City_Category+Marital_Status,
                            data = combine.traintest[!is.na(combine.traintest$Product_Category_2),],
                            meth)
combine.traintest$Product_Category_2[is.na(combine.traintest$Product_Category_2)]<-
  predict(Product_category2fit,combine.traintest[is.na(combine.traintest$Product_Category_2),])
                   
Product_category3fit<-rpart(Product_Category_3~Gender+Age+
                              Occupation+City_Category+Marital_Status,
                            data = combine.traintest[!is.na(combine.traintest$Product_Category_3),],
                            method = "anova")
combine.traintest$Product_Category_3[is.na(combine.traintest$Product_Category_3)]<-
  predict(Product_category3fit,combine.traintest[is.na(combine.traintest$Product_Category_3),])
summary(combine.traintest)
train<-combine.traintest[1:550068,]
test<-combine.traintest[550069:783667,]
model<-glm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1
             +Product_Category_2+Product_Category_3,data=train)
prediction<-predict(model,test)
rmse<-function(error)
{
  sqrt(mean(error^2))
}
for (i in 1:550068) {
  error=train_data$Purchase[i]-prediction[i]
}
error1=train_data$Purchase-prediction
rmse(error)
submit<-data.frame(User_ID=test_data$User_ID,Product_ID=test_data$Product_ID,Purchase=prediction)
#First submission , soon with better solution 
write.csv(submit,file = "submission1.csv",row.names = FALSE)
