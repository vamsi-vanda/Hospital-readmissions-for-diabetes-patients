
#Start time 10:00pm
library(nnet)
library(ggplot2)
library(corrplot)
library(rpart)
library(randomForest)
library(e1071)
library(corrplot)
library(tree)
library(e1071)

#Reading the Dataset and Converting all the "?" in the dataset to NA's
Train_Data = read.csv('/Users/vamsi_5/Desktop/data-challenge/training_data.csv', header = TRUE)
Test_Data = read.csv('/Users/vamsi_5/Desktop/data-challenge/test_data.csv', header = TRUE)


#From the frequency table, weight has 78844 unknown values, it doesnt help in prediction
ftable(Train_Data$weight)



#Removing columns which are unwanted to the prediction
summary(Train_Data)
Train_Data<- subset(Train_Data,select=-c( encounter_id,patient_nbr, weight,payer_code, medical_specialty,examide,citoglipton,metformin.rosiglitazone)) 
Test_Data<- subset(Test_Data,select=-c( patient_nbr, weight,payer_code, medical_specialty,examide,citoglipton,metformin.rosiglitazone)) 


#Removing the observations, which has "?" in one of the columns from 1 to 42.
for (i in 1: 42){
  Train_Data=Train_Data[Train_Data[i] != "?",]
  Test_Data= Test_Data[Test_Data[i] != "?",]
}

#Now the observations are 78466 of 42 variables
Test_Data_1 <- Test_Data[-1]

#Convert into factors
Train_Data_2 <- cbind(Train_Data[c(7:13,17)], lapply(Train_Data[c(1:6,14:16,18:42)],factor))
Test_Data_2 <- cbind(Test_Data_1[c(7:13,17)], lapply(Test_Data_1[c(1:6,14:16,18:41)],factor))
head(Train_Data_2)


table(Train_Data_2$readmitted)

#Data Visualization for insights
#Plots and Distributions

#Data in Age column is skewed towards the right
plot(Train_Data_2$age, main = "age distribution")

#Females are  55%, Males are 43%
plot(Train_Data_2$gender, main = "gender distribution")

#Caucassian are very high in compared to other races, they are followed by Africian_Americans.
plot(Train_Data_2$race, main = "Race")

#Plot between time_in_hospital vs age
g <- ggplot(Train_Data_2, aes(x=age, y=time_in_hospital))
g + geom_boxplot(aes(fill=readmitted))

#Readmission Comparison with gender
Gender_Table <- table(Train_Data_2$readmitted,Train_Data_2$gender)
Gender_Table

#Readmission Comparison with Age(Age's between 70 - 80 are the majority who gets readmitted)
Age_Table<- table(Train_Data_2$readmitted,Train_Data_2$age)
Age_Table

#Data Processing
#Diag_1, Diag_2 Diag_3 parameters has lot of levels reducing it. 
Train_Data_2$diag_1<- as.character(Train_Data_2$diag_1)
Train_Data_2$diag_2<- as.character(Train_Data_2$diag_2)
Train_Data_2$diag_3<- as.character(Train_Data_2$diag_3)

Test_Data_2$diag_1<- as.character(Test_Data_2$diag_1)
Test_Data_2$diag_2<- as.character(Test_Data_2$diag_2)
Test_Data_2$diag_3<- as.character(Test_Data_2$diag_3)

#Function to reduce the levels of diag_1, diag_2 and diag_3(There are lot of factors which need to be reduced)
func <- function(x) {
  if (startsWith(x, "E"))
  { value = 1
  }
  else if (startsWith(x, "V"))
  {
    value = 2
  }
  else
  {
    x = as.numeric(x)
    
    if (x < 140) {
      value = 1
    }
    else if (x >= 140 && x < 350) {
      value = 2
    }
    else if (x >= 350 && x < 550) {
      value = 3
    }
    else {
      value = 4
    }
  }
}
 

#Applying the function and applying as.factor
Train_Data_2$diag_1 <- lapply(Train_Data_2$diag_1,func)  
Train_Data_2$diag_1 <- as.factor(unlist(Train_Data_2$diag_1))

Train_Data_2$diag_2 <- lapply(Train_Data_2$diag_2,func)  
Train_Data_2$diag_2 <- as.factor(unlist(Train_Data_2$diag_2))

Train_Data_2$diag_3 <- lapply(Train_Data_2$diag_3,func)  
Train_Data_2$diag_3 <- as.factor(unlist(Train_Data_2$diag_3))

Test_Data_2$diag_1 <- lapply(Test_Data_2$diag_1,func)  
Test_Data_2$diag_1 <- as.factor(unlist(Test_Data_2$diag_1))

Test_Data_2$diag_2 <- lapply(Test_Data_2$diag_2,func)  
Test_Data_2$diag_2 <- as.factor(unlist(Test_Data_2$diag_2))

Test_Data_2$diag_3 <- lapply(Test_Data_2$diag_3,func)  
Test_Data_2$diag_3 <- as.factor(unlist(Test_Data_2$diag_3))


#Data Modelling
#Logisitc Regression : Linear model - It gives out probabilities to classify the binomial classes.
#Using all the columns for the prediction
fit_all <- glm(readmitted ~.- number_inpatient, data=Train_Data_2, family=binomial)

summary(fit_all)

#Using the test data and predicting with the help of glm model
pred <-predict(fit_all, newdata=Test_Data_2 ,type = "response")
final_pred <-ifelse((pred<0.3),"No","Yes")
table(final_pred)

#Tree based model, Random Forest is much more complex model than a linear model.
Rf_fit<-randomForest(formula=readmitted ~ age+admission_type_id+num_lab_procedures+num_procedures+num_medications
                     +number_outpatient+ number_emergency+number_inpatient+number_diagnoses+
                       insulin+change+diabetesMed+diag_1+diag_2+diag_3,
                     data=Train_Data_2)
pred1 <-predict(Rf_fit , newdata= Test_Data_2,type = "response")
table(pred1)



#Neural Networks, It is more complex than the above two models, NNet predicted all the test data into single class. 
#Complex models won't always give accurate results. 
nnet_model <- nnet(formula = readmitted ~ age+admission_type_id+num_lab_procedures+num_procedures+num_medications
                   +number_outpatient+ number_emergency+number_inpatient+number_diagnoses+
                     insulin+change+diabetesMed+diag_1+diag_2+diag_3,
                   data=Train_Data_2,size = 10, maxit = 100)
pred2 <-predict(nnet_model, newdata= Test_Data_2,type = "class")
table(pred2)


Test_Data$Readmittance <- final_pred
write.csv(Test_Data, "Vandana_Vamsi Krishna.csv")

#Endtime 12pm