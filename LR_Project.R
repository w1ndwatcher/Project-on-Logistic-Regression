#The assigned project is a logistic regression.
#Logistic regression is for prediction of categorical values, whether something will happen or not
#To predict: Heart disease will happen or not.

### CLEANING OF DATA ###
#STEP 1: Set the directory
getwd()
setwd("D:/R-Programming")

#STEP 2: Load the dataset
heart_data <- read.csv("heart.csv")
View(heart_data)

#STEP 3: Understand the data
head(heart_data)
#age: The person's age in years
#sex: The person's sex (1 = male, 0 = female)
#cp: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)
#trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)
#chol: The person's cholesterol measurement in mg/dl
#fbs: The person's fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)
#restecg: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
#thalach: The person's maximum heart_data rate achieved
#exang: Exercise induced angina (1 = yes; 0 = no)
#oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot. See more here)
#slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
#ca: The number of major vessels (0-3)
#thal: A blood disorder called thalassemia (3 = normal; 6 = fixed defect; 7 = reversable defect)
#target: heart_data disease (0 = no, 1 = yes)

#STEP 4: Change name of the columns as required
names(heart_data)[1] <- "age"
View(heart_data)

#STEP 5: Check structure, class and summary
str(heart_data)
dim(heart_data)
table(is.na(heart_data))
#no NA values 
summary(heart_data)

#STEP 5: Change classes of columns as per understanding
heart_data$sex <- as.factor(heart_data$sex)
levels(heart_data$sex)
heart_data$cp <- as.factor(heart_data$cp)
levels(heart_data$cp)
heart_data$fbs <- as.factor(heart_data$fbs)
levels(heart_data$fbs)
heart_data$restecg <- as.factor(heart_data$restecg)
levels(heart_data$restecg)
heart_data$exang <- as.factor(heart_data$exang)
levels(heart_data$exang)
heart_data$slope <- as.factor(heart_data$slope)
levels(heart_data$slope)
heart_data$ca <- as.factor(heart_data$ca)
levels(heart_data$ca)
heart_data$thal <- as.factor(heart_data$thal)
levels(heart_data$thal)
heart_data$target <- as.factor(heart_data$target)
levels(heart_data$target)
str(heart_data)


#categorising age for the ease of analysis
heart_data$age_Category <- ifelse(heart_data$age<21,"<21",
                             ifelse(((heart_data$age>=21) & (heart_data$age<=25)),"21-25",
                                    ifelse(((heart_data$age>25) & (heart_data$age<=30)),"25-30",
                                           ifelse(((heart_data$age>30) & (heart_data$age<=35)),"30-35",
                                                  ifelse(((heart_data$age>35) & (heart_data$age<=40)),"35-40",
                                                         ifelse(((heart_data$age>40) & (heart_data$age<=50)),"40-50",
                                                                ifelse(((heart_data$age>50) & (heart_data$age<=60)),"50-60",">60"))))))
)
heart_data$age_Category                                     
View(heart_data)

head(heart_data$age)
head(heart_data$age_Category)

class(heart_data$age_Category)
str(heart_data)

# Since the age category is in character class,
# We must change it to factor.

heart_data$age_Category <- as.factor(heart_data$age_Category)
class(heart_data$age_Category)

str(heart_data)

#STEP 6: EXPLORATORY ANALYSIS #
a <- heart_data[heart_data$target == 1,]
a
#filters data of individuals that have heart_data disease.
summary(a)
nrow(a)

b <- heart_data[heart_data$target == 0,]
b
#filters data of individuals that do not have heart_data disease
summary(b)
nrow(b)

#subsets a and b have different no. of rows, which will make it difficult to make comparison
#we make subsets containing equal number of rows
heart_disease <- a[1:100,]
no_disease <- b[1:100,]
nrow(heart_disease)
nrow(no_disease)
#both subsets have equal number of rows

hist(heart_data$age,col = "blue",main = "age distribution", ylab = "no. of individuals", xlab = "age")
hist(heart_disease$age, col = "blue", main = "age vs heart_data disease", ylab = "no. of individuals", xlab = "age")
#represents age distribution of people with heart_data disease
#maximum chances of heart_data disease is between the age group 50-60 years
summary(heart_disease$age)
# 52 years -> peak age for heart_data disease
summary(no_disease$age)

hist(heart_data$trestbps,col = "yellow",main = "Resting Blood Pressure", ylab = "no. of individuals", xlab = "BP/mmHg")
hist(heart_disease$trestbps,col = "yellow",main = "Resting Blood Pressure", ylab = "no. of individuals", xlab = "BP/mmHg")
#mean blood pressure = 130mmHg (median from summary)
#most no. of individuals suffering from heart_data disease have resting blood pressure near the average value.
hist(no_disease$trestbps,col = "yellow",main = "Resting Blood Pressure", ylab = "no. of individuals", xlab = "BP/mmHg")
summary(heart_disease$trestbps)
summary(no_disease$trestbps)
#mean bp similar for diseased and non-diseased groups => not a good predictive feature

hist(heart_data$chol,col = "yellow",main = "Serum chol Level", ylab = "no. of individuals", xlab = "chol Level")
hist(heart_disease$chol,col = "yellow",main = "Serum chol Level", ylab = "no. of individuals", xlab = "chol Level")
hist(no_disease$chol,col = "yellow",main = "Serum chol Level", ylab = "no. of individuals", xlab = "chol Level")
summary(heart_disease$chol)
summary(no_disease$chol)
#most people with heart_data disease have chol higher than 200mg/dL.

hist(heart_data$thalach,col = "pink",main = "thalach achieved", ylab = "no. of individuals", xlab = "thalach")
hist(heart_disease$thalach,col = "pink",main = "thalach achieved", ylab = "no. of individuals", xlab = "thalach")
hist(no_disease$thalach,col = "pink",main = "thalach achieved", ylab = "no. of individuals", xlab = "thalach")
summary(heart_disease$thalach)
summary(no_disease$thalach)
#maximum heart_data rate of diseased patients is lower than that seen in a healthy person

hist(heart_data$oldpeak,col = "green",main = "oldpeak", xlab = "oldpeak")
hist(heart_disease$oldpeak,col = "green",main = "oldpeak", xlab = "oldpeak")
hist(no_disease$oldpeak,col = "green",main = "oldpeak", xlab = "oldpeak")
summary(heart_disease$oldpeak)
summary(no_disease$oldpeak)
#individuals with heart_data disease show st segment depression induced by exercise.
#st depression value is more in people without heart disease although more people with heart disease have it in the range 0-0.5. 

library(ggplot2)

ggplot(heart_data,aes(age_Category))+geom_bar(fill="blue")
ggplot(heart_data,aes(age_Category,trestbps))+geom_boxplot(fill="green")
ggplot(heart_data,aes(age_Category,chol))+geom_boxplot(fill="red")
ggplot(heart_data,aes(age_Category,thalach))+geom_boxplot(fill="yellow")
ggplot(heart_data,aes(age_Category,oldpeak))+geom_boxplot(fill="orange")
ggplot(heart_data,aes(age_Category,age))+geom_boxplot(fill="purple")

names(heart_data)

ggplot(heart_disease, aes(sex)) + geom_bar(fill = "pink")
#more number of males have heart disease
ggplot(heart_disease, aes(cp)) + geom_bar(fill = "steel blue")
ggplot(no_disease, aes(cp)) + geom_bar(fill = "steel blue")
ggplot(heart_disease, aes(fbs)) + geom_bar(fill = "orange")
ggplot(no_disease, aes(fbs)) + geom_bar(fill = "orange")
#A slightly larger proportion of diseased patients show higher levels of blood sugar.
#Most individuals do not have fasting blood sugar levels > 120mg/dL.
ggplot(heart_disease, aes(restecg)) + geom_bar(fill = "dark green")
ggplot(no_disease, aes(restecg)) + geom_bar(fill = "dark green")
#A very large proportion of individuals with heart disease report ST-T wave abnormality in their ecg.
#Very few individuals reported hyperthropy.
ggplot(heart_disease, aes(exang)) + geom_bar(fill = "brown")
ggplot(no_disease, aes(exang)) + geom_bar(fill = "brown")
#More number of individuals without heart disease experienced exercise induced angina.
ggplot(heart_disease, aes(slope)) + geom_bar(fill = "purple")
ggplot(no_disease, aes(slope)) + geom_bar(fill = "purple")
#ST depression induced by exercise relative to rest is observed more in individuals with heart disease while a large proportion of non diseased people show flat or normal st slope.
ggplot(heart_disease, aes(ca)) + geom_bar(fill = "yellow")
ggplot(no_disease, aes(ca)) + geom_bar(fill = "yellow")
#Individuals with heart disease have one or more major arteries blocked.
ggplot(heart_disease, aes(thal)) + geom_bar(fill = "pink")
ggplot(no_disease, aes(thal)) + geom_bar(fill = "pink")
#Most number of individuals with heart disease have fixed defect of thalassemia.


#STEP 7:Splitting into Test & Train Data
set.seed(1234)

library(caret)
ind<-as.vector(createDataPartition(heart_data$target,p=0.60,list = FALSE))

heart_data_train<- as.data.frame(heart_data[ind,])
heart_data_train
heart_data_test<-as.data.frame(heart_data[-ind,])
heart_data_test
#exporting the test and train data
write.csv(heart_data_train, file = "heart_train_data.csv")
write.csv(heart_data_test, file = "heart_test_data.csv")

nrow(heart_data_train)
nrow(heart_data_test)  # Data gets divided into test data , train data


#STEP 8: Selecting relevant variables for correlation

#a) Checking Correlation
str(heart_data)

DB_Corr <- cor(heart_data[,c(1,4,5,8,10)])
View(DB_Corr)
# +ve sign shows positively related (ie positive realtion) , 
# -ve related shows negitively related (ie negitive relation)

# visual representation for correlation
library(corrplot)
corrplot(DB_Corr,method = "circle")
#Positive correlations are displayed in blue and negative correlations in orange color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients.
# bigger size of circle shows high correlation


# b) Mutilcollinearity :

library(usdm)
vif(heart_data)
#age has the highest vif value. Therefore, we can remove it to reduce multicollinearity
vif(heart_data[,c(4,5,8,10)])
#the vif values show that multicollinearity has been removed

# c) By checking the p values of each variable
heart_data_model <- glm(target~.,data = heart_data_train,family = "binomial")
options(scipen = 999)
summary(heart_data_model)
#according to the p variables(threshold taken p<0.05), the important variables that can be taken are

## d)step model
step_model <- step(heart_data_model)
#according to step model, the important variables that can be taken are
#thal oldpeak slope sex cp  ca

#STEP 9: Modelling

#We will make a model of all our possible combination of variables and select the best fit by checking their accuracy

#MODEL A: with variables- sex cp thalach ca 
modelA <- glm(target~sex+ cp + thalach+ ca ,data = heart_data_train,family = "binomial")
modelA
summary(modelA)
#AIC=167.32

#MODEL B: with variables- sex cp ca 
modelB <- glm(target~sex+ cp + ca ,data = heart_data_train,family = "binomial")
modelB
summary(modelB)

#MODEL C: with variables- thal oldpeak slope sex cp  ca
modelC <- glm(target~sex+ cp + thal+ ca+ oldpeak+ slope ,data = heart_data_train,family = "binomial")
modelC
summary(modelC)
#AIC=148.01


#STEP 10: Prediction 

#for MODEL A
PredictionA <- predict(modelA,newdata=heart_data_test,type="response")
PredictionA
summary(PredictionA)

#for MODEL B
PredictionB <- predict(modelB,newdata=heart_data_test,type="response")
PredictionB
summary(PredictionB)

#for MODEL C
PredictionC <- predict(modelC,newdata=heart_data_test,type="response")
PredictionC
summary(PredictionC)


#STEP 11: Accuracy

# Since we are not given any preferences
# We could take threshold = 0.7

#for MODEL A
fitted_valuesA <- modelA$fitted.values
fitted_valuesA
summary(fitted_valuesA)

residualsA <- modelA$residuals
residualsA
summary(residualsA)

fitted.resultsA <- ifelse(PredictionA>0.7,1,0)
fitted.resultsA
View(fitted.resultsA)


#for MODEL B
fitted_valuesB <- modelB$fitted.values
fitted_valuesB
summary(fitted_valuesB)

residualsB <- modelB$residuals
residualsB
summary(residualsB)

fitted.resultsB <- ifelse(PredictionB>0.7,1,0)
fitted.resultsB

View(fitted.resultsB)


#for MODEL C
fitted_valuesC <- modelC$fitted.values
fitted_valuesC
summary(fitted_valuesC)

residualsC <- modelC$residuals
residualsC

summary(residualsC)

fitted.resultsC <- ifelse(PredictionC>0.7,1,0)
fitted.resultsC
View(fitted.resultsC)
write.csv(fitted.resultsC, file = "Predicted_results.csv")


# Acuuracy -- 2 ways
# 1. use confusion matrix
# 2. using where predicted == actuals


#MODEL A
ErrorA <- mean(fitted.resultsA!=heart_data_test$target)
print(paste('Accuracy for Model A= ',1-ErrorA))
## Gives us ACC : 0.793388429752066

#MODEL B
ErrorB <- mean(fitted.resultsB!=heart_data_test$target)
print(paste('Accuracy for Model B= ',1-ErrorB))
## Gives us ACC : 0.785123966942149

#MODEL C
ErrorC <- mean(fitted.resultsC!=heart_data_test$target)
print(paste('Accuracy for Model C= ',1-ErrorC))
## Gives us ACC : 0.851239669421488


# Confusion matrix

#MODEL A
table(heart_data_test$target,PredictionA>0.7)
# accuracy is ((44+52) / (44+52+11+14)) = 0.79338842975

#MODEL B
table(heart_data_test$target,PredictionB>0.7)
# accuracy is ((42+53) / (42+53+13+13)) = 0.78512396694

#MODEL C
table(heart_data_test$target,PredictionC>0.7)
# accuracy is ((48+55) / (48+55+11+7)) = 0.85123966942


## Check ROC Curve

library(ROCR)

#MODEL A
ROCRpredA <- prediction(PredictionA,heart_data_test$target)
ROCRperfA <- performance(ROCRpredA,'tpr','fpr')
plot(ROCRperfA,colorize=TRUE, main = "Model A ROC")

## Check Accuracy for area under the curve
aucA <- performance(ROCRpredA,measure = "auc")
aucA
aucA <- aucA@y.values[[1]]
aucA

## Gives us the accuracy of 0.8870523


#MODEL B
ROCRpredB <- prediction(PredictionB,heart_data_test$target)
ROCRperfB <- performance(ROCRpredB,'tpr','fpr')
plot(ROCRperfB,colorize=TRUE, main = "Model B ROC")

## Check Accuracy for area under the curve
aucB <- performance(ROCRpredB,measure = "auc")
aucB
aucB <- aucB@y.values[[1]]
aucB

## Gives us the accuracy of 0.8586777


#MODEL C
ROCRpredC <- prediction(PredictionC,heart_data_test$target)
ROCRperfC <- performance(ROCRpredC,'tpr','fpr')
plot(ROCRperfC,colorize=TRUE, main = "Model C ROC")

## Check Accuracy for area under the curve
aucC <- performance(ROCRpredC,measure = "auc")
aucC
aucC <- aucC@y.values[[1]]
aucC

## Gives us the accuracy of 0.9298898

#Therefore, model C is the best model since it gives us most accurate predictions
#------------------------------------------------------------------------------------------#