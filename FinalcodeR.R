#libraries needed
library(caret)
library(class)
library(corrplot)
library(dplyr)
library(e1071)
library(FNN)
library(gmodels)
library(psych)
library(ggvis)
library(boot)
library(olsrr)
library(MASS)
library(leaps)
data<-read.csv("C:/Users/milin/OneDrive - UMBC/777/project/D3/covid.csv",header = TRUE) #read the file
data
head(data)
#We will will be removing the columns which will not be used in our analysis. id,entry_date , date_symptoms ,contact_other_covid will be removed.
df<-data
head(df)
df$entry_date=NULL
df$date_symptoms=NULL
df$id=NULL
df$contact_other_covid=NULL

df$sex[df$sex == 2] <- 0 #Replacing the value of 2 as male to 0 as male. Now, 0-male and 1-female
head(df)
df$patient_type[df$patient_type==2]<-0 ##Replacing the value of 2 as hospitalized to 0 as hospitalized. Now, 0-hospitalized and 1-not hospitalized
colnames(df)[3]<-"died" #Changed the column name from date_died to died

#In the data, missing values or not specified values are mentioned as 97,98,99. So i am replacing all the missing values as NULL
df$intubed[df$intubed %in% c(97,98,99)]<-NA
df$pneumonia[df$pneumonia %in% c(97,98,99)]<-NA
df$pregnancy[df$pregnancy %in% c(97,98,99)]<-NA
df$diabetes[df$diabetes %in% c(97,98,99)]<-NA
df$copd[df$copd %in% c(97,98,99)]<-NA
df$asthma[df$asthma %in% c(97,98,99)]<-NA
df$inmsupr[df$inmsupr %in% c(97,98,99)]<-NA
df$hypertension[df$hypertension %in% c(97,98,99)]<-NA
df$other_disease[df$other_disease %in% c(97,98,99)]<-NA
df$cardiovascular[df$cardiovascular %in% c(97,98,99)]<-NA
df$obesity[df$obesity %in% c(97,98,99)]<-NA
df$renal_chronic[df$renal_chronic %in% c(97,98,99)]<-NA
df$tobacco[df$tobacco %in% c(97,98,99)]<-NA
df$covid_res[df$covid_res %in% c(97,98,99)]<-NA
df$icu[df$icu %in% c(97,98,99)]<-NA
df$age[df$age %in% c(0)]<-NA
df$covid_res[df$covid_res==3]<- NA # ignore all awating data


df$died=NULL # Removed died column as this is not required for our analysis
head(df) 
summary(df)


#From summary We can observe that 80% of the null values or Not specified values are in ICU Pregnancy Columns and intubed So we will no be using these for any analysis.
df$pregnancy=NULL
df$icu=NULL
df$intubed=NULL
head(df)
summary(df)
df=na.omit(df) # removed all the NA values from dataframe
dim(data)      #original data had 566602 rows and 23 columns  
dim(df)    # Data After clening has 562647 rows and 16 columns
head(df)
summary(df)
#Repaced the cateorical values into Binary Values
df$pneumonia[df$pneumonia==2]<-0 #0-NO 1-Yes
df$diabetes[df$diabetes==2]<-0   #0-No 1-Yes
df$copd[df$copd==2]<-0      #0-No 1-Yes
df$asthma[df$asthma==2]<-0   #0-No 1-Yes
df$inmsupr[df$inmsupr==2]<-0   #0-No 1-Yes
df$hypertension[df$hypertension==2]<-0   #0-No 1-Yes
df$other_disease[df$other_disease==2]<-0   #0-No 1-Yes
df$cardiovascular[df$cardiovascular==2]<-0   #0-No 1-Yes
df$obesity[df$obesity==2]<-0   #0-No 1-Yes
df$diabetes[df$diabetes==2]<-0   #0-No 1-Yes
df$renal_chronic[df$renal_chronic==2]<-0   #0-No 1-Yes
df$tobacco[df$tobacco==2]<-0   #0-No 1-Yes
df$diabetes[df$diabetes==2]<-0   #0-No 1-Yes
#df$covid_res : 0 - Positive, 1 - Negative
df$covid_res[df$covid_res==1]<- 0
df$covid_res[df$covid_res==2]<- 1

head(df)
summary(df)
str(df)
#data normalization in the scale of -1 to 1
data_norm <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
datn<-as.data.frame(lapply(df[,1:15],data_norm))
df<-datn
Hmisc::describe(df)
# changing continous fields to factor
df[c(15)] <- lapply(df[c(15)],factor)

df[c(1:3,5:14)] <- lapply(df[c(1:3,5:14)],factor)
# histogram 
barplot(table(df$sex),main='0-Male 1- Female',xlab="Sex Count",
        col="red")
barplot(table(df$patient),las=1,main='0-Hospitalized 1.Not Hospitalized',xlab="Patient",col="red")
hist(df$age,main='Age',xlab="Age",
     col="red")
barplot(table(df$pneumonia),main='0-No 1.Yes',xlab="Pneumonia",
        col="red")
barplot(table(df$diabetes),main='0-No 1-Yes',xlab="diabetes",
        col="red")
barplot(table(df$copd),main='0-No 1-Yes',xlab="Copd",
        col="red")

barplot(table(df$inmsupr),main='0-No 1-Yes',xlab="Inmsupr",
        col="red")
barplot(table(df$hypertension),main='0-No 1-Yes',xlab="Hypertension",
        col="red")
barplot(table(df$cardiovascular),main='0-No 1-Yes',xlab="Cardio",
        col="red")
barplot(table(df$obesity),main='0-No 1-Yes',xlab="Obesity",
        col="red")
barplot(table(df$renal_chronic),main='0-No 1-Yes',xlab="RenalChronic",
        col="red")
barplot(table(df$other_disease),main='0-No 1-Yes',xlab="OtherDisease",
        col="red")
barplot(table(df$tobacco ),main='0-No 1-Yes',xlab="tobacco",
        col="red")
barplot(table(df$covid_res),main='0-Positive 1-Neagative',xlab="Covid",
        col="red")
head(df)
summary(df)
head(df)
library(Hmisc)
co <- rcorr(as.matrix(df))
co$r  #correlation Matrix
library(corrplot) 
corrplot(co$r, method = "square")#correlation Plot
corrplot(co$r,order='AOE',method='color',addCoef.col='blue') #correlation plot with coefficient values
library('ggplot2')
df1<-df
df<-datn 
# set the seed to make the partition reproducible
set.seed(1234)
head(df)
# view correlation between features
corrMat <- cor(df)
correlated <- findCorrelation(corrMat, cutoff = 0.01)
data_y <- df %>% select(covid_res) # storing response variable covid_res in data_y
data_x <- df %>% select(-covid_res) # storing all the predictors varibale in data_x
# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_x))
train_ind <- sample(seq_len(nrow(data_x)), size = smp_size)
# creating test and training sets that contain all of the predictors
train.X <- data_x[train_ind, ]
train.Y <- data_y[train_ind, ]

test.X <- data_x[-train_ind, ]
test.Y <- data_y[-train_ind, ]
#KNN---------------------------------------
# sqrt (370724) =~ 608/609 : we have 370724 training examples
pred_knn.608 <- knn(train = train.X, test = test.X, cl = train.Y, k=10)
pred_knn.609 <- knn(train = train.X, test = test.X, cl = train.Y, k=11)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

tab.608 <- table(pred_knn.608,test.Y)
accuracy(tab.608)
confusionMatrix(tab.608)

tab.609 <- table(pred_knn.609,test.Y)
accuracy(tab.609)
confusionMatrix(tab.609)

# Find optimal value of K and plot the results
i=1
k.optm=1
while (i <= 11){
  knn.mod <- knn(train = train.X, test = test.X, cl = train.Y, k=i)
  knn.res <- table(knn.mod,test.Y)
  k.optm[i] <- accuracy(knn.res)
  cat('k[', i, ']', '=', k.optm[i],'\n')
  i = i+10
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")


#Logistic Regression-----------------------

ran <- sample(1:nrow(df), 0.5 * nrow(df))

train1 <- df[c(1:15)][ran,]
test1 <- df[c(1:15)][-ran,]

glm.fits=glm(covid_res ~ ., data = train1 ,family = binomial )
summary (glm.fits)


fitted.results <- predict(glm.fits, newdata=subset(train1,select=c(1:14)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
ClasificError <- mean(fitted.results == train1$covid_res)
print(paste('Accuracy', ClasificError))

fitted.results <- predict(glm.fits, newdata=subset(test1,select=c(1:14)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
ClasificError <- mean(fitted.results == test1$covid_res)
print(paste('Accuracy', ClasificError))
anova(glm.fits , test = "Chisq")


#----------------------------------------------------------------------------------

#---------logistic regression with -whole dataset approach  ----------------------------


i=1
acc= list()
while (i <= 10){
  lr.mod <- glm(covid_res~ poly(age, i)+sex+patient_type+pneumonia+diabetes+
                  copd+asthma+inmsupr+hypertension+other_disease+cardiovascular+obesity+renal_chronic+tobacco ,
                data = df ,family =binomial )
  print(summary(lr.mod))
  fitted.results <- predict(lr.mod, newdata=subset(df,select=c(1:14)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  ClasificError <- mean(fitted.results == df$covid_res)
  print(ClasificError)
  acc[[i]] <- ClasificError
  i = i+1
}
acc
plot(unlist(acc))                
#---------logistic regression with - validation set approach ----------------------------

ran <- sample(1:nrow(df), 0.5 * nrow(df))

train1 <- df[c(1:15)][ran,]
test1 <- df[c(1:15)][-ran,]


i=1
acc= list()
while (i <= 10){
  lr.mod <- glm(covid_res~ poly(age, i)+sex+patient_type+pneumonia+diabetes+
                  copd+asthma+inmsupr+hypertension+other_disease+cardiovascular+obesity+renal_chronic+tobacco ,
                data = train1 ,family =binomial )
  print(summary(lr.mod))
  fitted.results <- predict(lr.mod, newdata=subset(test1,select=c(1:14)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  ClasificError <- mean(fitted.results == test1$covid_res)
  print(ClasificError)
  acc[[i]] <- ClasificError
  i = i+1
}
acc
plot(unlist(acc)) 




#---------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------
#---------logistic regression with - k- fold----------------------------


i=1
acc= list()
while (i <= 10){
  lr.mod <-glm(covid_res~ poly(age, i)+sex+patient_type+pneumonia+diabetes+
                 copd+asthma+inmsupr+hypertension+other_disease+cardiovascular+obesity+renal_chronic+tobacco ,
               data = df ,family =binomial )
  cv.err =cv.glm(df ,lr.mod , K = 10 )
  print(cv.err$delta[1])
  acc[[i]] <- 1- cv.err$delta[1]
  i = i+1
}

acc
plot(unlist(acc))
#---- stepwise subset selection --------------------------------------------------------------------
full.model <-glm(covid_res~ ., data = df ,family =binomial )

sub.models <- regsubsets(covid_res~ ., data = df , nvmax = 15)
summary(sub.models)

#forward selection code----------------------------------------------------------------------------------------------------

full.model <-glm(covid_res~ ., data = df ,family =binomial )

coef(full.model)

forward.mod <- stepAIC(full.model, direction = "forward", 
                       trace = FALSE)
coef(forward.mod)

cv.err =cv.glm(df ,forward.mod , K = 10)
print(cv.err$delta[1])
print(cv.err$delta[2])

fitted.results <- predict(forward.mod, newdata=subset(df,select=c(1:14)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
ClasificError <- mean(fitted.results == df$covid_res)
print(paste('Accuracy', ClasificError))


#--backward inversion code---------------------------------

full.model <-glm(covid_res~ ., data = df ,family =binomial )

coef(full.model)

backward.mod <- stepAIC(full.model, direction = "backward", 
                        trace = FALSE)
coef(backward.mod)

cv.err =cv.glm(df ,backward.mod , K = 10)
print(cv.err$delta[1])
print(cv.err$delta[2])

fitted.results <- predict(backward.mod, newdata=subset(df,select=c(1:14)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
ClasificError <- mean(fitted.results == df$covid_res)
print(paste('Accuracy', ClasificError))

#----------------------ridge--------------------------------


#load the library
library(glmnet)
library(ISLR)
library(dplyr)

#spliting the data into train and test
full.model <-glm(covid_res~ ., data = df ,family =binomial )
summary(full.model )


#model.matrix creates a  model matrix, by expanding factors to a set of dummy variables.

df.x = model.matrix(covid_res~ .,df )[,-1]
df.y = df$covid_res




rigde_mod = glmnet(df.x, df.y, nlambda = 50, alpha = 0 , family = binomial)

plot(rigde_mod, xvar = "lambda")

#to find out optimal lambda
#Cross-Validation For Glmnet Does k-fold cross-validation for glmnet, produces a plot, and returns a value for lambda, I have not defined nfold as default is 10

cv_rigde_mod <- cv.glmnet(df.x, df.y, nlambda = 50, alpha = 0 , family = binomial)
best_lambda <- cv_rigde_mod$lambda.min
print(best_lambda)
plot(cv_rigde_mod)

#---------------------------------creating new model with optimal lambda value----------


ridge_model2 = glmnet(df.x, df.y, nlambda = 1, alpha = 0 , family = binomial, lambda = best_lambda)


fitted.results <- predict(ridge_model2, newx = df.x)

fitted.results <- ifelse(fitted.results > 0,1,0)
misClasificError <- mean(fitted.results != df.y)
print(paste('Accuracy',1-misClasificError))

library(glmnet)
library(shape)

#--------------------------------lasso-------------------

full.model <-glm(covid_res~ ., data = df ,family =binomial )
summary(full.model )



df.x = model.matrix(covid_res~ .,df )[,-1]
df.y = df$covid_res




lasso_mod = glmnet(df.x, df.y, nlambda = 50, alpha = 1 , family = binomial)

plot(lasso_mod, xvar = "lambda")

#to find out best lambda
#Cross-Validation For Glmnet Does k-fold cross-validation for glmnet, produces a plot, and returns a value for lambda, I have not defined nfold as default is 10

cv_lasso_mod <- cv.glmnet(df.x, df.y, nlambda = 50, alpha = 1 , family = binomial)
best_lambda <- cv_lasso_mod$lambda.min
print(best_lambda)
plot(cv_lasso_mod)


#---------------------------------creating new model with best lambda value----------


lasso_model2 = glmnet(df.x, df.y,  alpha = 1 , family = binomial, lambda = best_lambda)


fitted.results <- predict(lasso_model2, newx = df.x)

fitted.results <- ifelse(fitted.results > 0,1,0)
misClasificError <- mean(fitted.results != df.y)
print(paste('Accuracy',1-misClasificError))

#------------GAM
#M1 <- lm(covid_res~ age + sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco, data = df)
#summary(M1)
#anova(M1)

#M2 <- lm(covid_res~ age + sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco+renal_chronic+cardiovascular+obesity, data = df)
#anova(M1,M2)

#-----------GAM
library(mgcv)
#mgcv provides functions for generalized additive modelling
AM1 <- gam(covid_res~ s(age) + sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco+renal_chronic+cardiovascular+obesity, data = df)
anova(AM1)
#started with a model that contains all explanatory variables
AM2 <- gam(covid_res~ s(age,bs="cs") + sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco+renal_chronic+cardiovascular+obesity, data = df)
anova(AM2)
#The new bit is the bs = "cs" part. It tells R to use the cubic regression spline with shrinkage.
AM3<-gam(covid_res~ s(age,bs="cs")+sex,data=df)
AM3
plot(AM3)
#Smoothing the predictor Age

E.AM3 <- resid(AM3)
Fit.AM3 <- fitted(AM3)
plot(x = Fit.AM3, y = E.AM3, xlab = "Fitted values",
       ylab = "Residuals")

M3 <- lm(covid_res~age+sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco+renal_chronic+cardiovascular+obesity, data = df)
AM3<-gam(covid_res~ s(age,bs="cs") + sex+patient_type+ diabetes+copd+asthma+hypertension+other_disease+tobacco+renal_chronic+cardiovascular+obesity, data = df)
anova(M3, AM3, test ="F")
#Comparision between GAM and Linear regression indicates both the model are same. The GAM shows no residual patten ,so we prefer GAM. 