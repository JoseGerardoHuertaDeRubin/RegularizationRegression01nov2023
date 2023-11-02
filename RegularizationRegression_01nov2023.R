#DATASET HAS BEEN SPLIT
library(readxl)
############################################################################################
# Split the dataset into training and testing sets (now is 80-20)
#New CUSTOMER CHURN DATASET
customer_churn_dataset<-read_excel("C:/Users/pacor/OneDrive/Escritorio/numeric.xlsx")
set.seed(123)
library(caret)
index<-createDataPartition(customer_churn_dataset$Churn,p=0.8,list=FALSE,times=1)
########################################################################################
# Create the training and testing sets
train_set <- customer_churn_dataset[index, ]
test_set <- customer_churn_dataset[-index, ]

#DATASET HAS BEEN SPLITTED
x.train<-train_set[,1:20]
y.train<-as.factor( train_set$Churn)
y.test<-as.factor(test_set$Churn)
# Let's check class imbalance
{# Convert the factor variable to a numeric variable
  churn_numeric <- as.numeric(train_set$Churn) - 1 #Because it uses 1 and 2, instead of 0 and 1
  # Calculate the percentage of customers who churned
  churn_rate <- mean(churn_numeric)
  
  # Check if the dataset is imbalanced
  if (churn_rate > 0.2) {
    print("The dataset is imbalanced.")
  } else {
    print("The dataset is not imbalanced.")
  }
  
  # MITIGATE CLASS IMBALANCE
}
# PROVE REGULARIZATION REGRESSION
##EEK FOLD CROSS VALIDATION
ctrspec<-trainControl(method = "cv",number=10,
                      savePredictions = "all",
                      sampling = "rose")
#VECTOR OF POTENTIAL LAMBDA VALUES
lmbda_vector<-10^seq(5,-5,length=100)
library(glmnet)
install.packages("caret")
library(caret)
# Fit lasso regression:
mlasso <- train(as.factor(Churn)~.,
                data=train_set,
                method = "glmnet", 
                tuneGrid = expand.grid(alpha = 1,lambda = lmbda_vector),
                metric="Accuracy",
                trControl = ctrspec,na.action=na.omit) 
melastic<-train(as.factor(Churn)~ .,
                data = train_set,
                method = "glmnet", 
                tuneGrid = expand.grid(alpha = seq(0, 1, 0.1),lambda = lmbda_vector),
                metric="Accuracy",
                trControl = ctrspec,
                na.action=na.omit) 
mridge<-train(as.factor(Churn)~ .,
              data = train_set,
              method = "glmnet", 
              tuneGrid = expand.grid(alpha = 0,lambda = lmbda_vector),
              metric="Accuracy",
              trControl = ctrspec,
              na.action=na.omit) 

# Print the results:
mlasso;melastic;mridge
#bEST MODEL (OPTIMAL) TUNING PARAMETER (ALPHA, LAMBDA)
mlasso$bestTune
melastic$bestTune
mridge$bestTune

#LASSO REGRESSIN  MODEL COEFICIENTS (PARAMETER ESTIMATES)
round(coef(mlasso$finalModel,mlasso$bestTune$lambda),4)
round(coef(melastic$finalModel,melastic$bestTune$lambda),4)
round(coef(mridge$finalModel,mridge$bestTune$lambda),4)

#PLOT RESULTS 
plot(log(mlasso$results$lambda),mlasso$results$Accuracy,
     xlab="LOG Lambda",ylab="Accuracy")
plot(log(melastic$results$lambda),melastic$results$Accuracy,
     xlab="LOG Lambda",ylab="Accuracy")
plot(log(mridge$results$lambda),mridge$results$Accuracy,
     xlab="LOG Lambda",ylab="Accuracy")
#VARIABLE IMPORTANCE 
varImp(mlasso)

library(ggplot2)

####MODEL PREDICITON
pr_lasso<-predict(mlasso,newdata = test_set)
pr_elastic<-predict(melastic,newdata = test_set)
pr_ridge<-predict(mridge,newdata = test_set)
length(pr_lasso)
length(test_set$Churn)
RMSE()
#MODEL PERFORMACER
install.packages("MLmetrics")
library(MLmetrics)
ac1<-accuracy.meas(y.test,pr_lasso)
m1per<-data.frame(RMSE=RMSE(as.numeric(pr_lasso),as.numeric(y.test)),
                  RSQUARED=R2(as.numeric(pr_lasso),as.numeric(y.test)),
                  ACCURACY=mean(pr_lasso == y.test),
                  PRECISION=precision(pr_lasso, y.test),
                  F1SCORE=F1_Score(pr_lasso,y.test))
m2per<-data.frame(RMSE=RMSE(as.numeric(pr_elastic),as.numeric(y.test)),
                  RSQUARED=R2(as.numeric(pr_elastic),as.numeric(y.test)),
                  ACCURACY=mean(pr_elastic == y.test),
                  PRECISION=precision(pr_elastic, y.test),
                  F1SCORE=F1_Score(pr_elastic,y.test))
m3per<-data.frame(RMSE=RMSE(as.numeric(pr_ridge),as.numeric(y.test)),
                  RSQUARED=R2(as.numeric(pr_ridge),as.numeric(y.test)),
                  ACCURACY=mean(pr_ridge == y.test),
                  PRECISION=precision(pr_ridge, y.test),
                  F1SCORE=F1_Score(pr_ridge,y.test))
ggplot(varImp(mlasso))
ggplot(varImp(melastic))
ggplot(varImp(mridge))
ggplot(m2, metric = "RMSE")
ggplot(m2, metric = "MAE")


coef(m2$finalModel, m2$finalModel$lambdaOpt)





m2 <- train(mpg ~ .,
            data = mtcars,
            method = "glmnet", 
            metric =  "RMSE",
            trControl = tc) 
m2



#LASSO
tc <- trainControl(method = "LOOCV")
m3 <- train(mpg ~ .,
            data = mtcars,
            method = "glmnet", 
            tuneGrid = expand.grid(alpha = 1,
                                   lambda = seq(0, 10, 0.1)),
            metric = "MAE",
            trControl = tc) 

# Plot the results:
ggplot(m3, metric = "RMSE")
ggplot(m3, metric = "MAE")

# Results for the best model:
m3$results[which(m3$results$lambda == m3$finalModel$lambdaOpt),]

# Coefficients for the best model:
coef(m3$finalModel, m3$finalModel$lambdaOpt)




#Elastic Net
tc <- trainControl(method = "LOOCV")
m4 <- train(mpg ~ .,
            data = mtcars,
            method = "glmnet", 
            tuneGrid = expand.grid(alpha = seq(0, 1, 0.1),
                                   lambda = seq(0, 10, 0.1)),
            metric = "RMSE",
            trControl = tc) 

# Print best choices of alpha and lambda:
m4$bestTune

# Print the RMSE and MAE for the best model:
m4$results[which(rownames(m4$results) == rownames(m4$bestTune)),]

# Print the coefficients of the best model:
coef(m4$finalModel, m4$bestTune$lambda, m4$bestTune$alpha)




# Choose model using oneSE:
tc <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 100,
                   selectionFunction = "oneSE")
#tolerance, which chooses the simplest model that has a performance within (by default) 1.5 % of the model with the best performance.

m3 <- train(mpg ~ .,
            data = mtcars,
            method = "glmnet", 
            tuneGrid = expand.grid(alpha = 1,
                                   lambda = seq(0, 10, 0.1)),
            trControl = tc) 

# Print the "best" model (according to the oneSE rule):
m3$bestTune
coef(m3$finalModel, m3$finalModel$lambdaOpt)