#Output variable -> y
#y -> Whether the client has subscribed a term deposit or not 
#Binomial ("yes" or "no")
#Input variables:

# bank client data:

#1 - age (numeric)
#2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student","bluecollar","selfemployed","retired","technician","services") 
#3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
#4 - education (categorical: "unknown","secondary","primary","tertiary")
#5 - default: has credit in default? (binary: "yes","no")
#6 - balance: average yearly balance, in euros (numeric) 
#7 - housing: has housing loan? (binary: "yes","no")
#8 - loan: has personal loan? (binary: "yes","no")

# related with the last contact of the current campaign:

#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
#10 - day: last contact day of the month (numeric)
#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#12 - duration: last contact duration, in seconds (numeric)

# other attributes:

#13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#15 - previous: number of contacts performed before this campaign and for this client (numeric)
#16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

#Output variable (desired target):
  
#17 - y - has the client subscribed a term deposit? (binary: "yes","no")

library(readxl)
bank_data <- read_excel(file.choose())
summary(bank_data)
str(bank_data)

sum(is.na(bank_data)) ## missing values are null

library(moments)

hist(bank_data$age)
boxplot(bank_data$age)

hist(bank_data$day)
boxplot(bank_data$day)

hist(bank_data$campaign)
boxplot(bank_data$campaign)

## converting char into factor

bank_data$job <- factor(bank_data$job)
str(bank_data$job)

bank_data$marital <- factor(bank_data$marital)
bank_data$education <- factor(bank_data$education)
bank_data$housing <- factor(bank_data$housing)
bank_data$loan <- factor(bank_data$loan)
bank_data$contact <- factor(bank_data$contact)
bank_data$month <- factor(bank_data$month)
bank_data$poutcome <- factor(bank_data$poutcome)
bank_data$y <- factor(bank_data$y)

str(bank_data)

## we will build 1st model

bank_model1 <- glm(bank_data$y ~ .,data = bank_data,family = "binomial")
summary(bank_model1)

## here we got Null deviance 32631 which is greater than residual deviance 21562  

## age,job,marrital,defaults,campagin,pdays,previous thses variables are insignificant

## now will check corelation 

library(corpcor)
cor2pcor(cor(bank_data[-c(2,3,4,5,7,8,9,11,16,17)]))         
pairs(bank_data[-c(2,3,4,5,7,8,9,11,16,17)])

## from this pdays and previous has strong relationship 0.452263030

bank_data1 <- bank_data[-c(14,15)]

## we will build our 2nd model

bank_model2 <- glm(bank_data1$y ~. ,data = bank_data1,family = "binomial")
summary(bank_model2)

### from this age,defaults is insignificant so we will remove age from this and will build our new model

bank_data1 <- bank_data[-c(1,4,14,15)]
bank_model3 <- glm(bank_data1$y ~.,data = bank_data1,family = binomial)
summary(bank_model3)

# here null deviance is greater than residual deviance so we can go ahed with this model

prob <- predict(bank_model3,type = "response",bank_data)
prob

## confusion matrix

confusion1 <- table(prob > 0.50,bank_data$y)
confusion1  ## false negative alpha value is 3447

## Accuracy

bank_accuracy1 <- sum(diag(confusion1)/sum(confusion1))
bank_accuracy1 ## 0.902015

## Error in model

bank_error <- 1- bank_accuracy1
bank_error ## 0.097985

prop.table(table(bank_data$y))
##no       yes 
##0.8830152 0.1169848

prop.table(table(prob>0.50))
##FALSE       TRUE 
##0.93751521 0.06248479 

### there is diff in predicted values so lets try some other p value

## COnfusion matrix 2

confusion2 <- table(prob > 0.30,bank_data$y)
confusion2 ## here we got alpha value 2416 this is lesser than previous one

## Accuracy2

bank_accuracy2 <- sum(diag(confusion2)/sum(confusion2))
bank_accuracy2 ## 0.9004667

prop.table(table(bank_data$y))

#no       yes 
#0.8830152 0.1169848 

prop.table(table(prob>0.30))

#FALSE      TRUE 
#0.8903585 0.1096415 

## now we will plot ROCR Curve

library(gplots)
library(ROCR)

bankROC_predict <- prediction(prob,bank_data$y)
bankROC_perform <- performance(bankROC_predict,'tpr','fpr')
str(bankROC_perform)

## plot roc Curve

plot(bankROC_perform,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))

## cutoff will be in between 0.2 and 0.1

## make dataframe for tpr and fpr

bankROCR_cutoff <- data.frame(cut_off =bankROC_perform@alpha.values[[1]],fpr=bankROC_perform@x.values,tpr=bankROC_perform@y.values)
View(bankROCR_cutoff)

colnames(bankROCR_cutoff) <- c('Cut OFF','FPR','TPR')

bankROCR_cutoff <- round(bankROCR_cutoff,2)

library(dplyr)

bankROCR_cutoff <- arrange(bankROCR_cutoff,desc(TPR))

confusion3 <- table(prob >0.11,bank_data$y)
confusion3  ## alpha is 824

accuracy3 <- sum(diag(confusion3)/sum(confusion3))
accuracy3 ## accuracy is 0.8324965

