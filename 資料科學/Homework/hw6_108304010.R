##hw6##

##parse argument##
args <- commandArgs(trailingOnly = T)

if (length(args)==0){
  stop('Usage:Rscript hw6_108304010.R --fold n --train Data/training.csv --test Data/test.csv --report performance.csv --predict predict.csv')
}

##fold number##
k <- as.numeric(args[2])
#k <- 5
##Read in data##
doc <- read.csv(args[4],header = T)
#doc <- read.csv('training.csv',header=T)
doc <- doc[-1]


##preprocessing##
##fill in NA values##
doc$MonthlyIncome <- ifelse(is.na(doc$MonthlyIncome)==T,mean(doc$MonthlyIncome,na.rm = T),doc$MonthlyIncome)
doc$NumberOfDependents <- ifelse(is.na(doc$NumberOfDependents)==T,0,doc$NumberOfDependents)

##adjusting entries##
doc$SeriousDlqin2yrs <- as.factor(doc$SeriousDlqin2yrs)
doc$RevolvingUtilizationOfUnsecuredLines <- as.factor(ifelse(doc$RevolvingUtilizationOfUnsecuredLines<0.5, 0, 1))
doc[doc$NumberOfTime30.59DaysPastDueNotWorse>1,]$NumberOfTime30.59DaysPastDueNotWorse <- 2 
doc$NumberOfTime30.59DaysPastDueNotWorse <- as.factor(doc$NumberOfTime30.59DaysPastDueNotWorse)
doc[doc$NumberOfTime60.89DaysPastDueNotWorse>1,]$NumberOfTime60.89DaysPastDueNotWorse <- 2
doc$NumberOfTime60.89DaysPastDueNotWorse <- as.factor(doc$NumberOfTime60.89DaysPastDueNotWorse)
doc[doc$NumberOfTimes90DaysLate>1,]$NumberOfTimes90DaysLate <- 2
doc$NumberOfTimes90DaysLate <- as.factor(doc$NumberOfTimes90DaysLate)
doc$NumberOfDependents <- as.factor(ifelse(doc$NumberOfDependents==0, 0, 1))

##function to split data##
datasplit <- function(n,k){
  splitpoint <- c(0)
  for (i in 1:k) {
    if (i==k){
      splitpoint=append(splitpoint,n)
    } else {
      splitpoint=append(splitpoint,i*(n%/%k))
    }
  }
  splitpoint <- append(splitpoint,0) 
  return(splitpoint)
}
splitpoint <- datasplit(nrow(doc),k)


##output table##
output <- matrix(0,k+2,4)
output[1,] <- c('set','training','validation','test')

##split data##
library(rpart)
library(ROCR)
for (i in 1:k) {
  if (i==1){
    data_testing <- doc[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- doc[((splitpoint[i+1]+1):splitpoint[3]),] 
    data_training <- doc[-(1:splitpoint[3]),]
  } else if (i==k) {
    data_testing <- doc[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- doc[(1:splitpoint[2]),] 
    data_training <- doc[((splitpoint[2]+1):splitpoint[i]),]
    } else {
      data_testing <- doc[((splitpoint[i]+1):splitpoint[i+1]),]
      data_validation <- doc[((splitpoint[i+1]+1):splitpoint[i+2]),] 
      data_training <- doc[-((splitpoint[i]+1):splitpoint[i+2]),]
    }
  ##training data(1st time)##
  #model <- rpart(SeriousDlqin2yrs~.,data = data_training,method = 'class')
  model <- glm(SeriousDlqin2yrs~.,data = data_training,family = 'binomial')
  ##first column##
  output[i+1,1] <- paste('fold',i,sep = '')
  
  ##area under curve(2nd column)##
  #pred_t <- predict(model,type='prob')
  pred_t <- predict(model,type = 'response')
  train_auc <- prediction(pred_t,data_training$SeriousDlqin2yrs)
  auc1 <- performance(train_auc,"auc")@y.values
  output[i+1,2] <- round(as.numeric(auc1),2)
  
  ##area under curve(3rd column)##
  #pred_v <- predict(model,newdata = data_validation,type = 'prob')
  pred_v <- predict(model,newdata = data_validation,type = 'response')
  vali_auc <- prediction(pred_v,data_validation$SeriousDlqin2yrs)
  auc2 <- performance(vali_auc,'auc')@y.values
  output[i+1,3] <- round(as.numeric(auc2),2)
  
  ####training data(2nd time)##
  final_training <- rbind(data_training,data_validation)
  #model2 <- rpart(SeriousDlqin2yrs~.,data = final_training,method = 'class')
  model2 <- glm(SeriousDlqin2yrs~.,data = final_training,family = 'binomial')
  ##area under curve(4th column)##
  #pred_test <- predict(model2, newdata=data_testing,type='prob')
  pred_test <- predict(model2,newdata = data_testing,type = 'response')
  test_auc <- prediction(pred_test,data_testing$SeriousDlqin2yrs)
  auc3 <-  performance(test_auc,"auc")@y.values
  output[i+1,4] <- round(as.numeric(auc3),2)
  
}

##last row##
output[k+2,] <- c('ave.',round(mean(as.numeric(output[2:k+1,2])),2),round(mean(as.numeric(output[2:k+1,3])),2),round(mean(as.numeric(output[2:k+1,4])),2))

##export  performance file##
write.table(output,file = args[8],sep = ',',col.names = F,row.names = F,quote = F)

##Testing file##
doc3 <- read.csv(args[6],header = T)
#doc3 <- read.csv('test.csv')

##preprocessing##
doc3 <- doc3[-1]
##fill in NA values##
doc3$MonthlyIncome <- ifelse(is.na(doc3$MonthlyIncome)==T,mean(doc3$MonthlyIncome,na.rm = T),doc3$MonthlyIncome)
doc3$NumberOfDependents <- ifelse(is.na(doc3$NumberOfDependents)==T,0,doc3$NumberOfDependents)

doc3$SeriousDlqin2yrs <- as.factor(doc3$SeriousDlqin2yrs)
doc3$RevolvingUtilizationOfUnsecuredLines <- as.factor(ifelse(doc3$RevolvingUtilizationOfUnsecuredLines<0.5, 0, 1))
doc3[doc3$NumberOfTime30.59DaysPastDueNotWorse>1,]$NumberOfTime30.59DaysPastDueNotWorse <- 2 
doc3$NumberOfTime30.59DaysPastDueNotWorse <- as.factor(doc3$NumberOfTime30.59DaysPastDueNotWorse)
doc3[doc3$NumberOfTime60.89DaysPastDueNotWorse>1,]$NumberOfTime60.89DaysPastDueNotWorse <- 2
doc3$NumberOfTime60.89DaysPastDueNotWorse <- as.factor(doc3$NumberOfTime60.89DaysPastDueNotWorse)
doc3[doc3$NumberOfTimes90DaysLate>1,]$NumberOfTimes90DaysLate <- 2
doc3$NumberOfTimes90DaysLate <- as.factor(doc3$NumberOfTimes90DaysLate)
doc3$NumberOfDependents <- as.factor(ifelse(doc3$NumberOfDependents==0, 0, 1))
##output table2##
output2 <- matrix(0,nrow(doc3)+1,2)
output2[1,] <- c('id','probability')

guessing <- data.frame(prediction=predict(model2,doc3,type='response'))

##fill in prediction values##
for (g in 1:nrow(doc3)) {
  output2[g+1,1] <- g
  output2[g+1,2] <- guessing[g,1]
}

##export prediction file##
write.table(output2,file = args[10],sep = ',' , row.names = F,col.names = F,quote = F)

