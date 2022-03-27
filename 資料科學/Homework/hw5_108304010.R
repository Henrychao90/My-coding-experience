##hw5##

#parse argument##
args <- commandArgs(trailingOnly = T)
if(length(args)==0){
  stop('USAGE: Rscript hw5_108304010.R --fold n --train Titanic_Data/training.csv --test Titanic_Data/test.csv --report performance.csv --predict predict.csv', call. = F)
}

##fold number##
k <- as.numeric(args[2])

##read in data##
doc <- read.csv(args[4],header = T)
#doc <- read.csv('train.csv')

##preprocessing##
doc$Age <- ifelse(is.na(doc$Age)==T,mean(doc$Age,na.rm = T),doc$Age)
str(doc)

##selecting variables##
input1 <- doc[,-c(1,4,9,11,12)]
input1$Survived <- as.factor(input1$Survived)
input1$Pclass <- as.factor(input1$Pclass)
input1$Sex <- as.factor(input1$Sex)
str(input1)
##function to split data##
splitpoint <- c(0)
datasplit <- function(n,k){
  for (i in 1:k) {
    if (i==k){
      splitpoint=append(splitpoint,nrow(input1))
    } else {
      splitpoint=append(splitpoint,i*(n%/%k))
    }
  }
  splitpoint <- append(splitpoint,0) 
  return(splitpoint)
}
splitpoint <- datasplit(nrow(input1),k)

##output table##
output <- matrix(0,k+2,4)
output[1,] <- c('set','training','validation','test')

##split data##
library(e1071)
for (i in 1:k) {
  if (i==1){
    data_testing <- input1[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- input1[((splitpoint[i+1]+1):splitpoint[2]),] 
    data_training <- input1[-(1:splitpoint[2]),]
  } else if (i==k) {
    data_testing <- input1[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- input1[(1:splitpoint[2]),] 
    data_training <- input1[((splitpoint[2]+1):splitpoint[i]),]
  } else {
    data_testing <- input1[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- input1[((splitpoint[i+1]+1):splitpoint[i+2]),] 
    data_training <- input1[-((splitpoint[i]+1):splitpoint[i+2]),]
  }
  ##training data(1st time)##
  model <- svm(Survived~., data=data_training)
  
  ##confusion matrix##
  resultframe_t <- data.frame(truth=data_training$Survived,prediction=predict(model,type='class'))
  resultframe_v <- data.frame(truth=data_validation$Survived,prediction=predict(model,newdata=data_validation,type='class'))
  table_train <- table(resultframe_t)
  table_v <- table(resultframe_v)
  ##first column##
  output[i+1,1] <- paste('fold',i,sep = '')
  ##second&third column##
  output[i+1,2] <- round((table_train[1,1]+table_train[2,2])/nrow(data_training),2)
  output[i+1,3] <- round((table_v[1,1]+table_v[2,2])/nrow(data_validation),2)
  ####training data(2nd time)##
  final_training <- rbind(data_training,data_validation)
  model2 <- svm(Survived~., data=final_training)
  
  ##confusion matrix##
  resultframe_test <- data.frame(truth=data_testing$Survived,prediction=predict(model2,newdata=data_testing,type='class'))
  table_test <- table(resultframe_test)
  ##last column##
  output[i+1,4] <- round((table_test[1,1]+table_test[2,2])/nrow(data_testing),2)
}

##last row##
output[k+2,] <- c('ave.',round(mean(as.numeric(output[2:k+1,2])),2),round(mean(as.numeric(output[2:k+1,3])),2),round(mean(as.numeric(output[2:k+1,4])),2))
print(output)

##export  performance file##
write.table(output,file = args[8],sep = ',',col.names = F,row.names = F,quote = F)


##Testing file##
doc2 <- read.csv(args[6],header = T)
#doc2 <- read.csv('test.csv')

##preprocessing##
doc2$Age <- ifelse(is.na(doc2$Age)==T,mean(doc2$Age,na.rm = T),doc2$Age)
doc2$Fare <- ifelse(is.na(doc2$Fare)==T,mean(doc2$Fare,na.rm = T),doc2$Fare)

##selecting variables##
input2 <- doc2[,-c(1,3,8,10,11)]
input2$Pclass <- as.factor(input2$Pclass)
input2$Sex <- as.factor(input2$Sex)

##output table2##
output2 <- matrix(0,nrow(doc2)+1,2)
output2[1,] <- c('PassengerId','Survived')

guessing <- data.frame(prediction=predict(model2,input2,type='class'))

##fill in prediction values##
for (g in 1:nrow(doc2)) {
  output2[g+1,1] <- doc2[g,1]
  output2[g+1,2] <- guessing[g,1]
  output2[g+1,2] <- as.numeric(output2[g+1,2])-1
}

##export prediction file##
write.table(output2,file = args[10],sep = ',' , row.names = F,col.names = F,quote = F)



