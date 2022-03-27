library(rpart)

#parse argument##
args <- commandArgs(trailingOnly = T)
if(length(args)==0){
  stop('USAGE: Rscript hw3_108304010.R --fold k --input Archaeal_tfpssm.csv --output performance.csv', call. = F)
}

##fold number##
k <- as.numeric(args[2])
##read file##
input <- read.csv(args[4],header = F)
input1 <- input[,-1]
input1$V2 <- as.factor(input1$V2)
input1 <- input1[sample(nrow(input1)),]

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
  model <- rpart(V2~.,data = data_training,control = rpart.control(maxdepth = 4),method = 'class')
  
  ##confusion matrix##
  resultframe_t <- data.frame(truth=data_training$V2,prediction=predict(model,type='class'))
  resultframe_v <- data.frame(truth=data_validation$V2,prediction=predict(model,newdata=data_validation,type='class'))
  table_train <- table(resultframe_t)
  table_v <- table(resultframe_v)
  ##first column##
  output[i+1,1] <- paste('fold',i,sep = '')
  ##second&third column##
  output[i+1,2] <- round((table_train[1,1]+table_train[2,2]+table_train[3,3]+table_train[4,4])/nrow(data_training),2)
  output[i+1,3] <- round((table_v[1,1]+table_v[2,2]+table_v[3,3]+table_v[4,4])/nrow(data_validation),2)
  ####training data(2nd time)##
  final_training <- rbind(data_training,data_validation)
  model2 <- rpart(V2~.,data = final_training,control = rpart.control(maxdepth = 4),method = 'class')
  
  ##confusion matrix##
  resultframe_test <- data.frame(truth=data_testing$V2,prediction=predict(model2,newdata=data_testing,type='class'))
  table_test <- table(resultframe_test)
  ##last column##
  output[i+1,4] <- round((table_test[1,1]+table_test[2,2]+table_test[3,3]+table_test[4,4])/nrow(data_testing),2)
}
##last row##
output[k+2,] <- c('ave.',round(mean(as.numeric(output[2:k+1,2])),2),round(mean(as.numeric(output[2:k+1,3])),2),round(mean(as.numeric(output[2:k+1,4])),2))
print(output)

##export file##
write.table(output,file = args[6],sep = ',',col.names = F,row.names = F,quote = F)
