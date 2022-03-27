##final project##
##parse argument##
args <- commandArgs(trailingOnly = T)

if (length(args)==0){
  stop('Usage:Rscript final_project.R  --train ourdata.csv  --report performance.csv')
}

##fold number##
k <- 5
##Read in data##
doc <- read.csv(args[2])
#doc <- read.csv('ourdata.csv')

##change class##
doc$date..t. <- as.Date(doc$date..t.)
doc$S_P_500_Close..t.1. <- gsub(',','',doc$S_P_500_Close..t.1.)
doc$S_P_500_Close..t.1. <- as.numeric(doc$S_P_500_Close..t.1.)
doc$total_net_tsmc..t.1. <- as.numeric(doc$total_net_tsmc..t.1.)
doc$TAIEX..t. <- as.factor(doc$TAIEX..t.)
doc$Bitcoin_Change...t.1. <- round(doc$Bitcoin_Change...t.1.,2)

##creating columns##

##function to calculate difference##
fluctuate <- function(n){
  diff=c()
  for (i in 1:length(n)-1) {
    diff[i]=round((n[i+1]-n[i])*100/n[i],2) 
  }
  return(diff)
}

##difference in Dow_Jones##
contain1 <- c(0.03)
contain1 <- append(contain1,fluctuate(doc$Dow.Jones_Close..t.1.))
doc$Dow.Jones_diff <- contain1

##difference in NASDAQ##
contain2 <- c(0.17)
contain2 <- append(contain2,fluctuate(doc$NASDAQ_Close..t.1.))
doc$NASDAQ_diff <- contain2

##difference in S&P 500##
contain3 <- c(0.33)
contain3 <- append(contain3,fluctuate(doc$S_P_500_Close..t.1.))
doc$S_P_500_diff <- contain3

##difference in SOX##
contain4 <- c(0.15)
contain4 <- append(contain4,fluctuate(doc$SOX_Close..t.1.))
doc$SOX_diff <- contain4

##Selecting variables##
doc2 <- doc[c(1,5,7,9,10,11,12,8)]

##Selecting last 100 rows##
boss <- doc2[1554:1653,]

##select modeling data##
doc2 <- doc2[1:1553,]

##Shuffling the order##
doc2 <- doc2[sample(nrow(doc2)),]

##function to split data##
splitpoint <- c(0)
datasplit <- function(n,k){
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


##k fold cross-validation##
splitpoint <- datasplit(nrow(doc2),k)

##output table##
output <- matrix(0,2*k,16)
output[k+3,] <- ''

##column names##
output[1,1:4] <- c('Accuracy','training','validation','test')
output[k+4,1:3] <- c('Accuracy','fulltrain','finaltest')
output[1,5:8] <- c('Precision','training','validation','test')
output[k+4,5:7] <- c('Precision','fulltrain','finaltest')
output[1,9:12] <- c('Recall','training','validation','test')
output[k+4,9:11] <- c('Recall','fulltrain','finaltest')
output[1,13:16] <- c('Auc','training','validation','test')
output[k+4,13:15] <- c('Auc','fulltrain','finaltest')
output[k+4,c(4,8,12,16)] <- ''
##split data##
library(rpart)
library(ROCR)
for (i in 1:k) {
  if (i==1){
    data_testing <- doc2[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- doc2[((splitpoint[i+1]+1):splitpoint[3]),] 
    data_training <- doc2[-(1:splitpoint[3]),]
  } else if (i==k) {
    data_testing <- doc2[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- doc2[(1:splitpoint[2]),] 
    data_training <- doc2[((splitpoint[2]+1):splitpoint[i]),]
  } else {
    data_testing <- doc2[((splitpoint[i]+1):splitpoint[i+1]),]
    data_validation <- doc2[((splitpoint[i+1]+1):splitpoint[i+2]),] 
    data_training <- doc2[-((splitpoint[i]+1):splitpoint[i+2]),]
  }
  
  ##training data(1st time)##
  model <- rpart(TAIEX..t.~.-date..t. ,data = data_training,control = rpart.control(maxdepth = 4),method = 'class')
  
  ##confusion matrix##
  resultframe_t <- data.frame(truth=data_training$TAIEX..t.,prediction=predict(model,type='class'))
  resultframe_v <- data.frame(truth=data_validation$TAIEX..t.,prediction=predict(model,newdata=data_validation,type='class'))
  table_train <- table(resultframe_t)
  table_v <- table(resultframe_v)
  
  ##fold column##
  output[i+1,c(1,5,9,13)] <- paste('fold',i,sep = '')
  
 
  ##Accuracy column##
  output[i+1,2] <- round((table_train[1,1]+table_train[2,2])/sum(table_train),2)
  output[i+1,3] <- round((table_v[1,1]+table_v[2,2])/sum(table_v),2)
  
  ##Precision column##
  output[i+1,6] <- round(table_train[2,2]/sum(table_train[,2]),2)
  output[i+1,7] <- round(table_v[2,2]/sum(table_v[,2]),2)
  
  ##Recall column##
  output[i+1,10] <- round(table_train[2,2]/sum(table_train[2,]),2)
  output[i+1,11] <- round(table_v[2,2]/sum(table_v[2,]),2)
  
  ##area under curve##
  pred_t <- predict(model,type='prob')[,2]
  train_auc <- prediction(pred_t,as.vector(data_training$TAIEX..t.))
  auc1 <- performance(train_auc,"auc")@y.values
  output[i+1,14] <- round(as.numeric(auc1),2)
  
  pred_v <- predict(model,newdata = data_validation,type = 'prob')[,2]
  vali_auc <- prediction(pred_v,as.vector(data_validation$TAIEX..t.))
  auc2 <- performance(vali_auc,'auc')@y.values
  output[i+1,15] <- round(as.numeric(auc2),2)
  
  ####training data(2nd time)##
  final_training <- rbind(data_training,data_validation)
  model2 <- rpart(TAIEX..t.~.-date..t. ,data = final_training,control = rpart.control(maxdepth = 4),method = 'class')
  
  ##confusion matrix##
  resultframe_test <- data.frame(truth=data_testing$TAIEX..t.,prediction=predict(model2,newdata=data_testing,type='class'))
  table_test <- table(resultframe_test)
  
  ##Accuracy column##
  output[i+1,4] <- round((table_test[1,1]+table_test[2,2])/sum(table_test),2)
  
  ##Precision column##
  output[i+1,8] <- round(table_test[2,2]/sum(table_test[,2]),2)
  
  ##Recall column##
  output[i+1,12] <- round(table_test[2,2]/sum(table_test[2,]),2)
  
  ##area under curve##
  pred_test <- predict(model2, newdata=data_testing,type='prob')[,2]
  test_auc <- prediction(pred_test,as.vector(data_testing$TAIEX..t.))
  auc5 <-  performance(test_auc,"auc")@y.values
  output[i+1,16] <- round(as.numeric(auc5),2)
  
}


##ave. row##
output[k+2,c(1,5,9,13)] <- 'ave.'

for (j in 1:3) {
  output[k+2,j+1] <- round(mean(as.numeric(output[2:k+1,j+1])),2)
  output[k+2,j+5] <- round(mean(as.numeric(output[2:k+1,j+5])),2)
  output[k+2,j+9] <- round(mean(as.numeric(output[2:k+1,j+9])),2)
  output[k+2,j+13] <- round(mean(as.numeric(output[2:k+1,j+13])),2)
  
}


##Testing the whole data boss and plot ROC Curve##
fullmodel <- rpart(TAIEX..t.~.-date..t. ,data = doc2,control = rpart.control(maxdepth = 4),method = 'class')
resultframe_whole <- data.frame(truth=doc2$TAIEX..t.,prediction=predict(fullmodel,type = 'class'))
table_whole <- table(resultframe_whole)

##Confusion matrix##
resultframe_boss <- data.frame(truth=boss$TAIEX..t.,prediction=predict(fullmodel,newdata = boss,type = 'class'))
table_boss <- table(resultframe_boss)

##Accuracy##
output[2*k,2] <- round((table_whole[1,1]+table_whole[2,2])/sum(table_whole),2)
output[2*k,3] <- round((table_boss[1,1]+table_boss[2,2])/sum(table_boss),2)

##Precision##
output[2*k,6] <- round(table_whole[2,2]/sum(table_whole[,2]),2)
output[2*k,7] <- round(table_boss[2,2]/sum(table_boss[,2]),2)

##Recall##
output[2*k,10] <- round(table_whole[2,2]/sum(table_whole[2,]),2)
output[k*2,11] <- round(table_boss[2,2]/sum(table_boss[2,]),2)

##Auc&ROC Curve##
element <- predict(fullmodel,type = 'prob')[,2]
fulltrain_auc <- prediction(element,as.vector(doc2$TAIEX..t.))
auc_fulltain <-  performance(fulltrain_auc,"auc")@y.values
output[2*k,14] <- round(as.numeric(auc_fulltain),2)

element2 <- predict(fullmodel,newdata = boss,type = 'prob')[,2]
boss_auc <- prediction(element2,as.vector(boss$TAIEX..t.))
auc_boss <-  performance(boss_auc,"auc")@y.values
output[2*k,15] <- round(as.numeric(auc_boss),2)

output[2*k,c(1,4,5,8,9,12,13,16)] <- ''

roc <- performance(prediction(element2,as.vector(boss$TAIEX..t.)),'tpr','fpr')
plot(roc,colorize=T)

##Export file##
write.table(output,file = args[4],sep = ',',col.names = F,row.names = F,quote = F)
