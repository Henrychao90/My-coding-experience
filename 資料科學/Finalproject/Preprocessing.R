##Preprocessing##
doc <- read.csv('ourdata.csv')

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

##Export csv##
write.table(doc2,file = 'only_diff.csv',sep=',',row.names = F,quote = F)
