library(arm)
library(car)
library(data.table)
library(foreign)
library(haven)
library(nnet)
library(stargazer)
library(fastDummies)
library(ggplot2)
library(Hmisc)
library(huxtable)
setwd('C:/Users/USER/Documents')
##第一題##
tscs021 <- read_sav('tscs021_prestige.sav')
##調整教育程度##
tscs021$fedu <- recode(as.numeric(tscs021$v16a),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
tscs021$edu <- recode(as.numeric(tscs021$v13a),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
##調整年齡##
tscs021$age <- 91-as.numeric(tscs021$v2y)+1
##調整生活滿意度##
tscs021$fill <- recode(as.numeric(tscs021$v138),'1=6;2=5;3=4;4=3;5=2;6=1;else=NA')
##擷取資料##
tscs021$prestige <- as.numeric(tscs021$v26b_pre)
var <- c('age','edu','fedu','prestige','fill')
ex_1a <- tscs021[var]
##擷取成年人資料即刪除遺漏值##
ex_1 <- ex_1a[which(ex_1a$age>19 & ex_1a$age<66),]
ex_1 <- ex_1[complete.cases(ex_1),]
##計算相關係數##
cor(ex_1a,use='pairwise')
##建立回歸模型##
modela1 <- lm(ex_1$fill~ex_1$prestige)
modela2 <- lm(ex_1$fill~ex_1$prestige+ex_1$fedu)
modela3 <- lm(ex_1$fill~ex_1$prestige+ex_1$fedu+ex_1$edu)
huxreg(modela1,modela2,modela3)


##第二題##
tscs021$sex <- factor(tscs021$v1,levels = c(1,2),labels = c('男','女'))
##調整母親教育年數##
tscs021$medu <- recode(as.numeric(tscs021$v15),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
##調整主觀社會地位##
tscs021$score <- recode(as.numeric(tscs021$v122),'1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA')
##擷取資料##
ex_2a <- data.frame(age=tscs021$age,sex=tscs021$sex,medu=tscs021$medu,score=tscs021$score)
##擷取成年人資料##
ex_2 <- ex_2a[which(ex_2a$age>19 & ex_2a$age<66),]
ex_2 <- ex_2[complete.cases(ex_2),]
##建立回歸模型##
modelb1 <- lm(ex_2$score~ex_2$medu+relevel(ex_2$sex,ref='女'))
modelb2 <- lm(ex_2$score~ex_2$medu+relevel(ex_2$sex,ref='女')+ex_2$medu*relevel(ex_2$sex,ref='女'))
summary(modelb1)
summary(modelb2)
huxreg(modelb1,modelb2)
##劃出座標圖##
ggplot()+
  geom_point(aes(x=ex_2$medu,y=ex_2$score))+
  geom_abline(slope = modelb1[['coefficients']][['ex_2$medu']],intercept =modelb1[['coefficients']][['(Intercept)']],col='red' )+
  geom_abline(slope = modelb1[['coefficients']][['ex_2$medu']],intercept =modelb1[['coefficients']][['(Intercept)']]+modelb1[['coefficients']][['relevel(ex_2$sex, ref = "女")男']],col='blue' )+
  labs(title='主觀地位分數',x='教育年數(母)',y='地位分數',subtitle='紅(女),藍(男)')
ggplot()+
  geom_point(aes(x=ex_2$medu,y=ex_2$score))+
  geom_abline(slope = modelb2[['coefficients']][['ex_2$medu']],intercept =modelb2[['coefficients']][['(Intercept)']],col='red' )+
  geom_abline(slope = modelb2[['coefficients']][['ex_2$medu']],intercept =modelb2[['coefficients']][['(Intercept)']]+modelb2[['coefficients']][['relevel(ex_2$sex, ref = "女")男']]+modelb2[['coefficients']][['ex_2$medu:relevel(ex_2$sex, ref = "女")男']],col='blue' )+
  labs(title='主觀地位分數(含交互作用)',x='教育年數(母)',y='地位分數',subtitle='紅(女),藍(男)')


##第三題##
##調整父親籍貫##
tscs021$home <- recode(as.numeric(tscs021$v5),'1=1;2=2;3=3;4=4;else=NA')
tscs021$home <- factor(tscs021$home,levels = c(1,2,3,4),labels = c('閩南人','客家人','大陸人','外省人'))
##製作上層階級變項##
tscs021$upper <- recode(as.numeric(tscs021$score),'1:6=0;7:10=1;else=NA')
##擷取資料##
var1 <- c('age','home','medu','upper')
ex_ca <- tscs021[var1]
##擷取成年人資料##
ex_3 <- ex_ca[which(ex_ca$age>19 & ex_ca$age<66),]
ex_3 <- ex_3[complete.cases(ex_3),]
##製作logistic模型##
modelc1 <- glm(ex_3$upper~ex_3$medu+relevel(ex_3$home,ref='閩南人'),family='binomial')
summary(modelc1)
modelc2 <- glm(ex_3$upper~ex_3$medu+relevel(ex_3$home,ref='閩南人')+ex_3$medu*relevel(ex_3$home,ref='閩南人'))
##製作係數表格##
stargazer(modelc1,modelc2,type = 'html',title = '上層階級之邏輯迴歸分析',align=T,out='上層階級之邏輯迴歸分析.html')


##第四題##
##讀取資料##
tscs051 <- read_sav('tscs051.sav')
tscs091 <- read_sav('tscs091.sav')
tscs191 <- read_sav('tscs191.sav')
##調整幸福感變項##
tscs051$lucky <- recode(as.numeric(tscs051$v69),'1=1;2=2;3=3;4=4;else=NA')
tscs091$lucky <- recode(as.numeric(tscs091$d5),'1=1;2=2;3=3;4=4;else=NA')
tscs191$lucky <- recode(as.numeric(tscs191$d19),'1=1;2=2;3=3;4=4;else=NA')
##調整年齡變項##
tscs051$age <- as.numeric(tscs051$age)+1
tscs091$age <- as.numeric(tscs091$age)+1
tscs191$age <- 108-as.numeric(tscs191$a2y)+1
tscs051$age2 <- (as.numeric(tscs051$age)**2)/100
tscs091$age2 <- (as.numeric(tscs091$age)**2)/100
tscs191$age2 <- (as.numeric(tscs191$age)**2)/100
##調整性別變項##
tscs051$sex <- as.numeric(tscs051$v1)
tscs091$sex <- as.numeric(tscs091$a1)
tscs191$sex <- as.numeric(tscs191$a1)
##調整教育程度##
tscs051$edu <- recode(as.numeric(tscs051$v7),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
tscs091$edu <- recode(as.numeric(tscs091$a9),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
tscs191$edu <- recode(as.numeric(tscs191$a13),'1:2=0;3=6;4=9;5=10;6:9=12;10=14;11=11;12=12;13:15=14;16:19=16;20=18;21=21;else=NA')
##調整父親籍貫##
tscs051$home <- recode(as.numeric(tscs051$v4),'1=1;2=2;3=3;4=4;else=NA')
tscs091$home <- recode(as.numeric(tscs091$a5),'1=1;2=2;3=3;4=4;else=NA')
tscs191$home <- recode(as.numeric(tscs191$a6),'1=1;2=2;3=3;4=4;else=NA')
##調整主觀社會地位分數##
tscs051$score <- recode(as.numeric(tscs051$v107),'1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA')
tscs091$score <- recode(as.numeric(tscs091$b10a),'1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA')
tscs191$score <- recode(as.numeric(tscs191$b14a),'1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA')
##建立年度##
tscs051$year <- as.numeric(tscs051$year)
tscs091$year <- as.numeric(tscs091$year)
tscs191$year <- 2019
##擷取資料##
var2 <- c('lucky','age','age2','edu','sex','home','score','year')
tscs051_cut <- tscs051[var2]
tscs091_cut <- tscs091[var2]
tscs191_cut <- tscs191[var2]
##合併資料##
a1 <- rbind(tscs051_cut,tscs091_cut,tscs191_cut)
##擷取成年人資料##
mydata <- a1[which(a1$age>19 & a1$age<66),]
mydata <- mydata[complete.cases(mydata),]
##調整變項##
mydata$sex <- recode(as.numeric(mydata$sex),'1=1;2=0')
mydata$year <- factor(mydata$year,levels = c(2005,2009,2019),labels=c('2005年','2009年','2019年'))
mydata$home <- factor(mydata$home,levels = c(1,2,3,4),labels=c('閩南人','客家人','原住民','外省人'))
##製作模型##
modeld1 <- lm(mydata$lucky~relevel(mydata$year,ref='2005年')+mydata$sex+mydata$age+mydata$age2+relevel(mydata$home,ref = '閩南人')+mydata$edu)
modeld2 <- lm(mydata$lucky~relevel(mydata$year,ref='2005年')+mydata$sex+mydata$age+mydata$age2+relevel(mydata$home,ref = '閩南人')+mydata$edu+mydata$score)
modeld3 <- lm(mydata$lucky~relevel(mydata$year,ref='2005年')+mydata$sex+mydata$age+mydata$age2+relevel(mydata$home,ref = '閩南人')+mydata$edu+mydata$score+mydata$score*relevel(mydata$year,ref='2005年'))
##製作係數表格##
stargazer(modeld1,modeld2,modeld3,title = '個人幸福感分數之迴歸分析',dep.var.labels='幸福感分數(1~4分)',covariate.labels=c('2009年(vs2005年)','2019年(vs2005年)','性別','年齡','年齡2','客家人(vs閩南)','原住民(vs閩南)','外省人(vs閩南)','教育程度','主觀社會地位分數','2009x主觀社會地位分數','2019x主觀社會地位分數','常數'),type='html',align=T,out='個人幸福感分數之迴歸分析.html')
