library(arm)
library(car)
library(data.table)
library(foreign)
library(gmodels)
library(haven)
library(lm.beta)
library(huxtable)
library(ggplot2)
library(fastDummies)
##讀入資料##
mydata <- read_sav('prestige.sav')
##調整受訪者教育年數##
mydata$edu <- recode(as.numeric(mydata$v13a),'1:2=0;3=6;4:5=9;6:9=12;10:11=14;12=15;13=14;14:16=16;17=18;18=22;else=NA')
##製作性別Dummy variable##
mydata$sex <- recode(as.numeric(mydata$v1),'1=1;2=0')
##製作交互作用項##
mydata$int <- 
##製作線性模型##
outputa1 <- lm(mydata$v32b_pre~mydata$edu+mydata$sex)
summary(lm.beta(outputa1))
outputa2 <- lm(v32b_pre~edu+sex+edu*sex,data=mydata)
summary(lm.beta(outputa2))
##製作係數表格##
huxreg(outputa1,outputa2)
##畫出座標圖##
install.packages('sjPlot')
library(sjPlot)
theme_set(theme_sjplot())
plot_model(outputa2,type = 'int')

a1_m <- 39.899+mean(mydata$v26b_pre,na.rm=T)-1.31
a1_f <- 39.899+mean(mydata$v26b_pre,na.rm=T)
ex_a1 <- data.frame(初職聲望=round(c(a1_m,a1_f),3),性別=c('男','女'))
a2_m <- 40.221+mean(mydata$v26b_pre,na.rm=T)-2.179+0.084
a2_f <- 40.221+mean(mydata$v26b_pre,na.rm=T)
ex_a2 <- data.frame(初職聲望=round(c(a2_m,a2_f),3),性別=c('男','女'))
ggplot() +
  geom_point(aes(x=ex_a1$性別,y=ex_a1$初職聲望,shape=ex_a1$性別))+
  geom_line(aes(x=ex_a1$性別,y=ex_a1$初職聲望,group=1))+
  labs(title='初職聲望分數模型',x='性別',y='初職聲望',shape='圖例')
ggplot() +
  geom_point(aes(x=ex_a2$性別,y=ex_a2$初職聲望,shape=ex_a2$性別))+
  geom_line(aes(x=ex_a2$性別,y=ex_a2$初職聲望,group=1))+
  labs(title='初職聲望分數模型(交互)',x='性別',y='初職聲望',shape='圖例')  

##EX_B##
tscs971 <- read_sav('tscs971_l.sav')
tscs071 <- read_sav('tscs071.sav')
tscs171 <- read_sav('tscs171.sav')
##調整年紀&性別##
tscs171$age <- 106-tscs171$a2y+1
tscs971$sex <- factor(tscs971$v1,levels = c(1,2),labels = c('male','female'))
tscs071$sex <- factor(tscs071$a1,levels = c(1,2),labels = c('male','female'))
tscs171$sex <- factor(tscs171$a1,levels = c(1,2),labels = c('male','female'))
##調整教育年數##
tscs971$edu <- recode(as.numeric(tscs971$v13a),'1:2=0;3=6;4:5=9;6:9=12;10:11=14;12=15;13=14;14:16=16;17=18;18=22;else=NA')
tscs071$edu <- recode(as.numeric(tscs071$c1),'1:2=0;3=6;4:5=9;6:9=12;10=14;13=14;14:16=15;17:19=16;20=18;21=22;else=NA')
tscs171$edu <- recode(as.numeric(tscs171$b1),'1:2=0;3=6;4:5=9;6:9=12;10=14;13=14;14:16=15;17:19=16;20=18;21=22;else=NA')
##調整籍貫##
tscs971$home <- recode(tscs971$v5,'1=1;2=2;3=3;4=4;else=NA')
tscs071$home <- recode(tscs071$a5,'1=1;2=2;3=3;4=4;else=NA')
tscs171$home <- recode(tscs171$a6,'1=1;2=2;3=3;4:6=4;else=NA')
##調整主觀地位分數##
tscs971$score <- recode(tscs971$v86a,'97:98=NA')
tscs071$score <- recode(tscs071$f1,'95:98=NA')
tscs171$score <- recode(tscs171$e1,'97:98=NA')
##擷取成年人資料##
tscs971_cut <- tscs971[which(tscs971$age>19 & tscs971$age<66 & tscs971$sex=='male'),]
tscs071_cut <- tscs071[which(tscs071$age>19 & tscs071$age<66 & tscs071$sex=='male'),]
tscs171_cut <- tscs171[which(tscs171$age>19 & tscs171$age<66 & tscs171$sex=='male'),]
##擷取地位分數+教育年數+籍貫+年度##
b1997 <- data.frame(score=as.numeric(tscs971_cut$score),edu=as.numeric(tscs971_cut$edu),home=as.numeric(tscs971_cut$home),year=1997)
b2007 <- data.frame(score=as.numeric(tscs071_cut$score),edu=as.numeric(tscs071_cut$edu),home=as.numeric(tscs071_cut$home),year=2007)
b2017 <- data.frame(score=as.numeric(tscs171_cut$score),edu=as.numeric(tscs171_cut$edu),home=as.numeric(tscs171_cut$home),year=2017)
##合併資料##
ex_b <- rbind(b1997,b2007,b2017)
##刪除含有遺漏值的樣本##
ex_b <- ex_b[complete.cases(ex_b),]
##製作Dummy variables(1997,閩南人當對照組)##
ex_b <- dummy_columns(ex_b,select_columns =c('home','year'))
##建立回歸模型##
##地位分數vs教育年數##
outputb1 <- lm(ex_b$score~ex_b$edu)
summary(lm.beta(outputb1))
##畫圖## 
ggplot()+
  geom_point(aes(x=ex_b$edu,y=ex_b$score))+
  geom_abline(slope=0.08,intercept =outputb1[['coefficients']][['(Intercept)']],col='blue' )+
  labs(title = 'testing',x='year',y='reputation',color='ex')
##畫圖model2##
ggplot()+
  geom_point(aes(x=ex_b$edu,y=ex_b$score))+
  geom_abline(slope = outputb2[['coefficients']][['ex_b$edu']],intercept = outputb2[['coefficients']][['(Intercept)']],col='blue')+
  geom_abline(slope = outputb2[['coefficients']][['ex_b$edu']],intercept = outputb2[['coefficients']][['(Intercept)']]+outputb2[['coefficients']][['ex_b$home_2']],col='red')+
  geom_abline(slope = outputb2[['coefficients']][['ex_b$edu']],intercept = outputb2[['coefficients']][['(Intercept)']]+outputb2[['coefficients']][['ex_b$home_3']],col='green')+
  geom_abline(slope = outputb2[['coefficients']][['ex_b$edu']],intercept = outputb2[['coefficients']][['(Intercept)']]+outputb2[['coefficients']][['ex_b$home_4']],col='purple')+
  labs(title = 'testing',x='year',y='reputation')
  
  

##地位分數vs教育年數+籍貫##
outputb2 <- lm(ex_b$score~ex_b$edu+ex_b$home_2+ex_b$home_3+ex_b$home_4)
summary(lm.beta(outputb2))
##地位分數vs教育年數+調查年度##
outputb3 <- lm(ex_b$score~ex_b$edu+ex_b$year_2007+ex_b$year_2017)
summary(lm.beta(outputb3))
##地位分數vs教育年數+籍貫(有互動項)##
ex_b$int1 <- ex_b$edu*ex_b$home_2
ex_b$int2 <- ex_b$edu*ex_b$home_3
ex_b$int3 <- ex_b$edu*ex_b$home_4
outputb4 <- lm(ex_b$score~ex_b$edu+ex_b$home_2+ex_b$home_3+ex_b$home_4+ex_b$int1+ex_b$int2+ex_b$int3)
summary(lm.beta(outputb4))
##地位分數vs教育年數+調查年度(有互動項)##
ex_b$int4 <- ex_b$edu*ex_b$year_2007
ex_b$int5 <- ex_b$edu*ex_b$year_2017
outputb5 <- lm(ex_b$score~ex_b$edu+ex_b$year_2007+ex_b$year_2017+ex_b$int4+ex_b$int5)
summary(lm.beta(outputb5))
##製作係數表格##
huxreg(outputb1,outputb2,outputb3,outputb4,outputb5)
##畫出座標圖##
##model2##
edu_mean <- mean(ex_b$edu)
M2a <- 4.31+0.08105*edu_mean
M2b <- 4.31+0.08105*edu_mean+0.13818
M2c <- 4.31+0.08105*edu_mean-0.14757
M2d <- 4.31+0.08105*edu_mean+0.32677
M2 <- data.frame(score=round(c(M2a,M2b,M2c,M2d),3),home=c('閩南','客家','原住民','大陸各省'))
ggplot() +
  geom_point(aes(x=M2$home,y=M2$score,shape=M2$home))+
  geom_line(aes(x=M2$home,y=M2$score,group=1))+
  labs(title = '主觀社會地位分數vs籍貫',x='籍貫',y='地位分數',shape='圖例')
##model3##
M3a <- 4.53+0.05735*edu_mean
M3b <- 4.53+0.05735*edu_mean-0.60473
M3c <- 4.53+0.05735*edu_mean+0.85411
M3 <- data.frame(score=round(c(M3a,M3b,M3c),3),year=c('1997','2007','2017'))
ggplot() +
  geom_point(aes(x=M3$year,y=M3$score,shape=M3$year))+
  geom_line(aes(x=M3$year,y=M3$score,group=1))+
  labs(title = '主觀社會地位分數vs調查年度',x='年度',y='地位分數',shape='圖例')
##model4##
M4a <- 4.33+0.07942*edu_mean
M4b <- 4.33-0.07942*edu_mean-0.27166
M4c <- 4.33+0.07942*edu_mean-0.9269
M4d <- 4.33+0.07942*edu_mean+2.10583
M4 <- data.frame(score=round(c(M4a,M4b,M4c,M4d),3),home=c('閩南','客家','原住民','大陸各省'))
ggplot() +
  geom_point(aes(x=M4$home,y=M4$score,shape=M4$home))+
  geom_point(aes(x=M2$home,y=M2$score,shape=M2$home))+
  geom_line(aes(x=M4$home,y=M4$score,group=1))+
  geom_line(aes(x=M2$home,y=M2$score,group=1))+
  labs(title = '主觀社會地位分數vs籍貫(交互)',x='籍貫',y='地位分數',shape='圖例')
##model5##
M5a <- 3.24+0.17135*edu_mean
M5b <- 3.24+0.17135*edu_mean-0.43542
M5c <- 3.24+0.17135*edu_mean+5.48009
M5 <- data.frame(score=round(c(M5a,M5b,M5c),3),year=c('1997','2007','2017'))
ggplot() +
  geom_point(aes(x=M3$year,y=M3$score,shape=M3$year))+
  geom_line(aes(x=M3$year,y=M3$score,group=1))+
  geom_point(aes(x=M5$year,y=M5$score,shape=M5$year))+
  geom_line(aes(x=M5$year,y=M5$score,group=1))+
  labs(title = '主觀社會地位分數vs調查年度(交互)',x='年度',y='地位分數',shape='圖例')
