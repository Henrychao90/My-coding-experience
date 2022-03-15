library(arm)
library(car)
library(data.table)
library(foreign)
library(gmodels)
library(haven)
##讀入資料##
tscs951 <- read_sav('tscs951.sav')
tscs001 <- read_sav('tscs001.sav')
tscs051 <- read_sav('tscs051.sav')
tscs101 <- read_sav('tscs101.sav')
##調整歸因分數變項##
tscs951$v54x <- recode(tscs951$v54,'1=4;2=3;3=2;4=1;else=NA')
tscs001$v60x <- recode(tscs001$v60,'1=4;2=3;3=2;4=1;else=NA')
tscs051$v39ex <- recode(tscs051$v39e,'1=4;2=3;3=2;4=1;else=NA')
tscs101$v39ex <- recode(tscs101$v39e,'1=4;2=3;3=2;4=1;else=NA')
##調整教育年度##
tscs951$edu <- recode(tscs951$v7a,'1:5=1;6:13=2;14:19=3;else=NA')
tscs001$edu <- recode(tscs001$v7a,'1:5=1;6:13=2;14:19=3;else=NA')
tscs051$edu <- recode(tscs051$v7,'1:5=1;6:13=2;14:21=3;else=NA')
tscs101$edu <- recode(tscs101$v7a,'1:5=1;6:13=2;14:21=3;else=NA')
tscs951$edu <- factor(tscs951$edu,levels = c(1,2,3),labels = c('國中以下','高中職','大專以上'))
tscs001$edu <- factor(tscs001$edu,levels = c(1,2,3),labels = c('國中以下','高中職','大專以上'))
tscs051$edu <- factor(tscs051$edu,levels = c(1,2,3),labels = c('國中以下','高中職','大專以上'))
tscs101$edu <- factor(tscs101$edu,levels = c(1,2,3),labels = c('國中以下','高中職','大專以上'))
##擷取成年人口##
tscs951_cut <- tscs951[which(tscs951$age>19 & tscs951$age<70),]
tscs001_cut <- tscs001[which(tscs001$age>19 & tscs001$age<70),]
tscs051_cut <- tscs051[which(tscs051$age>19 & tscs051$age<70),]
tscs101_cut <- tscs101[which(tscs101$age>19 & tscs101$age<70),]
##擷取歸因分數+年度##
site1 <- tscs951_cut$v54x
site2 <- tscs001_cut$v60x
site3 <- tscs051_cut$v39ex
site4 <- tscs101_cut$v39ex
ex_b1 <- data.frame(Y=c(site1,site2,site3,site4),Site=factor(rep(c('1995','2000','2005','2010'),times=c(length(site1),length(site2),length(site3),length(site4)))))
b1 <- aov(Y~Site,data=ex_b1)                    
anova(b1)
##擷取歸因分數+教育分組##
new1 <- tscs951_cut[which(tscs951_cut$edu=="國中以下"),]
new2 <- tscs951_cut[which(tscs951_cut$edu=="高中職"),]
new3 <- tscs951_cut[which(tscs951_cut$edu=="大專以上"),]

new4 <- tscs001_cut[which(tscs001_cut$edu=='國中以下'),]
new5 <- tscs001_cut[which(tscs001_cut$edu=="高中職"),]
new6 <- tscs001_cut[which(tscs001_cut$edu=="大專以上"),]

new7 <- tscs051_cut[which(tscs051_cut$edu=='國中以下'),]
new8 <- tscs051_cut[which(tscs051_cut$edu=="高中職"),]
new9 <- tscs051_cut[which(tscs051_cut$edu=="大專以上"),]

new10 <- tscs101_cut[which(tscs101_cut$edu=='國中以下'),]
new11 <- tscs101_cut[which(tscs101_cut$edu=="高中職"),]
new12 <- tscs101_cut[which(tscs101_cut$edu=="大專以上"),]

site5 <- c(new1$v54x,new4$v60x,new7$v39ex,new10$v39ex)
site6 <- c(new2$v54x,new5$v60x,new8$v39ex,new11$v39ex)
site7 <- c(new3$v54x,new6$v60x,new9$v39ex,new12$v39ex)

ex_b2 <- data.frame(Y=c(site5,site6,site7),Site=factor(rep(c('國中以下','高中職','大專以上'),times=c(length(site5),length(site6),length(site7)))))
b2 <- aov(Y~Site,data = ex_b2)
anova(b2)
##執行事後檢定##
TukeyHSD(b1,p.adjust.methods='bonf')
TukeyHSD(b2,p.adjust.methods='bonf')

##EX_C##
##讀入資料##
tscs131 <- read_sav('tscs131.sav')
##調整年齡變項##
tscs131$age <- 102-as.numeric(tscs131$v2y)+1
##調整籍貫&社會地位##
tscs131$home <- recode(tscs131$v5,'1=1;2=2;3=3;4=4;else=NA')
tscs131$score <- recode(tscs131$v84,'1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;10=10;else=NA')
##擷取成年人資料##
tscs131_cut <- tscs131[which(tscs131$age>19 & tscs131$age<66),]
##擷取性別+籍貫+社會地位##
mydata <- data.frame(sex=tscs131_cut$v1,home=tscs131_cut$home,score=tscs131_cut$score)
##刪除含有遺漏值的樣本##
mydata <- mydata[complete.cases(mydata),]
##製造Dummy variables(sex_2為女性，home_1為閩南人)##
install.packages('fastDummies')
library(fastDummies)
mydata <- dummy_cols(mydata,select_columns = c('sex','home'))
##建立男性vs社會地位模型(女性為參考組)##
library(lm.beta)
ex_c1 <- lm(mydata$score~mydata$sex_1)
summary(lm.beta(ex_c1))
summary(ex_c1)
##建立籍貫vs社會地位模型(閩南人為參考組)##
ex_c2 <- lm(mydata$score~mydata$home_2+mydata$home_3+mydata$home_4)
summary(lm.beta(ex_c2))
##建立籍貫+男性vs社會地位模型(女性閩南人為參考組，無互動項)##
ex_c3 <- lm(mydata$score~mydata$sex_1+mydata$home_2+mydata$home_3+mydata$home_4)
summary(lm.beta(ex_c3))
##建立籍貫+男性vs社會地位模型(女性閩南人為參考組，有互動項)##
mydata$int1 <- mydata$sex_1*mydata$home_1
mydata$int2 <- mydata$sex_1*mydata$home_2
mydata$int3 <- mydata$sex_1*mydata$home_3
mydata$int4 <- mydata$sex_1*mydata$home_4
ex_c4 <- lm(mydata$score~mydata$sex_1+mydata$home_2+mydata$home_3+mydata$home_4+mydata$int1+mydata$int2+mydata$int3)
summary(lm.beta(ex_c4))
##製作係數表格##
library(huxtable)
huxreg(ex_c1,ex_c2,ex_c3,ex_c4)
##Ex_c2##
mydata$sex1 <- factor(mydata$sex,levels = c('1','2'),labels = c('male','female'))
interaction.plot(mydata$home,mydata$sex1,mydata$score,type='l',xlab = 'hometown',ylab='status',col = c('blue','red'),lty = 1,lwd=3,trace.label = 'Sex')
