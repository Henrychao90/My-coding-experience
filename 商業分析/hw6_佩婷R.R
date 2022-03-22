##hw6##

##Read in data##
hw6 <- read.csv('hw6-fb.csv')

##checking data##
str(hw6)
summary(hw6)
hw6$visit_date <- as.Date(hw6$visit_date)

##split Data(tips$tools)
library(tidyverse)
library(ggplot2)
##Comparing clicking rate##
d1 <- hw6 %>% filter(condition=='tips'& clicked_article=='1')
d2 <- hw6 %>% filter(condition=='tools'& clicked_article=='1')

n1 <- nrow(hw6 %>% filter(condition=='tips'))
n2 <- nrow(hw6 %>% filter(condition=='tools'))
p1 <- nrow(d1)/n1
p2 <- nrow(d2)/n2

dif <- ((p2-p1)/p1)*100
dif
##dif almost equals to 1##

##Statistical tests##
library(pwr)
prop.test(c(nrow(d1),nrow(d2)),c(n1,n2))

##Comparing like rate##
d3 <- hw6 %>% filter(condition=='tips'& clicked_like=='1')
d4 <- hw6 %>% filter(condition=='tools'& clicked_like=='1')
p3 <- nrow(d3)/n1
p4 <- nrow(d4)/n2

dif2 <- ((p4-p3)/p3)*100
dif2
##dif2 is about 58.5%##

##Statistical tests##
prop.test(c(nrow(d3),nrow(d4)),c(n1,n2),conf.level = 0.99)


##Comparing sharing rate##
d5 <- hw6 %>% filter(condition=='tips'& clicked_share=='1')
d6 <- hw6 %>% filter(condition=='tools'& clicked_share=='1')
p5 <- nrow(d5)/n1
p6 <- nrow(d6)/n2

dif3 <- ((p6-p5)/p5)*100
dif3
##dif2 is about 8.7%##

##Statistical tests##
prop.test(c(nrow(d5),nrow(d6)),c(n1,n2))

