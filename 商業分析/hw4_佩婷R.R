##hw4##

##read in data##
data <- read.csv('financialdata.csv')
data <- data[,-1]
str(data)
summary(data)
##change factor into numeric##
data$op_profit_growth_rate <- as.character(data$op_profit_growth_rate)
data$op_profit_growth_rate <- gsub(',','',data$op_profit_growth_rate)
data$op_profit_growth_rate <- as.numeric(data$op_profit_growth_rate)

data$current_ratio <- as.character(data$current_ratio)
data$current_ratio <- gsub(',','',data$current_ratio)
data$current_ratio <- as.numeric(data$current_ratio)

data$quick_rartio <- as.character(data$quick_rartio)
data$quick_rartio <- gsub(',','',data$quick_rartio)
data$quick_rartio <- as.numeric(data$quick_rartio)
summary(data)
str(data)

##SPCA##
library(nsprcomp)
spca <- nscumcomp(data, k=80, nneg=T, scale=T)
summary(spca)
screeplot(spca)

##Q1##
pve=(spca$sdev)^2 / (sum(spca$sdev^2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')#scree plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
abline(h=0.8,col='blue')

##Q2##
library(ggplot2)
library(reshape2)
ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

##Q3##
g1 <- which(data$roe>spca$center[1])   
g2 <- which(data$rev_growth_rate>spca$center[9])
g3 <- which(data$profit_margin_rate>spca$center[3])
g4 <- which(data$op_profit_growth_rate>spca$center[11])
i1 <- intersect(intersect(intersect(g1,g2),g3),g4)
g5 <- which(data$margin_growth_rate>spca$center[10])
g6 <- which(data$current_ratio>spca$center[14])
g7 <- which(data$equity_turnnover>spca$center[8])
intersect(intersect(intersect(g5,i1),g6),g7)
