library(readxl)
library(MASS)
library(showtext)
library(corrplot)
library(CCA) 
library(CCP)
library(ggplot2)
showtext.auto(enable = TRUE)
font.add('SimSun')

# read file
df <- read_excel("ex9.5.xls")
a <- df$地区
df <- df[,-1]
rownames(df) <- a
colnames(df) <- c('工资性收入','经营净收入','财产性收入','转移性收入',
                  '食品','衣着','居住','交通和通信')
str(df)

# corr
cor(df) 
# 食品 vs 工资性收入; 交通和通信 vs 工资性收入;
# 财产性收入 vs 经营净收入; 交通和通信 vs 食品;
# 交通和通信 vs 居住
corrplot(cor(df))
corrplot(cor(df),tl.cex=4)

# 典型相关
ds <- scale(df)
cc1 <- cancor(ds[,1:4], ds[,5:8]) 
cc1$cor# 第一对典型相关系数很大
cc1$xcoef
cc1$ycoef
# 收入來源: 工资性收入;現金消費性支出指標:交通和通信->正相關


cc2 <- cc(ds[,1:4], ds[,5:8])
cc2

dfc <- data.frame(cc2$scores$xscores[, 1], cc2$scores$yscores[, 1])
colnames(dfc) <- c('x','y')
ggplot(dfc, aes(x, y)) +
  geom_point(color = 'orange',size=5) +
  labs(title ='第一对典型变量的散点图, p=0.9717098',
       x='自变量第一典型变量',y='因变量第一典型变量') +
  theme(plot.title = element_text(size=25, face="bold"),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18))

plt.cc(cc2, d1 = 1, d2 = 2, type = "v", var.label = TRUE)
#所有变量的点都集中在很小的范围， 
#说明自变量、因变量与前两对典型变量的相关性都很类似。 
#计算这些相关系数

cor(ds[,1:4], cc2$scores$xscores[ , 1:2])
cor(ds[,5:8], cc2$scores$yscores[ , 1:2])

plt.cc(cc2, d1 = 1, d2 = 2, type = "i", var.label = TRUE)
#将每个观测用第一和第二典型变量表示

#相关系数检验，用Pillai-Bartlett Trace方法:
p.asym(cc1$cor, N=nrow(ds),p=4, q=4, tstat='Pillai')

