library(readr)
library(ggplot2)
library(dplyr)
library(showtext)
library(fBasics)
library(akima)
##### 1 #####
pred.int.mvnorm <- function(x, alpha=.05) {
  p <- ncol(x)
  n <- nrow(x)
  Sigmahat <- var(x)
  xbar <- apply(x,2,mean)
  xbar
  theta <- seq(0, 2*pi, length=100)
  polygon <- xbar + 
    sqrt(p*(n-1)/(n-p)*(1 + 1/n)*qf(alpha, p, n - p, lower.tail = FALSE))*
    t(chol(Sigmahat)) %*% 
    rbind(cos(theta), sin(theta))
  t(polygon)
}
x <- matrix(c(-0.9,2.4,-1.4,2.9,2.0,0.2,0.7,1.0,-0.5,-1.0),ncol=2)
dt <- as.data.frame(pred.int.mvnorm(x))
dtx <- as.data.frame(x)

ggplot(data=dt, aes(x = V1,y = V2)) + 
  geom_path(color = 'skyblue',size = 1)+ 
  geom_point(data = dtx, aes(x = V1,y = V2),color = '#FF9966',size = 2) + 
  theme(plot.title = element_text(size=16, face="bold")) + 
  labs(title ="Bar Plot of Region",
       x = 'x1', y = 'x2')

#### 2 ####
data(cd4,package = 'boot')
str(cd4)
#normal test for baseline
#H_0:样本来自正态总体
#拟合优度卡方检验
##分段后比较观测频数和理论期望频数
pchiTest(cd4$baseline, description='baseline')
#结果p值>0.05不拒絕H_0，不符合正態

#Jarque-Bera偏度峰度检验:
## JB = $\frac{n}{6}(偏度^2 - \frac{1}{4}峰度^2)$
jbTest(cd4$baseline, title='baseline')

#Kolmogorov-Smirnov检验:
##比较经验分布函数与理论分布函数的最大差
ksnormTest( (cd4$baseline - mean(cd4$baseline))/sd(cd4$baseline) )

#Shapiro-Wilk检验: 
##基于QQ图思想
shapiro.test(cd4$baseline)

#plot
ggplot(cd4,aes(x = baseline,y = ..density..)) + 
    geom_density(color = 'skyblue',size = 3) + 
    theme(plot.title = element_text(size=25, face="bold"), 
                axis.title = element_text(size=20, face="bold"),
                axis.text = element_text(size=18)) + 
    labs(title =paste('Density of baseline'))
  
ggplot(cd4,aes(sample = baseline)) + 
  stat_qq_line(color = 'skyblue',size = 3) + 
  stat_qq(color = '#FF9966',size = 3) + 
  theme(plot.title = element_text(size=25, face="bold"), 
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18)) + 
  labs(title =paste('Normal QQ plot of baseline'),x='baseline')

#成對t test for baseline and one year
#为比较独立的两个总体的均值， 用两样本t检验。
t.test(cd4$baseline, cd4$oneyear)
#结果p值<0.05則有顯著差異
ggplot(cd4) + 
  geom_density(aes(x = baseline,y = ..density..,color = 'skyblue'),size = 3,show.legend = TRUE) + 
  geom_density(aes(x = oneyear,y = ..density..,color = 'blue'),size = 3,show.legend = TRUE) + 
  theme(plot.title = element_text(size=25, face="bold"), 
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18),
        legend.title=element_blank()) + 
  labs(title =paste('Density of baseline and oneyear'),
       x = 'variable') + 
  scale_colour_discrete(breaks=c("blue", "skyblue"),
                      labels=c("oneyear", "baseline")) +
  geom_vline(xintercept = 3.288,linetype = 2,color = 'darkslategray4',size = 2) +
  geom_vline(xintercept = 4.093,linetype = 2,color = 'firebrick4',size = 2)
