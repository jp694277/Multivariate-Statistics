library(mvnormtest)
library(mvShapiroTest)
library(readr)
library(dplyr)
library(ggplot2) 
library(GGally)   
options(scipen = 999)
#### read file ####
happy <- read_csv("archive/2016.csv")
happy <- happy %>%
  rename('Happiness_Rank' = 'Happiness Rank',
         'Happiness_Score'='Happiness Score',
         'Lower_Confidence_Interval'='Lower Confidence Interval',
         'Upper_Confidence_Interval'='Upper Confidence Interval',
         'Economy_GDP'='Economy (GDP per Capita)',
         'Health'='Health (Life Expectancy)',
         'Trust_Government_Corruption' = 'Trust (Government Corruption)',
         'Dystopia_Residual'='Dystopia Residual')
head(happy)

happy_con <- select_if(happy, is.numeric)

d <- as.matrix(happy_con[,5:11]) 
n <- nrow(d)
p <- ncol(d)

wrap_1<-wrap(ggally_points,size=2,color="mediumpurple2",alpha=0.3)
wrap_2<-wrap(ggally_densityDiag,size=2,color="skyblue")

ggpairs(happy_con[,5:11],
        lower = list(continuous = wrap_1),
        diag = list(continuous = wrap_2)) + 
  theme(plot.title = element_text(size=25, face="bold"), 
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18))

  
mah <- mahalanobis(d, colMeans(d), var(d)) ## p=6, 应该近似独立chi^2(6)分布
y <- sort(mah)
x <- qchisq((1:n)/(n+1), p)
dt <- data.frame(x,y)
ggplot(dt, aes(x, y)) + 
  geom_point(color = 'orange') + 
  labs(title ='Mahalanobis distance vs. chi-square quantiles',
       x='Chi-square quantile',y='Obs') + 
  geom_smooth(method='lm', formula= y~x,se = FALSE)

z <- qnorm(pchisq(mah, 10)) 
shapiro.test(z)

psych::mardia(d)

d <- t(as.matrix(happy_con[,2:11]))
mvnormtest::mshapiro.test(d)
mvShapiroTest::mvShapiro.Test(as.matrix(happy_con[,5:11]))
