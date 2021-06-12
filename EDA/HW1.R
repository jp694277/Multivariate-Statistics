#####package#####
library(readr)
library(ggplot2)
library(gridExtra)
library(hexbin)
library(dplyr)
library(showtext)
library(corrplot)
#####read#####
happy <- read_csv("archive/2016.csv")
head(happy)
str(happy)
showtext.auto(enable = TRUE)
font.add('SimSun')
#####clean#####
colnames(happy)
happy <- happy %>%
  rename('Happiness_Rank' = 'Happiness Rank',
         'Happiness_Score'='Happiness Score',
         'Lower_Confidence_Interval'='Lower Confidence Interval',
         'Upper_Confidence_Interval'='Upper Confidence Interval',
         'Economy_GDP'='Economy (GDP per Capita)',
         'Health'='Health (Life Expectancy)',
         'Trust_Government_Corruption' = 'Trust (Government Corruption)',
         'Dystopia_Residual'='Dystopia Residual')
#####bar#####
ggplot(data=happy, aes(x = Region)) + 
  geom_bar(fill = 'skyblue')+ 
  theme(text=element_text(family="黑體-繁 中黑"), 
        plot.title = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title ="Bar Plot of Region")
#####boxplot######
ggplot(happy,aes(Region,Happiness_Score,fill = Region)) + 
  geom_boxplot() +
  theme(text=element_text(family="黑體-繁 中黑"), 
        plot.title = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title ="Box Plot of Happiness Score by Region")
#####density#####

dp <- function(n,hc,dc,nn){
  ggplot(happy,aes(x = n,y = ..density..)) + 
           geom_histogram(fill = hc) +
           geom_density(color = dc) + 
           theme(text=element_text(family="黑體-繁 中黑"), 
                 plot.title = element_text(size=16, face="bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)) + 
           labs(title =paste('Density of ', nn,
                             sep=''))

}

p1 <- dp(happy$Happiness_Score,'skyblue','blue','Happiness Score')
p2 <- dp(happy$Economy_GDP,'mediumpurple2','purple','Economy GDP')
p3 <- dp(happy$Health,'wheat','gold3','Health')
p4 <- dp(happy$Freedom,'pink','hotpink2','Freedom')
p5 <- dp(happy$Trust_Government_Corruption,'skyblue','blue','Trust Government')
p6 <- dp(happy$Generosity,'mediumpurple2','purple','Generosity')
p7 <- dp(happy$Dystopia_Residual,'wheat','gold3','Dystopia Residual')
p8 <- dp(happy$Family,'pink','hotpink2','Family')

grid.arrange(p1, p2, p3,p4,nrow = 2)
grid.arrange(p5, p6, p7,p8,nrow = 2)
#####scatter#####
sp <- function(x,y,nn,nx,ny){
  ggplot(data=happy, aes(x=x, y=y,group = Region,color = Region))+ 
  geom_point() + 
  theme(text=element_text(family="黑體-繁 中黑")) + 
  scale_color_brewer(palette="Paired") + 
  labs(title =paste('Scatter Plot of ', nn,sep=''),
       x=nx,y=ny)}

p1 <- sp(happy$Economy_GDP,happy$Happiness_Score,'Economy GDP & Happiness Score',
         'Economy_GDP','Happiness_Score')
p2 <- sp(happy$Health,happy$Happiness_Score,'Health & Happiness Score',
         'Health','Happiness_Score')
p3 <- sp(happy$Freedom,happy$Happiness_Score,'Freedom & Happiness Score',
         'Freedom','Happiness_Score')
p4 <- sp(happy$Trust_Government_Corruption,happy$Happiness_Score,'Trust Government Corruption & Happiness Score',
         'Trust_Government_Corruption','Happiness_Score')
p5 <- sp(happy$Generosity,happy$Happiness_Score,'Generosity & Happiness Score',
         'Generosity','Happiness_Score')
p6 <- sp(happy$Dystopia_Residual,happy$Happiness_Score,'Dystopia Residual & Happiness Score',
         'Dystopia_Residual','Happiness_Score')
grid.arrange(p1, p2,nrow = 2)
grid.arrange(p3, p4,nrow = 2)
grid.arrange(p5, p6,nrow = 2)

#####hex#####
hp <- function(x,y,nn,nx,ny){
ggplot(happy, aes(x, y)) + 
  geom_density_2d_filled() + 
    theme(text=element_text(family="黑體-繁 中黑")) + 
    labs(title =paste('Density2D Plot of ', nn,sep=''),
         x=nx,y=ny)
}
p1 <- hp(happy$Economy_GDP,happy$Happiness_Score,'Economy GDP & Happiness Score',
         'Economy_GDP','Happiness_Score')
p2 <- hp(happy$Health,happy$Happiness_Score,'Health & Happiness Score',
         'Health','Happiness_Score')
p3 <- hp(happy$Freedom,happy$Happiness_Score,'Freedom & Happiness Score',
         'Freedom','Happiness_Score')
p4 <- hp(happy$Trust_Government_Corruption,happy$Happiness_Score,'Trust Government Corruption & Happiness Score',
         'Trust_Government_Corruption','Happiness_Score')
p5 <- hp(happy$Generosity,happy$Happiness_Score,'Generosity & Happiness Score',
         'Generosity','Happiness_Score')
p6 <- hp(happy$Dystopia_Residual,happy$Happiness_Score,'Dystopia Residual & Happiness Score',
         'Dystopia_Residual','Happiness_Score')
grid.arrange(p1, p2,p3,p4,p5,p6,nrow = 2)
