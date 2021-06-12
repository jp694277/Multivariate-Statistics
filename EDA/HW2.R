library(readr)
library(ggplot2)
library(gridExtra)
library(hexbin)
library(dplyr)
library(showtext)
library(GGally)
library(aplpack)

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

#### plot ####
#hex
hp <- function(x,y,nn,nx,ny){
  ggplot(happy, aes(x, y)) + 
    geom_hex() + 
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

#radar
ggplot(data=happy, aes(x = Region,fill = as.factor(Region))) + 
  geom_bar()+ 
  theme(plot.title = element_text(size=16, face="bold"),
        axis.text.x = element_blank()) + 
  labs(title ="Bar Plot of Region",fill = "Region") + 
  coord_polar(theta = "x") + 
  scale_fill_brewer(palette="Set3")

#parallel
ggparcoord(happy, columns = c(4,7,8,9), groupColumn = 2)  + 
  scale_color_manual(values=c( "skyblue", "mediumpurple2", "wheat","pink","#006699",
                               "#FF9966","#FFCC99","#66CC66","#009999","#6699FF") )
ggparcoord(happy, columns = 10:13, groupColumn = 2)  + 
  scale_color_manual(values=c( "skyblue", "mediumpurple2", "wheat","pink","#006699",
                               "#FF9966","#FFCC99","#66CC66","#009999","#6699FF") )
#face
happy_g <- happy %>%
  group_by(Region) %>%
  summarise(Happiness_Score = mean(Happiness_Score),
            Lower_Confidence_Interval = mean(Lower_Confidence_Interval),
            Upper_Confidence_Interval = mean(Upper_Confidence_Interval),
            Economy_GDP = mean(Economy_GDP),
            Family = mean(Family),
            Health = mean(Health),
            Freedom  = mean(Freedom),
            Trust_Government_Corruption = mean(Trust_Government_Corruption),
            Generosity = mean(Generosity),
            Dystopia_Residual = mean(Dystopia_Residual)
  )
  
aplpack::faces(happy_g[,2:11],labels = happy_g$Region,cex = 1)

