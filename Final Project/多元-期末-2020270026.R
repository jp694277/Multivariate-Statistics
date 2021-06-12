library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(fBasics)
library(tibble)
library(forcats)
pca.pkg <- c("FactoMineR", "factoextra")
lapply(pca.pkg, library, character.only=TRUE)
library(devtools)
library(ggbiplot)
library(psych)
library(GPArotation)
library(showtext)
library(MASS)
library(CCA) 
library(CCP)
showtext.auto(enable = TRUE)
options(scipen=999)

#Read Data
employment_data <- read_csv("employment_data.csv")
employment_data <- employment_data %>%
  rename('total_weekly_hours_worked' = 'total_weekly_hours_worked(estimates_in_thousands)',
         'employed_female_25_2019'='employed_female_25+_2019',
         'employed_male_25_2019'='employed_male_25+_2019',
         'ratio_of_weekly_hours_worked_by_population'='ratio_of_weekly_hours_worked_by_population_age_15-64'
         )

#Data Cleaning
d <- dplyr::filter(employment_data, !grepl('World|Western|Eastern|Southern|Northern|Sub-Saharan Africa|G20|G7|ASEAN|MENA|CARICOM|BRICS|Asia and the Pacific|European Union|Europe and Central Asia|Arab States|Latin America and the Caribbean|South America|Americas|income|Arab League|America', country))
str(d)
d <- d %>%
  dplyr::filter(country!="Africa") %>%
  dplyr::filter(country!='Central Africa')
employment_data <- d


str(employment_data) #283*9 -> 191*9

#EDA
summary(employment_data[-1])

#Density Plot
dp <- function(n,hc,dc,nn){ ggplot(employment_data,aes(x = n,y = ..density..)) +
geom_histogram(fill = hc) +
  geom_density(color = dc) +
  theme(plot.title = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title =paste('Density of ', nn,
                    sep=''))}

p1 <- dp(employment_data$total_weekly_hours_worked,'skyblue','blue','total_weekly_hours_worked')
p2 <- dp(employment_data$percentage_of_working_hrs_lost,'mediumpurple2','purple','percentage_of_working_hrs_lost')
p3 <- dp(employment_data$percent_hours_lost_40hrs_per_week,'wheat','gold3','percent_hours_lost_40hrs_per_week')
p4 <- dp(employment_data$percent_hours_lost_48hrs_per_week,'pink','hotpink2','percent_hours_lost_48hrs_per_week')
p5 <- dp(employment_data$labour_dependency_ratio,'skyblue','blue','labour_dependency_ratio')
p6 <- dp(employment_data$employed_female_25_2019,'mediumpurple2','purple','employed_female_25_2019')
p7 <- dp(employment_data$employed_male_25_2019,'wheat','gold3','employed_male_25_2019')
p8 <- dp(employment_data$ratio_of_weekly_hours_worked_by_population,'pink','hotpink2','ratio_of_weekly_hours_worked_by_population')
grid.arrange(p1, p2, p3,p4,nrow = 2)
grid.arrange(p5, p6, p7,p8,nrow = 2)

#Box plot
bp <- function(n,hc,nn){
  ggplot(data= employment_data, aes(x = n)) +
  geom_boxplot(fill = hc)+
  theme(plot.title = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title =paste('Box plot of ', nn,
                    sep=''))}

p1 <- bp(employment_data$total_weekly_hours_worked,'skyblue','total_weekly_hours_worked')
p2 <- bp(employment_data$percentage_of_working_hrs_lost,'mediumpurple2','percentage_of_working_hrs_lost')
p3 <- bp(employment_data$percent_hours_lost_40hrs_per_week,'wheat','percent_hours_lost_40hrs_per_week')
p4 <- bp(employment_data$percent_hours_lost_48hrs_per_week,'pink','percent_hours_lost_48hrs_per_week')
p5 <- bp(employment_data$labour_dependency_ratio,'skyblue','labour_dependency_ratio')
p6 <- bp(employment_data$employed_female_25_2019,'mediumpurple2','employed_female_25_2019')
p7 <- bp(employment_data$employed_male_25_2019,'wheat','employed_male_25_2019')
p8 <- bp(employment_data$ratio_of_weekly_hours_worked_by_population,'pink','ratio_of_weekly_hours_worked_by_population')
grid.arrange(p1, p2, p3,p4,nrow = 2)
grid.arrange(p5, p6, p7,p8,nrow = 2)

#Corr
corrplot(cor(employment_data[-1]),tl.cex=0.65, type = "lower",tl.col = "black"
         , tl.srt = 45,addCoef.col ='gold3',number.cex=0.6
         , title='Corrplot of Continuous Variables',mar=c(0,0,1,0))

#Density Plot and Hex Plot only > 0.7
hp <- function(x,y,nn,nx,ny){ ggplot(employment_data, aes(x, y)) +
    geom_hex() +
    labs(title =paste('Hex Plot of ', nn,sep=''),
         x=nx,y=ny)
}

sp <- function(x,y,col,nn,nx,ny){
  ggplot(data=employment_data, aes(x=x, y=y))+ geom_point(color = col) +
    labs(title =paste('Scatter Plot of ', nn,sep=''),
         x=nx,y=ny)}
# total_weekly_hours_worked
p1 <- sp(employment_data$total_weekly_hours_worked,employment_data$percent_hours_lost_40hrs_per_week,
   'skyblue','total_weekly_hours_worked & percent_hours_lost_40hrs_per_week',
   'total_weekly_hours_worked','percent_hours_lost_40hrs_per_week')  
p2 <- hp(employment_data$total_weekly_hours_worked,employment_data$percent_hours_lost_40hrs_per_week,
   'total_weekly_hours_worked & percent_hours_lost_40hrs_per_week',
   'total_weekly_hours_worked','percent_hours_lost_40hrs_per_week')  

p3 <- sp(employment_data$total_weekly_hours_worked,employment_data$percent_hours_lost_48hrs_per_week,
   'mediumpurple2','total_weekly_hours_worked & percent_hours_lost_48hrs_per_week',
   'total_weekly_hours_worked','percent_hours_lost_48hrs_per_week')  
p4 <- hp(employment_data$total_weekly_hours_worked,employment_data$percent_hours_lost_48hrs_per_week,
   'total_weekly_hours_worked & percent_hours_lost_48hrs_per_week',
   'total_weekly_hours_worked','percent_hours_lost_48hrs_per_week')  

p5 <- sp(employment_data$total_weekly_hours_worked,employment_data$employed_female_25_2019,
   'gold3','total_weekly_hours_worked & employed_female_25_2019',
   'total_weekly_hours_worked','employed_female_25_2019')  
p6 <- hp(employment_data$total_weekly_hours_worked,employment_data$employed_female_25_2019,
   'total_weekly_hours_worked & employed_female_25_2019',
   'total_weekly_hours_worked','employed_female_25_2019')  

p7 <- sp(employment_data$total_weekly_hours_worked,employment_data$employed_male_25_2019,
   'hotpink3','total_weekly_hours_worked & employed_male_25_2019',
   'total_weekly_hours_worked','employed_male_25_2019')  
p8 <- hp(employment_data$total_weekly_hours_worked,employment_data$employed_male_25_2019,
   'total_weekly_hours_worked & employed_male_25_2019',
   'total_weekly_hours_worked','employed_male_25_2019')  

#percent_hours_lost_40hrs_per_week
p9 <- sp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$percent_hours_lost_48hrs_per_week,
   'skyblue','percent_hours_lost_40hrs_per_week & percent_hours_lost_48hrs_per_week',
   'percent_hours_lost_40hrs_per_week','percent_hours_lost_48hrs_per_week')  
p10 <- hp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$percent_hours_lost_48hrs_per_week,
   'percent_hours_lost_40hrs_per_week & percent_hours_lost_48hrs_per_week',
   'percent_hours_lost_40hrs_per_week','percent_hours_lost_48hrs_per_week')  

p11 <- sp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$employed_female_25_2019,
   'mediumpurple2','percent_hours_lost_40hrs_per_weekP & employed_female_25_2019',
   'percent_hours_lost_40hrs_per_week','employed_female_25_2019')  
p12 <- hp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$employed_female_25_2019,
   'percent_hours_lost_40hrs_per_weekP & employed_female_25_2019',
   'percent_hours_lost_40hrs_per_week','employed_female_25_2019')  

p13 <- sp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$employed_male_25_2019,
   'gold3','percent_hours_lost_40hrs_per_week & employed_male_25_2019',
   'percent_hours_lost_40hrs_per_week','employed_male_25_2019')  
p14 <- hp(employment_data$percent_hours_lost_40hrs_per_week,employment_data$employed_male_25_2019,
   'percent_hours_lost_40hrs_per_week & employed_male_25_2019',
   'percent_hours_lost_40hrs_per_week','employed_male_25_2019')  

#percent_hours_lost_48hrs_per_week
p15 <- sp(employment_data$percent_hours_lost_48hrs_per_week,employment_data$employed_female_25_2019,
   'hotpink3','percent_hours_lost_48hrs_per_week & employed_female_25_2019',
   'percent_hours_lost_48hrs_per_week','employed_female_25_2019')  
p16 <- hp(employment_data$percent_hours_lost_48hrs_per_week,employment_data$employed_female_25_2019,
   'percent_hours_lost_48hrs_per_week & employed_female_25_2019',
   'percent_hours_lost_48hrs_per_week','employed_female_25_2019')  

p17 <- sp(employment_data$percent_hours_lost_48hrs_per_week,employment_data$employed_male_25_2019,
   'skyblue','percent_hours_lost_48hrs_per_week & employed_male_25_2019',
   'percent_hours_lost_48hrs_per_week','employed_male_25_2019')  
p18 <- hp(employment_data$percent_hours_lost_48hrs_per_week,employment_data$employed_male_25_2019,
   'percent_hours_lost_48hrs_per_week & employed_male_25_2019',
   'percent_hours_lost_48hrs_per_week','employed_male_25_2019')  

#labour_dependency_ratio
p19 <- sp(employment_data$labour_dependency_ratio,employment_data$ratio_of_weekly_hours_worked_by_population,
   'mediumpurple2','labour_dependency_ratio & ratio_of_weekly_hours_worked_by_population',
   'labour_dependency_ratio','ratio_of_weekly_hours_worked_by_population') 
p20 <- hp(employment_data$labour_dependency_ratio,employment_data$ratio_of_weekly_hours_worked_by_population,
   'labour_dependency_ratio & ratio_of_weekly_hours_worked_by_population',
   'labour_dependency_ratio','ratio_of_weekly_hours_worked_by_population') 

#employed_female_25_2019
p21 <- sp(employment_data$employed_female_25_2019,employment_data$employed_male_25_2019,
   'gold3','employed_female_25_2019 & employed_male_25_2019',
   'employed_female_25_2019','employed_male_25_2019') 
p22 <- hp(employment_data$employed_female_25_2019,employment_data$employed_male_25_2019,
   'employed_female_25_2019 & employed_male_25_2019',
   'employed_female_25_2019','employed_male_25_2019') 

grid.arrange(p1, p2, p3,p4,nrow = 2)
grid.arrange(p5, p6, p7,p8,nrow = 2)
grid.arrange(p9, p10, p11,p12,nrow = 2)
grid.arrange(p13, p14, p15,p16,nrow = 2)
grid.arrange(p17, p18, p19,p20,nrow = 2)
grid.arrange(p21, p22,nrow = 1)

#Top20
p_20 <- function(xx,yy,ny,col){
  ggplot(data=df_20, aes(x = reorder(xx, -yy),y=yy)) +
    geom_col(fill=col) + 
    theme(plot.title = element_text(size=16, face="bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title ="total_weekly_hours_worked ")+
    labs(title =paste('Top 20 country of ', ny,sep=''),
         x='country',y=ny)
}
df_20 <- employment_data %>% 
  arrange(desc(total_weekly_hours_worked)) %>%
  head(20)
p1 <- p_20(df_20$country,df_20$total_weekly_hours_worked,'total_weekly_hours_worked','skyblue')

df_20 <- employment_data %>% 
  arrange(desc(percentage_of_working_hrs_lost)) %>%
  head(20)
p2 <- p_20(df_20$country,df_20$percentage_of_working_hrs_lost,'percentage_of_working_hrs_lost','mediumpurple2')

df_20 <- employment_data %>% 
  arrange(desc(percent_hours_lost_40hrs_per_week)) %>%
  head(20)
p3 <- p_20(df_20$country,df_20$percent_hours_lost_40hrs_per_week,'percent_hours_lost_40hrs_per_week','wheat')

df_20 <- employment_data %>% 
  arrange(desc(percent_hours_lost_48hrs_per_week)) %>%
  head(20)
p4 <- p_20(df_20$country,df_20$percent_hours_lost_48hrs_per_week,'percent_hours_lost_48hrs_per_week','pink')

df_20 <- employment_data %>% 
  arrange(desc(labour_dependency_ratio)) %>%
  head(20)
p5 <- p_20(df_20$country,df_20$labour_dependency_ratio,'labour_dependency_ratio','skyblue')

df_20 <- employment_data %>% 
  arrange(desc(employed_female_25_2019)) %>%
  head(20)
p6 <- p_20(df_20$country,df_20$employed_female_25_2019,'employed_female_25_2019','mediumpurple2')

df_20 <- employment_data %>% 
  arrange(desc(employed_male_25_2019)) %>%
  head(20)
p7 <- p_20(df_20$country,df_20$employed_male_25_2019,'employed_male_25_2019','wheat')

df_20 <- employment_data %>% 
  arrange(desc(ratio_of_weekly_hours_worked_by_population)) %>%
  head(20)
p8 <- p_20(df_20$country,df_20$ratio_of_weekly_hours_worked_by_population,'ratio_of_weekly_hours_worked_by_population','pink')
grid.arrange(p1, p2, p3,p4,nrow = 2)
grid.arrange(p5, p6, p7,p8,nrow = 2)

#only for top 20 country of percent_hours_lost_40hrs_per_week
df_20 <- employment_data %>% 
  arrange(desc(percent_hours_lost_40hrs_per_week)) %>%
  head(20)

#norm
pchiTest(df_20$total_weekly_hours_worked, description='total_weekly_hours_worked')
pchiTest(df_20$percentage_of_working_hrs_lost, description='percentage_of_working_hrs_lost') #norm
pchiTest(df_20$percent_hours_lost_40hrs_per_week, description='percent_hours_lost_40hrs_per_week')
pchiTest(df_20$percent_hours_lost_48hrs_per_week, description='percent_hours_lost_48hrs_per_week')
pchiTest(df_20$labour_dependency_ratio, description='labour_dependency_ratio') #norm
pchiTest(df_20$employed_female_25_2019, description='employed_female_25_2019')
pchiTest(df_20$employed_male_25_2019, description='employed_male_25_2019')
pchiTest(df_20$ratio_of_weekly_hours_worked_by_population, description='ratio_of_weekly_hours_worked_by_population') #norm

#star
palette('Set3')
stars(df_20, len=0.5, cex=0.6, key.loc=c(5.5,0.5),draw.segments=TRUE,
      labels = paste(df_20$country),mar=c(4.3,0,0,0))
#face
aplpack::faces(df_20[-1], label=paste(df_20$country),cex=2)

#cluster
df_20 <- data.frame(column_to_rownames(df_20, var = "country"))

max.k <- 8
rat <- numeric(max.k) 
for(kk in seq(max.k)){
  res2 <- kmeans(df_20, centers=kk, nstart=20)
  rat[kk] <- (1 - res2$tot.withinss / res2$totss)*100}
plot(rat, xlab='k', ylab='%var explained', type='b',col = 'skyblue',lwd = 3,
     main = 'Explained Variance of Different K in Kmeans')

#choose k=3
k <- 3
res11 <- kmeans(df_20, centers=2, nstart=20) #97.3
res11

#descending cluster by row mean
xmean1 <- rowMeans(df_20)
cl.new1 <- res11$cluster
cl.new1[] <- as.integer(fct_reorder(factor(cl.new1), xmean1, mean, .desc=TRUE))
cd <- as.data.frame(sort(cl.new1))
colnames(cd) <- 'cluster number'
kbl(cd) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 12)

#scatter plot of clustering
cmap <- c("skyblue", "mediumpurple2", "gold3")
pairs(df_20,col=cmap[res11$clust])

#pca
corrplot(cor(df_20),tl.cex=0.65, type = "lower",tl.col = "black"
         , tl.srt = 45,addCoef.col ='gold3',number.cex=0.6
         , title='Corrplot of Continuous Variables',mar=c(0,0,1,0))
pca1 <- princomp(df_20, cor=TRUE)
summary(pca1, loadings=TRUE) #3 pcs

#scree plot
df_20.pca <- PCA(df_20, graph = FALSE,scale.unit = TRUE)
fviz_eig(df_20.pca, addlabels = TRUE, ylim = c(0, 70)) +
  theme(text = element_text(size = 12))

#biplot
ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = NULL,varname.size = 2.3) +
  xlim(-6,8) +
  ylim(-6,8) + 
  labs(title = 'Biplot') + 
  theme(text = element_text(size = 12)) 

ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = row.names(df_20),
         varname.size = 2.3, labels.size = 2.3) +
  xlim(-6,8) +
  ylim(-6,8) + 
  labs(title = 'Biplot with Country') + 
  theme(text = element_text(size = 12))

#contrib
fviz_contrib(df_20.pca, choice = "var", axes = 1:2, top = 10) +
  theme(text = element_text(size = 14))

fviz_pca_var(df_20.pca, col.var = "contrib",labelsize = 3,
             gradient.cols = c("blue", "yellow", "red"))+
  theme(text = element_text(size = 12),
        legend.key.height = unit(1, "cm"))

set.seed(123)
var <- get_pca_var(df_20.pca)
var.kms <- kmeans(var$contrib, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
fviz_pca_var(df_20.pca, col.var = kms.grp, palette = c("blue", "green", "red")
             ,legend.title = "Cluster",labelsize = 3)+
  theme(text = element_text(size = 12),
        legend.key.height = unit(1, "cm"))

#FA
fac1 <- fa(r=cor(df_20), nfactors=3, rotate="none", SMC=FALSE, fm="mle")
fac1
fac2 <- fa(r=cor(df_20), nfactors=3, rotate="varimax", SMC=FALSE, fm="mle")
fac2
fac3 <- fa(r=cor(df_20), nfactors=3, rotate="promax", SMC=FALSE, fm="mle")
fac3


load1 <- fac1$loadings[,1:2]
plot(load1, type='n', main='不旋转的因子分析载荷',cex.lab=1, cex.axis=0.8, 
     cex.main=1,xlim=c(-1,1.5)) 
text(load1, rownames(load1), cex = 0.8)
load2 <- fac2$loadings[,1:2]
plot(load2, type='n', main='varimax旋转的因子分析载荷',cex.lab=1, cex.axis=0.8,
     cex.main=1,xlim=c(-1,1.5))
text(load2, rownames(load2), cex = 0.8)
load3 <- fac3$loadings[,1:2]
plot(load3, type='n', main='promax旋转的因子分析载荷',cex.lab=1, cex.axis=0.8, 
     cex.main=1,xlim=c(-1,1.5))
text(load3, rownames(load3), cex = 0.8)

plot(loadings(pca1)[,1], loadings(pca1)[,2], xlab="1st PCA",
     ylab="2nd PCA", main="PCA Loadings Plot", type="n",
     cex.lab=1, cex.axis=0.8, cex.main=1,xlim=c(-1,1.5))
text(loadings(pca1)[,1], loadings(pca1)[,2], labels=colnames(df_20)
     , cex = 0.8)

#corresp 對應分析
chisq.test(df_20[,6:7]) #not independent
co <- corresp(df_20[,6:7] %>% round(), nf=2) #1st is better
co
co$cor

ggbiplot.corresp <- function(obj) { require(ggplot2) 
  require(ggrepel)
  require(tibble)
  rscore <- tibble(
    label = rownames(obj$rscore), x = obj$rscore[,1],
    y = obj$rscore[,2]
  )
  cscore <- tibble(
    label = rownames(obj$cscore), x = obj$cscore[,1],
    y = obj$cscore[,2]
  )
  p <- ggplot(mapping = aes(x = x, y = y, label=label)) +
    geom_text_repel(data = rscore, color="black",size=3) + 
    geom_text_repel(data = cscore, color="red",size=3) + 
    geom_hline(yintercept=0, linetype=3, col="gray") + 
    geom_vline(xintercept=0, linetype=3, col="gray") + 
    labs(x='1st CO', y='2nd CO'
         ,title = 'Corresp Plot of employed_female_25_2019 & employed_male_25_2019')
  p} 
ggbiplot.corresp(co)

#con 典型分析
ds <- scale(df_20)
cc1 <- cancor(ds[,3:4], ds[,6:7]) 
cc1$cor
cc1$xcoef
cc1$ycoef

cc2 <- cc(ds[,3:4], ds[,6:7])
cc2$scores$xscores
cc2$scores$yscores

dfc <- data.frame(cc2$scores$xscores[, 1], cc2$scores$yscores[, 1])
colnames(dfc) <- c('x','y')
ggplot(dfc, aes(x, y)) +
  geom_point(color = 'orange',size=2) + 
  labs(title ='第一对典型变量散点图, p=0.9895805',
       x='自变量第一典型变量',y='自变量第二典型变量') + 
  theme(plot.title = element_text(size=12, face="bold"), 
        axis.title = element_text(size=10, face="bold"),
        axis.text = element_text(size=10)) 

cor(ds[,3:4], cc2$scores$xscores)
cor(ds[,6:7], cc2$scores$yscores)

plt.cc(cc2, d1 = 1, d2 = 2, type = "i", var.label = TRUE,int=1)

p.asym(cc2$cor, N=nrow(ds),p=2, q=2, tstat='Pillai')

#古典多维标度分析
d <- dist(df_20)
D <- round(as.matrix(df_20), 0)
kbl(D) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 7)

#point
res <- cmdscale(d, k = 2, eig=TRUE)
p <- as.data.frame(res$points)
colnames(p) <- c('dim1','dim2')
kbl(p) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 7)

#eigen
e <- as.data.frame(res$eig)
row.names(e) <- rownames(df_20)
colnames(e) <- c('eigenvalue')
kbl(e) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 7)

#GOF
g <- as.data.frame(res$GOF)
row.names(g) <- c('绝对累积贡献率','平方累积贡献率') 
colnames(g) <- c('GOF')
kbl(g) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 7)

dp <- as.data.frame(res$points)
colnames(dp) <- c("dim1", "dim2")
dp[["label"]] <- rownames(df_20)
ggplot(data = dp, mapping = aes(x=dim1, y=dim2, label=label)) +
  geom_text_repel(size=4) +
  labs(title="世界劳工状况多维标度分析结果") + 
  theme(plot.title = element_text(size=12, face="bold"),
        axis.title = element_text(size=10, face="bold"),
        axis.text = element_text(size=10)) +
  ylim(c(-100000,200000)) +
  xlim(c(0,4000000))


rotate.mat <- function(theta){
  rbind(c(cos(theta), sin(theta)), c(-sin(theta), cos(theta)))
}
deg <- 240
theta <- pi/180 * deg
MX <- res$points %*% t(rotate.mat(-theta))
dp <- as.data.frame(MX)
colnames(dp) <- c("dim1", "dim2")
dp[["label"]] <- rownames(df_20)

ggplot(data = dp, mapping = aes(x=dim1/100000, y=dim2/100000, label=label)) +
  geom_text_repel(size=2.5) +
  labs(title="世界劳工状况多维标度分析结果") + 
  theme(plot.title = element_text(size=12, face="bold"),
        axis.title = element_text(size=10, face="bold"),
        axis.text = element_text(size=10))  +
  xlim(c(-40,50)) + 
  ylim(c(-50,50))

as.dist(d)
round(dist(res$points)) #after DR


