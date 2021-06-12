library(readxl)
pca.pkg <- c("FactoMineR", "factoextra", "corrplot")
lapply(pca.pkg, library, character.only=TRUE)
library(devtools)
library(ggbiplot)
library(ggplot2)
library(corrplot)
library(reshape2)
library(showtext)
library("FactoMineR")
showtext.auto(enable = TRUE)
font.add('SimSun')

# read file
df <- read_excel("ex6.7.xls")
a <- df$...1
df <- df[,-1]
rownames(df) <- a
str(df)

## pca
ㄏ
corrplot(cor(df)) #tl.cex=4 #>0.7 食品x交通通讯, 食品x教育, 食品x家庭服务 
#居住x交通通讯, 居住x教育, 居住x家庭服务, 居住x耐用消费品 
#交通通讯x教育, 交通通讯x家庭服务
#教育x家庭服务, 家庭服x务耐用消费品

pca1 <- princomp(df, cor=TRUE) 
summary(pca1, loadings=TRUE) #comp1 食品, 居住, 交通通讯, 教育, 家庭服务, 耐用消费品 
#comp2 衣着, 医疗

df.pca <- PCA(df, graph = FALSE,scale.unit = TRUE)
fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 70)) + 
  theme(text = element_text(size = 40))

ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = NULL,varname.size = 13) + 
  theme(text = element_text(size = 40))

fviz_contrib(df.pca, choice = "var", axes = 1:2, top = 10) + 
  theme(text = element_text(size = 40)) #corr

fviz_pca_var(df.pca, col.var = "contrib",labelsize = 13,
             gradient.cols = c("blue", "yellow", "red"))+ 
  theme(text = element_text(size = 40),
        legend.key.height = unit(2.5, "cm"))

set.seed(123)
var <- get_pca_var(df.pca)
var.kms <- kmeans(var$contrib, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
fviz_pca_var(df.pca, col.var = kms.grp, palette = c("blue", "green", "red"),legend.title = "Cluster",labelsize = 13)+ 
  theme(text = element_text(size = 40),
        legend.key.height = unit(2.5, "cm"))

ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = row.names(df)[1:31],varname.size = 13, labels.size = 8) + 
  theme(text = element_text(size = 40))

## fa 不旋转
fac1 <- factanal(df, factors=2, rotation='none') 
fac1 #f1 食品, 居住, 交通通讯, 教育, 家庭服务, 耐用消费品
#f2 衣着, 医疗, 耐用消费品

load1 <- loadings(fac1)
plot(load1, type='n', main='不旋转的因子分析载荷') 
text(load1, rownames(load1))

## fa varimax
fac2 <- factanal(df, factors=2, rotation='varimax') 
fac2

load2 <- loadings(fac2)
plot(load2, type='n', main='varimax旋转的因子分析载荷')
text(load2, rownames(load2))

# fa promax
fac3 <- factanal(df, factors=2, rotation='promax',scores='regression')
fac3
par(mfrow=c(1,3))
load3 <- loadings(fac3)
plot(load3, type='n', main='promax旋转的因子分析载荷') 
text(load3, rownames(load3))

biplot(fac3$scores, fac3$loadings)
