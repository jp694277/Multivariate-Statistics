library(readxl)
#library(MASS)
library(showtext)
#library(corrplot)
#library(CCA) 
#library(CCP)
library(ggplot2)
library(ggrepel)
showtext.auto(enable = TRUE)
font.add('SimSun')

# read file
df <- read_excel("ex10.2.xls")
a <- df$...1
df <- df[,-1]
rownames(df) <- a
colnames(df) <- c('农业产值','林业产值','牧业产值','企业人数','企业总产值','利润总额')
str(df)

#analysis
#農村經濟狀況的多维标度分析结果:
d <- dist(df) 
print(round(as.matrix(d), 0))
res <- cmdscale(d, k = 2, eig=TRUE) 
print(res)

#参数中 k 是降低到的维数， eig=TRUE 要求输出特征值。  -> k=2
#结果是列表，其中:
#points 是 矩阵，每行是一个降维坐标 
#eig是21个特征值 
#GOF是绝对值累积贡献率和平方累积贡献率。 
#降到2维时贡献率很高。

#plot
rotate.mat <- function(theta){
  rbind(c(cos(theta), sin(theta)), c(-sin(theta), cos(theta)))
}
deg <- 90
theta <- pi/180 * deg
MX <- res$points %*% t(rotate.mat(-theta))
dp <- as.data.frame(MX) 
colnames(dp) <- c("dim1", "dim2") 
dp[["label"]] <- rownames(df) 
ggplot(data = dp, mapping = aes(x=dim1, y=dim2, label=label)) + 
geom_text_repel() + 
  labs(title="农村经济状况多维标度分析结果")

#多维数据如果想做二维或三维图， 需要进行降维。 虽然主成分、因子分析可以降维， 但是关注的保留信息 不同。 
#多维标度分析关注的是保留原来的距离信息或相似度信息。

#contrast 原始里程矩阵和降维到2维的近似距离矩阵
as.dist(d) #original
round(dist(res$points)) #after DR
