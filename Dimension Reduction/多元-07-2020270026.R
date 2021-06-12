library(dplyr)
pca.pkg <- c("FactoMineR", "factoextra", "corrplot")
install.packages(pca.pkg)
lapply(pca.pkg, library, character.only=TRUE)

data(state)
head(state.x77)
dim(state.x77) #8 dim

#因為各列不可比，用样本相关阵。 样本相关阵
#前三个指标相关性强，代表规模。
round( cor(state.x77), 3)
cor(state.x77)[abs(cor(state.x77))>0.7] #Murder vs Illiteracy , Murder vs Life Exp

#pca
pca1 <- princomp(state.x77, cor=TRUE) 
pca1
summary(pca1, loadings=TRUE)
#loadings #5個就夠了 #可以看+-
summary(pca1, loadings=TRUE)$loadings
#为了得到更好的解释，可以用带因子旋转的因子分析。 用 loadings() 计算载荷:

plot(loadings(pca1)[,1], loadings(pca1)[,2], xlab="1st PCA",
     ylab="2nd PCA", main="Loadings Plot", type="n")
text(loadings(pca1)[,1], loadings(pca1)[,2], labels=colnames(state.x77)[1:8]
     )
abline(h=0)
abline(v=0)

biplot(pca1$scores[,1:2], pca1$loadings[,1:2])

library(devtools)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca1,labels = row.names(state.x77)[1:50]) 
ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = row.names(state.x77)[1:50])
ggbiplot(pca1, obs.scale = 1, var.scale = 1,labels = NULL)

# 2D plot for first two components
#pca.dim1 <- pca1$scores[,1]
#pca.dim2 <- pca1$scores[,2]

scores <- predict(pca1) #前三个主成分得分
cbind(state.x77, round( scores[,1:3], 3))

####
state.x77.pca <- PCA(state.x77, graph = FALSE,scale.unit = TRUE)
get_eigenvalue(state.x77.pca)
fviz_eig(state.x77.pca, addlabels = TRUE, ylim = c(0, 50)) #screeplot ##帮助确定主成分个数
var <- get_pca_var(state.x77.pca)
var$coord
#loadings<-sweep(state.x77.pca$var$coord,2,sqrt(state.x77.pca$eig[1:5,1]),FUN="/")
fviz_pca_var(state.x77.pca, col.var = "black")
corrplot(var$contrib, is.corr=FALSE)

fviz_contrib(x.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(x.pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"))

set.seed(123)
var.kms <- kmeans(var$contrib, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
fviz_pca_var(x.pca, col.var = kms.grp, palette = c("blue", "green", "red"),legend.title = "Cluster")
