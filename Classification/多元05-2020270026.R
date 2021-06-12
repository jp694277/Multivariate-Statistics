load('provConsume.RData') 
load('Consume2019.RData')
library(showtext)
showtext.auto(enable = TRUE)
font.add('SimSun')
d1 <- provConsume
d2 <- dataConsume 
knitr::kable(d)

rn <- d2$X
cn <- c('食品','衣著','居住','家庭設備及用品','交通和通信','文教娛樂','醫療保健','其他')
rownames(d1) <- rn
rownames(d2) <- rn
colnames(d1) <- cn
colnames(d2) <- cn
d1 <- d1[,1:8]
d2 <- d2[,2:9]

str(d1)
str(d2)

k <- 4
res11 <- kmeans(d1, centers=k, nstart=20) 
res11
res12 <- kmeans(d2, centers=k, nstart=20) 
res12

library(forcats)
xmean1 <- rowMeans(d1)
cl.new1 <- res11$cluster
cl.new1[] <- as.integer(fct_reorder(factor(cl.new1), xmean1, mean, .desc=TRUE))
knitr::kable(as.data.frame(sort(cl.new1)))

xmean2 <- rowMeans(d2)
cl.new2 <- res12$cluster
cl.new2[] <- as.integer(fct_reorder(factor(cl.new2), xmean2, mean, .desc=TRUE))
knitr::kable(as.data.frame(sort(cl.new2)))



cmap <- c("skyblue", "mediumpurple2", "wheat","pink")
pairs(d1, labels=attr(d1, '变量说明'),
      col=cmap[res1$clust])
pairs(d2, labels=attr(d2, '变量说明'),
      col=cmap[res12$clust])

## 按类排序
ind1 <- order(cl.new1) 
d11 <- d1[ind1,]
names(d11) <- cn
stars(d11, len=0.9, cex=0.8, key.loc=c(12,1.6),draw.segments=TRUE,
      labels = paste(row.names(d11), '(', cl.new1[ind1], ')', sep=''))

ind2 <- order(cl.new2) 
d12 <- d2[ind2,]
names(d12) <- cn
stars(d12, len=0.9, cex=0.8,nrow=7, ncol=6, key.loc=c(10, 1), draw.segments=TRUE,
      labels = paste(row.names(d12), '(', cl.new2[ind1], ')', sep=''))


palette(rainbow(12)) 
aplpack::faces(
  d1, label=paste(row.names(d1), '(', cl.new1[ind1], ')', sep='') )

aplpack::faces(
  d2, label=paste(row.names(d2), '(', cl.new2[ind2], ')', sep='') )

max.k <- 8
rat <- numeric(max.k) 
for(kk in seq(max.k)){
  res2 <- kmeans(d1, centers=kk, nstart=20)
  rat[kk] <- (1 - res2$tot.withinss / res2$totss)*100 }
plot(rat, xlab='k', ylab='%var explained', type='b')

max.k <- 8
rat <- numeric(max.k) 
for(kk in seq(max.k)){
  res2 <- kmeans(d2, centers=kk, nstart=20)
  rat[kk] <- (1 - res2$tot.withinss / res2$totss)*100 }
plot(rat, xlab='k', ylab='%var explained', type='b')
