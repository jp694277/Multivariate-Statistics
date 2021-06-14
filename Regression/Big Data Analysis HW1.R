library(dplyr)
library(ggplot2)
library(sf)
library(GISTools)
library(raster)
library(ISLR)
library(GGally)
library(RSE)
library(broom)
library(ggfortify)
library(MASS)
library(caret)
options(scipen = 999)
#######1######
#####1.1######
#create data
set.seed(2020270026)
n <- 1000
x1 <- rnorm(n,0,3)
x2 <- rbinom(n, size=1, prob=0.47)  #0,1
x3 <- runif(n, 18, 60)
a <- 2
b1 <- 2
b2 <- 0.4
b3 <- 0.02
e <- runif(n,-1,1)
y <- a + b1*x1 + b2*x2 + b3*x3 + e
dat <- cbind.data.frame(y, x1, x2, x3)

#ols
x <- data.matrix(dat[2:4])
y_m <- data.matrix(dat[1])
xx_inv <- solve(t(x) %*% x)

#beta
B <- xx_inv %*% t(x) %*% y_m
y_pre <- x %*% B
eps <- y_pre-y_m
#residual
result1_1 <- cbind.data.frame(y, y_pre, eps,x1)
colnames(result1_1) <- c("y", "y_pre", "eps", 'x1')
#斜方差矩阵
ee <- t(eps) %*% eps
v <- (ee[1]/n-3)*xx_inv

ggplot(result1_1, aes(x=x1, y=y)) +
  geom_point(aes(color='skyblue'),show.legend = T) + 
  geom_point(aes(x=x1, y=y_pre,color = 'orange'), alpha = 0.3,show.legend = T) + 
  scale_colour_manual(values = c("orange","skyblue"), labels = c('y','predicted y'),name = ' ')+
  theme(plot.title = element_text(size=25, face="bold"),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18),
        legend.title=element_blank(),
        legend.text = element_text(size=18)) +
  labs(title =paste('Scatter plot of y and predicted y'))

#####1.2######
boot <- function(i){sample_n(dat,i,replace=TRUE)}
dat_50 <- boot(50)
dat_5000 <- boot(5000)

#dat_50
x <- data.matrix(dat_50[2:4])
y_m <- data.matrix(dat_50[1])
xx_inv <- solve(t(x) %*% x)
B_50 <- xx_inv %*% t(x) %*% y_m
y_pre_50 <- x %*% B_50
eps <- y_pre_50-y_m
#residual
result1_2_50 <- cbind.data.frame(dat_50[1], y_pre_50, eps)
colnames(result1_2_50) <- c("y", "y_pre", "eps")
#斜方差矩阵
ee <- t(eps) %*% eps
v_50 <- (ee[1]/50-3)*xx_inv
mean(B_50)

#dat_5000
x <- data.matrix(dat_5000[2:4])
y_m <- data.matrix(dat_5000[1])
xx_inv <- solve(t(x) %*% x)
B_5000 <- xx_inv %*% t(x) %*% y_m
y_pre_5000 <- x %*% B_5000
eps <- y_pre_5000-y_m
#residual
result1_2_5000 <- cbind.data.frame(dat_5000[1], y_pre_5000, eps)
colnames(result1_2_5000) <- c("y", "y_pre", "eps")
#斜方差矩阵
ee <- t(eps) %*% eps
v_5000 <- (ee[1]/5000-3)*xx_inv
mean(B_5000)

#######2######
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mar=c(3,3,1,1), mgp=c(2,0.2,0), tcl=-0.2)
#####2.1######
x <- data.matrix(dat[2:4])
y_m <- data.matrix(dat[1])
xx_inv <- solve(t(x) %*% x)
B <- xx_inv %*% t(x) %*% y_m
y_pre <- x %*% B
eps <- y_pre-y_m
plot(c(1:1000), eps, main="Residual Plot", 
     xlab="Index", ylab='Residual', col = 'skyblue')
abline(h=0, col="orange",lwd=2,lty=2)
abline(h=2*sd(eps), col="orange",lwd=2,lty=2)
abline(h=(-2)*sd(eps), col="orange",lwd=2,lty=2)
#####2.2######
t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  invisible(t.col)
}

hist(eps, main='Histogram of Residual and Error'
     ,col=t_col("mediumpurple3", perc = 50, name = ""), border=F, breaks=8)
hist(e, col=t_col("yellow", perc = 50, name = ""), add=T, border=F, breaks=4)

#####2.3######
set.seed(2020270026)
GDP <- rnorm(35, mean=5000, sd=50)
cols <- heat.colors(35, alpha = 1)[order(GDP)]
cnMap <- read_sf('https://geo.datav.aliyun.com/areas_v2/bound/100000_full.json') 
st_crs(cnMap) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
mapObj <- st_geometry(cnMap)
ext <- extent(cnMap)
plot(mapObj, xlim=c(ext[1],ext[2]), ylim=c(ext[3], ext[4]), border="gray80", col=cols, lwd=0.5)

#######3######
attach(Auto)
str(Auto)

#corr
wrap_1<-wrap(ggally_points,size=2,color="mediumpurple2",alpha=0.3)
wrap_2<-wrap(ggally_densityDiag,size=2,color="skyblue")
wrap_3 <- wrap(ggally_cor, size = 40, color = "darkgrey", fontface = "bold")
ggpairs(Auto[,1:8],
        lower = list(continuous = wrap_1),
        diag = list(continuous = wrap_2),
        higher = list(continuous = wrap_3))

cor(Auto[,1:8])
cor(Auto[,1:8])[1,]

#fit model
lm.fit <- lm(mpg~.,data=Auto[,1:8])
summary(lm.fit)
summary(lm.fit)$coefficient

model.diag.metrics <- augment(lm.fit)
model.diag.metrics <- as.data.frame(model.diag.metrics)
model.diag.metrics$.rownames <- as.numeric(model.diag.metrics$.rownames)
str(model.diag.metrics)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(x=.rownames, y=mpg)) +
  geom_point(aes(color='skyblue'),show.legend = T) + 
  geom_point(aes(x=.rownames, y=.fitted,color = 'orange'), alpha = 0.3,show.legend = T) + 
  scale_colour_manual(values = c("orange","skyblue"), labels = c('y','predicted y'),name = ' ')+
  theme(plot.title = element_text(size=25, face="bold"),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=18),
        legend.title=element_blank(),
        legend.text = element_text(size=18)) +
  labs(title =paste('Scatter plot of mpg and predicted mpg'))

#Diagnostic plots
autoplot(lm.fit) #超略微右偏
# Cook's distance
plot(lm.fit, 4) #若大于1，我们就认为这个点是异常点。也有人把这个阈值设置为4/n
#library(lindia)
#gg_diagnose(lm.fit)

#######4######
attach(Boston)
str(Boston)
#x = dis, y = nox
dataB <- Boston[,c(5,8)]
model <- lm(nox ~ poly(dis,3),data = dataB)
summary(model)
confint(model, level=0.95)

predicted.intervals <- predict(model,data.frame(x=dis),interval='confidence',
                               level=0.99)
plot(dataB$dis, dataB$nox, main=paste("Regression and C.I Poly = ", 3), 
     xlab="dis", ylab='nox', col = 'skyblue')
myPredict <- predict(model , interval="confidence",level=0.95)
ix <- sort(dis,index.return=T)$ix
lines(dis[ix], myPredict[ix , 1], col='deepskyblue4', lwd=2 )
polygon(c(rev(dis[ix]), dis[ix]), c(rev(myPredict[ ix,3]), myPredict[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
legend('topright',c("Regression Model","95% confidence"), 
       col=c("deepskyblue4","gray"), lwd=3)

#1~5
par(mfrow=c(2,3))
model1 <- lm(nox ~ poly(dis,1),data = dataB)
model2 <- lm(nox ~ poly(dis,2),data = dataB)
model3 <- lm(nox ~ poly(dis,3),data = dataB)
model4 <- lm(nox ~ poly(dis,4),data = dataB)
model5 <- lm(nox ~ poly(dis,5),data = dataB)

lmpoly <- function(model,k){
  predicted.intervals <- predict(model,data.frame(x=dis),interval='confidence',
                                 level=0.99)
  plot(dataB$dis, dataB$nox, main=paste("Regression and C.I Poly = ", k), 
       xlab="dis", ylab='nox', col = 'skyblue')
  myPredict <- predict(model , interval="confidence",level=0.95)
  ix <- sort(dis,index.return=T)$ix
  lines(dis[ix], myPredict[ix , 1], col='deepskyblue4', lwd=2 )
  polygon(c(rev(dis[ix]), dis[ix]), c(rev(myPredict[ ix,3]), myPredict[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  legend('topright',c("Regression Model","95% confidence"), 
         col=c("deepskyblue4","gray"), lwd=3)
}

lmpoly(model1,1)
lmpoly(model2,2)
lmpoly(model3,3)
lmpoly(model4,4)
lmpoly(model5,5)

#RSS
deviance(model1)
deviance(model2)
deviance(model3)
deviance(model4)
deviance(model5)

#choose model
train.control <- trainControl(method = "LOOCV")
model1 <- train(nox ~ poly(dis,1), data = dataB, method = "lm",
               trControl = train.control)
model2 <- train(nox ~ poly(dis,2), data = dataB, method = "lm",
                trControl = train.control)
model3 <- train(nox ~ poly(dis,3), data = dataB, method = "lm",
                trControl = train.control)
model4 <- train(nox ~ poly(dis,4), data = dataB, method = "lm",
                trControl = train.control)
model5 <- train(nox ~ poly(dis,5), data = dataB, method = "lm",
                trControl = train.control)
CV_d <- bind_rows(model1$results, model2$results,model3$results, 
          model4$results,model5$results)[,2:4]
CV_d$poly <- c(1:5)
#sp
fit1<-smooth.spline(dataB$dis,dataB$nox,df=12) 
fit2<-smooth.spline(dataB$dis,dataB$nox,df=13) 
fit3<-smooth.spline(dataB$dis,dataB$nox,df=14) 
plot(dataB$dis, dataB$nox, main=paste('Smoothing Spline with different df'), 
     xlab="dis", ylab='nox', col = 'skyblue')
lines(fit3,col="brown",lwd=2)
lines(fit2,col="darkgreen",lwd=2)
lines(fit1,col="deepskyblue4",lwd=2)
legend("topright",c("df=12",'df=13','df=14'),col=c("deepskyblue4","darkgreen",'brown'),lwd=2)


