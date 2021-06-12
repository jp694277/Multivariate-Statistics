library(readxl)
library(MASS)
library(showtext)
library("FactoMineR")
showtext.auto(enable = TRUE)
font.add('SimSun')

# read file
df <- read_excel("ex8.5.xls")
a <- df$区县名
df <- df[,-1]
rownames(df) <- a
str(df)

# chi square test
chisq.test(df) #不獨立

# corresp
co <- corresp(df, nf=2)
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
    geom_text_repel(data = rscore, color="black") + geom_text_repel(data = cscore, color="red") + geom_hline(yintercept=0, linetype=3, col="gray") + geom_vline(xintercept=0, linetype=3, col="gray") + labs(x=NULL, y=NULL)
  p }

ggbiplot.corresp(co)

# star
stars(df, len=0.9, cex=0.8, key.loc=c(12,2.2),draw.segments=TRUE,
      labels = paste(row.names(df)))

# face
library(aplpack)
aplpack::faces(df,cex = 2.5)

