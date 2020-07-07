#import libraries
library(forecast)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(purrr)
library(cluster)
library(viridis)
library(scatterplot3d)
library(openair)
library(olsrr)
library(purrr)
library(cluster)

#vizualize the quantile plot
qqplot.data <- function (vec, string) {
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) + ggtitle(string)
}

#get the seasonal data
getseason <- function (data) {
  library(forcats)
  library(dplyr)
  data <- data %>%
    mutate(
      season = fct_collapse(
        .f = Month,
        Spring = c("September", "October", "November"),
        Summer = c("December", "January", "February"),
        Autumn = c("March", "April", "May"),
        Winter = c("June", "July", "August")))
  return(data)
}


anzac <- read.csv("*****************************************")
ash <- read.csv("***************************************")
ger <- read.csv("****************************************")
wash <- read.csv("***************************************")

#scaling the data
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}

anzacSquare <- sapply(anzac[,3:14], scale01)
ashburton <- sapply(ash[,3:13], scale01)
geraldine <- sapply(ger[,3:12], scale01)
washdyke <- sapply(wash[,3:11], scale01)


# fitting linear Regression
## Anzac Square


md1 <- lm(PM10 ~ ., data = anzac[,3:14])
summary(md1)
anova(md1)
qqplot.data(residuals(md1), "Normal QQplot for Anzac Square Residuals")
checkresiduals(md1)
corPlot(anzac[,-c(1,2)])
ols_vif_tol(md1)


## Ashburton


md2 <- lm(PM10 ~ ., data = ash[,3:13])
summary(md2)
anova(md2)
qqplot.data(residuals(md2), "Normal QQplot for Ashburton Residuals")
checkresiduals(md2)
corPlot(ash[,-c(1,2)])
ols_vif_tol(md2)


## Geraldine


md3 <- lm(PM10 ~ ., data = ger[,3:12])
summary(md3)
anova(md3)
qqplot.data(residuals(md3), "Normal QQplot for Geraldine Residuals")
checkresiduals(md3)
corPlot(ger[,-c(1,2)])
ols_vif_tol(md3)


## Washdyke


md4 <- lm(PM10 ~ ., data = wash[,3:11])
summary(md4)
anova(md4)
qqplot.data(residuals(md4), "Normal QQplot for Washdyke Residuals")
checkresiduals(md4)
corPlot(wash[,-c(1,2)])
ols_vif_tol(md4)


# fit K-Means clustering algorithm
## Anzac Square

avg_sil <- function(k) {
  km.res <- kmeans(anzacSquare, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(anzacSquare))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes", main = "Anzac Square")



set.seed(234561)
km1 <- kmeans(anzacSquare,4, iter.max = 20, nstart = 10)
km2 <- kmeans(anzacSquare,12, iter.max = 20, nstart = 10)

p1 <- fviz_cluster(km1, geom = "point", data = anzacSquare) + ggtitle("Anzac Square K = 4")
p2 <- fviz_cluster(km2, geom = "point", data = anzacSquare) + ggtitle("Anzac Square K = 12")
grid.arrange(p1, p2, nrow = 1)


## Ashburton


avg_sil <- function(k) {
  km.res <- kmeans(ashburton, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(ashburton))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes", main = "Ashburton")



wss <- function(k) {
  kmeans(ashburton, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




km1 <- kmeans(ashburton,4, iter.max = 15, nstart = 25)
km2 <- kmeans(ashburton,12, iter.max = 15, nstart = 25)

p1 <- fviz_cluster(km1, geom = "point", data = ashburton) + ggtitle("Ashburton K = 4")
p2 <- fviz_cluster(km2, geom = "point", data = ashburton) + ggtitle("Ashburton K = 12")
grid.arrange(p1, p2, nrow = 1)


## Geraldine

avg_sil <- function(k) {
  km.res <- kmeans(geraldine, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(geraldine))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes", main = "Geraldine")



km1 <- kmeans(geraldine,4, iter.max = 15, nstart = 25)
km2 <- kmeans(geraldine,12, iter.max = 15, nstart = 25)

p1 <- fviz_cluster(km1, geom = "point", data = geraldine) + ggtitle("Geraldine K = 4")
p2 <- fviz_cluster(km2, geom = "point", data = geraldine) + ggtitle("Geraldine K = 12")
grid.arrange(p1, p2, nrow = 1)


## washdyke

avg_sil <- function(k) {
  km.res <- kmeans(washdyke, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(washdyke))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes", main = "Washdyke")



km1 <- kmeans(washdyke,4, iter.max = 15, nstart = 25)
km2 <- kmeans(washdyke,12, iter.max = 15, nstart = 25)

p1 <- fviz_cluster(km1, geom = "point", data = washdyke) + ggtitle("Washdyke K = 4")
p2 <- fviz_cluster(km2, geom = "point", data = washdyke) + ggtitle("Washdyke K = 12")
grid.arrange(p1, p2, nrow = 1)


# fitting linear reagression after applying prinicpal component analysis for datasets from each region

## Anzac Square

anzac <- getseason(anzac)
anzacSquare <- as.data.frame(anzacSquare)
pcaanzac <- prcomp(anzacSquare[,-7])
barplot(pcaanzac$sdev^2, ylab = "Varainces", main = "Screeplot Anzac Square", xlab = "Principal Components")

scatterplot3d(x=pcaanzac$x[,1],y=pcaanzac$x[,2],z=pcaanzac$x[,3], color = viridis(4)[anzac$season], angle=55, pch = 19, xlab = "PCA 1", ylab = "PCA 2", zlab = "PCA3", main = "Anzac Square")
legend("right", legend = levels(anzac$season),
       col =  viridis(4), pch = 19)

summary(pcaanzac)
pcaanzac$rotation[,1:4]
pcaDataAnzac <- as.data.frame(pcaanzac$x[,1:4])
pcaDataAnzac$PM10 <- anzacSquare$PM10
lmAnzac <- lm(PM10 ~ ., pcaDataAnzac)
summary(lmAnzac)
anova(lmAnzac)
qqplot.data(residuals(lmAnzac), "Normal QQplot for Anzac Square PCA Resdiduals")
checkresiduals(lmAnzac)
ols_vif_tol(lmAnzac)


## Ashburton

ash <- getseason(ash)
ashburton <- as.data.frame(ashburton)
pcaAsh <- prcomp(ashburton[,-6])
barplot(pcaAsh$sdev^2, ylab = "Varainces", main = "Screeplot Ashburton", xlab = "Principal Components")

scatterplot3d(x=pcaAsh$x[,1],y=pcaAsh$x[,2],z=pcaAsh$x[,3], color = viridis(4)[ash$season], angle=20, pch = 19, xlab = "PCA 1", ylab = "PCA 2", zlab = "PCA3", main = "Ashburton")
legend("right", legend = levels(ash$season),
       col =  viridis(4), pch = 19)

summary(pcaAsh)
pcaAsh$rotation[,1:4]
pcaDataAsh <- as.data.frame(pcaAsh$x[,1:4])
pcaDataAsh$PM10 <- ashburton$PM10
lmAsh <- lm(PM10 ~ ., pcaDataAsh)
summary(lmAsh)
anova(lmAsh)
qqplot.data(residuals(lmAsh), "Normal QQplot for Ashburton PCA Residuals")
checkresiduals(lmAsh)
ols_vif_tol(lmAsh)


## Geraldine

ger <- getseason(ger)
geraldine <- as.data.frame(geraldine)
pcaGer <- prcomp(geraldine[,-7])
barplot(pcaGer$sdev^2, ylab = "Varainces", main = "Screeplot Geraldine", xlab = "Principal Components")

scatterplot3d(x=pcaGer$x[,1],y=pcaGer$x[,2],z=pcaGer$x[,3], color = viridis(4)[ger$season], angle=20, pch = 19, xlab = "PCA 1", ylab = "PCA 2", zlab = "PCA3", main = "Geraldine")
legend("right", legend = levels(ger$season),
       col =  viridis(4), pch = 19)

summary(pcaGer)
pcaGer$rotation[,1:3]
pcaDataGer <- as.data.frame(pcaGer$x[,1:3])
pcaDataGer$PM10 <- geraldine$PM10
lmGer <- lm(PM10 ~ ., pcaDataGer)
summary(lmGer)
anova(lmGer)
qqplot.data(residuals(lmGer), "Normal QQplot for Geraldine PCA Residuals")
checkresiduals(lmGer)
ols_vif_tol(lmGer)


## Washdyke

wash <- getseason(wash)
washdyke <- as.data.frame(washdyke)
pcaWash <- prcomp(washdyke[,-6])
barplot(pcaWash$sdev^2, ylab = "Varainces", main = "Screeplot Washdyke", xlab = "Principal Components")

scatterplot3d(x=pcaWash$x[,1],y=pcaWash$x[,2],z=pcaWash$x[,3], color = viridis(4)[wash$season], angle=90, pch = 19, xlab = "PCA 1", ylab = "PCA 2", zlab = "PCA3", main = "Washdyke")
legend("right", legend = levels(wash$season),
       col =  viridis(4), pch = 19)

summary(pcaWash)
pcaWash$rotation[,1:3]
pcaDataWash <- as.data.frame(pcaWash$x[,1:3])
pcaDataWash$PM10 <- washdyke$PM10
lmWash <- lm(PM10 ~ ., pcaDataWash)
summary(lmWash)
anova(lmWash)
qqplot.data(residuals(lmWash), "Normal QQplot for Washdyke PCA Residuals")
checkresiduals(lmWash)
ols_vif_tol(lmWash)








