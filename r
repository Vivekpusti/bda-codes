install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")


library("ggplot2")
library("dplyr")
library("ggfortify")

summary(iris)
head(iris)
data <- select(iris, c(1:4))

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data)

kmean <- kmeans(data, 2)
kmean$centers

autoplot(kmean, data, frame = TRUE)

kmean <- kmeans(data, 3)
kmean$centers







library(ggplot2)
data(iris)
head(iris)

irisScale = scale(iris[,-5])
head(irisScale)

ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()

ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

K.max = 10 
wss = rep(NA, K.max = 10) 

for(i in 1:K.max){
 irisclasses= kmeans(irisScale,i) 
 wss[i] = irisclasses$tot.withinss 
 nClust[[i]] = irisclasses$size 
}

plot(1:K.max, wss, 
 type="b",pch = 19, 
 xlab= "Number of clusters",
 ylab="between ss/ total ss")
 
fitK = kmeans(irisScale, 3)
str(fitK)

plot(iris,col = fitK$cluster)

table(Predicted=fitK$cluster,Actual =iris$Species)

50 + 39 + 36
11 + 14

125/(125+25)
