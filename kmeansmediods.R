#kmeans
set.seed(1234)
x <- rnorm(24, mean=rep(1:3, each=4), sd=0.2)
y <- rnorm(24, mean=rep(c(1,2,1), each=4), sd=0.2)
data <- data.frame(x, y)
plot(x, y, col="blue", pch=19, cex=1)
text(x+0.05, y+0.05, labels=as.character(1:24))
kmeansObj <- kmeans(data, centers=3)
names(kmeansObj)
kmeansObj$withinss
kmeansObj$cluster
plot(x, y, col=kmeansObj$cluster, pch=19, cex=1)
points(kmeansObj$centers, col=1:3, pch=4, cex=3, lwd=3)

#kmediods
library(cluster)
kmedoidObj <- pam(x=data, k=4) 
names(kmedoidObj)
kmedoidObj$objective
par(mfrow=c(2,2), mar=c(3,3,3,3))

for(i in 1:4){
  plot(x, y, col=kmedoidObj$clustering, pch=19, cex=1)
  points(kmedoidObj$medoids, col=1:4, pch=4, cex=3, lwd=3)
}