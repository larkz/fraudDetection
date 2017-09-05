set.seed(1444)

kMWal <- subset(histsWal, histsWal > 0)

set.seed(7)
dat = attitude[,c(3,4)]
km1 = kmeans(dat, 2, nstart=100)


# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)


set.seed(333)
df = kMWal[complete.cases(kMWal),][1:20,1:7]*1000


df <- dat
library(DeducerExtras)
dfCluster<-kmeans(df, 2, nstart=100)
cent <- list(dfCluster$centers)
ass <- list(applyModel(dfCluster, df))
print(ass)
plot(dat, col =(dfCluster$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

 
max_iter = 5
for (i in 2:max_iter){
  dfCluster <- kmeans(df, centers = 2, iter.max = i, nstart =100)
  cent[[i]] <- dfCluster$centers
  
  
  ass[[i]] <- applyModel(dfCluster, df)
  plot(dat, col =(dfCluster$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)
  
  # print(ass[[i]])
}
cent

sum(ass[[2]] - ass[[1]])

########
set.seed(1337)
df = iris[,1:2]

dfCluster<-kmeans(df,centers=3, iter.max = 1)
plot(df[,1], df[,2], col=dfCluster$cluster,pch=19,cex=0.5, main="iter 1")
points(dfCluster$centers,col=1:5,pch=3,cex=3,lwd=3)
print(dfCluster$centers)

max_iter = 10

for (i in 2:max_iter){

  plot(df[,1], df[,2], col=dfCluster$cluster,pch=19,cex=0.5, main=paste("iter",i))
  points(dfCluster$centers,col=1:5,pch=3,cex=3,lwd=3)

  print(dfCluster$centers)
}