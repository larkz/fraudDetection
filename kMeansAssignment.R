# KMeans


# K-Means Clustering Assignment

kk <- kmeans(histsWallet[,2:8], 3, nstart = 2000)
kk$centers


histsWithClusterAss <- cbind(histsHealthy, kk$cluster)
write.csv(histsWithClusterAss, file = "mkvHistsHealthyClusMulti_3.csv")

write.csv(kk$centers, file = "mkvHistsKMeansAssignment.csv")
