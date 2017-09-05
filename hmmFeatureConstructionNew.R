# Load training and validation sets
load("trainSummaryDataEncoded.RData")
load("testSummaryDataEncoded.RData")

# Marketplace Construction

lengs <- c()
histsMkt <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

# wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(trainHealthy$customer_id)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  log(as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000))

  tempHist <- cbind(c1HistSum)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistSum)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsMkt <- rbind(histsMkt, tempHist)
  i <- i+1
}
save(histsMkt, file = "mkvMarketTraining.RData")


### Test Set Construction
lengs <- c()
histsMktTest <- c() 
i <- 0

cid_test <- testSummaryDataEncoded$customer_id
for(id in unique(cid_test)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  
  tempHist <- cbind(c1HistSum)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistSum)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsMktTest <- rbind(histsMktTest, tempHist)
  i <- i+1
}
save(histsMktTest, file = "mkvMarketTest.RData")


# Marketplace Testing Assignment

library(DeducerExtras)
assignKTest <- applyModel(kk, histsMkt)

colnames(assignKTest)[3] <- "k_class"
assignKTest <- cbind(histsMktWithClusterAss)
save(assignK, file = "assignK.RData")

write.csv(assignKTest, file = "mkvHistsTestClusMultiMarketTest_3.csv")


#### WALLET

# Wallet training data
lengs <- c()
histsWallet <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(trainHealthy$customer_id)){
  salesOrderBlockedSingle <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$txn_amount)))
  

  tempHist <- cbind(c1HistSum)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistSum)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  
  histsWallet <- rbind(histsWallet, tempHist)
  i <- i+1
}

save(histsWallet, file = "mkvWalletTraining.RData")

# Run K means training
kkw <- kmeans(histsWallet[,1], 3, nstart = 2000)
kkw$centers

save(kkw, file = "kMeansWalletTrain.RData")
write.csv(kkw$centers, file = "kMeansWalletTrain.csv")

# Run K Means Assignment
histsWalletWithClusterAss <- cbind(histsWallet, kkw$cluster)
write.csv(histsWalletWithClusterAss, file = "mkvHistsHealthyClusMultiWallet_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiWalletLengs_3.csv")
save(kkw, file = "kkw.RData")


library(DeducerExtras)
assignK <- applyModel(kkw, histsWalletWithClusterAss[,1:1])

colnames(assignK)[3] <- "k_class"
assignK <- cbind(histsMktWithClusterAss)
save(assignK, file = "assignK.RData")

write.csv(assignK, file = "mkvHistsTestClusMultiMarket_3.csv")

















########## WALLET

lengs <- c()
histsWal <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

# wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(trainHealthy$customer_id)){
  salesOrderBlockedSingle <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$price)))
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$price)))
  
  tempHist <- cbind(c1HistSum,
                    c1HistCount,
                    c1HistMean,
                    c1HistGM,
                    c1HistMax,
                    c1HistMin,
                    c1HistSD)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsWal <- rbind(histsWal, tempHist)
  i <- i+1
}
save(histsWal, file = "mkvWalletTraining.RData")

# Run K means training
histsWalSample <- subset(histsWal, histsWal$customer_id == "9b98109eabb29b676ad6e39e88d26f623b2f5e83")

kMTrainWal <- subset(histsWal, histsWal[,1] > 0)
kkw <- kmeans(kMTrainWal[,1:7], 4, nstart = 2000)
kkw$centers


library(DeducerExtras)
assignK <- applyModel(kkw, histsWal[,1:7])

colnames(assignK)[3] <- "k_class"
assignK <- cbind(histsMktWithClusterAss)
save(assignK, file = "assignK.RData")






