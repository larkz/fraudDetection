# Load training and validation sets

load("trainSummaryDataEncoded.RData")
load("testSummaryDataEncoded.RData")

# Create data set for marketplace

lengs <- c()
hists <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

for(id in unique(trainHealthy$customer_id)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  
  hists <- rbind(hists, cbind(c1HistSum,
                              c1HistCount,
                              c1HistMean,
                              c1HistGM,
                              c1HistMax,
                              c1HistMin,
                              c1HistSD))
  i <- i+1
}

lengs <- c()
hists <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

for(id in unique(trainHealthy$customer_id)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow, as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1e3)
  
  hists <- rbind(hists, cbind(c1HistSum,
                              c1HistCount,
                              c1HistMean,
                              c1HistGM,
                              c1HistMax,
                              c1HistMin,
                              c1HistSD))
  i <- i+1
}

write.csv(hists, file = "mkvHistsHealthyMulti.csv")
write.csv(lengs, file = "mkvLengthsHealthyMulti.csv")

histsHealthy <- read.table("mkvHistsHealthyMulti.csv", header = TRUE, sep = ",")
histsTest <- read.table("mkvHistsHealthyMultiTest.csv", header = TRUE, sep = ",")

kk <- kmeans(hists[1:2100,1:7], 3, nstart = 2000)
kk$centers

histsWithClusterAss <- cbind(hists[1:2100,], kk$cluster)

write.csv(histsWithClusterAss, file = "mkvHistsHealthyClusMulti_3.csv")
write.csv(kk$centers, file = "mkvHistsKMeansAssignment.csv")
# load("KMeansData_3.RData")


library(DeducerExtras)
assignK <- applyModel(kk,histsTest[,2:8])
write.csv(assignK, file = "mkvHistsTestClusMulti_3.csv")

save(kk, file = "KMeansData_3.RData")
save(assignK, file = "assignK.RData")



## Wallet data generation

lengs <- c()
histsWallet <- c() 
i <- 0

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")

cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(trainHealthy$customer_id)){
  ss <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(ss))
  
  ss <- ss[order(ss$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistCount <- sliderWindowCount(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistMean <- sliderWindowMean(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistGM <- sliderWindowGM(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistMax <- sliderWindowMax(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistMin <- sliderWindowMin(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  c1HistSD <- sliderWindowSD(ss$ca, timeWindow,  as.numeric(as.character(ss$price))/1e3)
  
  tempHist <- cbind(c1HistSum,
                    c1HistCount,
                    c1HistMean,
                    c1HistGM,
                    c1HistMax,
                    c1HistMin,
                    c1HistSD)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsWallet <- rbind(histsWallet, tempHist)
  i <- i+1
}

save(histsWallet, file = "mkvWalletTraining.RData")

# Run K means training
kkw <- kmeans(histsWallet, 3, nstart = 2000)
kkw$centers

save(kkw, file = "kMeansWalletTrain.RData")
write.csv(kkw$centers, file = "kMeansWalletTrain.csv")

# Run K Means Assignment
histsWalletWithClusterAss <- cbind(histsWallet, kkw$cluster)
write.csv(histsWalletWithClusterAss, file = "mkvHistsHealthyClusMultiWallet_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiWalletLengs_3.csv")
save(kkw, file = "kkw.RData")


# Generate Test Set Wallet

lengs <- c()
histsWallet <- c() 
i <- 0

wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(testDataWallet$customer_id)){
  salesOrderBlockedSingle <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  
   tempHist <- cbind(c1HistSum,
                                    c1HistCount,
                                    c1HistMean,
                                    c1HistGM,
                                    c1HistMax,
                                    c1HistMin,
                                    c1HistSD)
   tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
   colnames(tempHist)[ncol(tempHist)] <- "customer_id"
    histsWallet <- rbind(histsWallet, tempHist)
    i <- i+1
}

# Run K means training
kkw <- kmeans(histsWallet[1:10000,1:7], 4, nstart = 2000)
kkw$centers

save(kkw, file = "kMeansWalletTrain.RData")
write.csv(kkw$centers, file = "kMeansWalletTrain.csv")

# Run K Means Assignment
histsWalletWithClusterAss <- cbind(histsWallet, kkw$cluster)
write.csv(histsWalletWithClusterAss, file = "mkvHistsHealthyClusMultiWallet_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiWalletLengs_3.csv")
save(kkw, file = "kkw.RData")




library(DeducerExtras)
assignKW <- applyModel(kkw, histsWallet[1:1000,1:7])

colnames(assignKW)[8] <- "k_class"
assignKW <- cbind(assignKW, histsWallet$customer_id)
colnames(assignKW)[9] <- "customer_id"
save(assignKW, file = "assignKW.RData")



write.csv(assignKW, file = "mkvHistsTestClusMultiWallet_3.csv")


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
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price))/1000)
  
  tempHist <- cbind(c1HistSum,
                    c1HistCount,
                    c1HistMean,
                    c1HistGM,
                    c1HistMax,
                    c1HistMin,
                    c1HistSD)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsMkt <- rbind(histsMkt, tempHist)
  i <- i+1
}



save(histsMkt, file = "mkvMarketTraining.RData")

# Run K means training
kMTrain <- subset(histsMkt, histsMkt[,1] > 0)
kk <- kmeans(kMTrain[,1:7], 3, nstart = 2000)
kk$centers

save(kk, file = "kMeansMarketTrain.RData")
write.csv(kk$centers, file = "kMeansMarketTrain.csv")

# Run K Means Assignment
histsMktWithClusterAss <- cbind(histsMkt, kk$cluster)
write.csv(histsMktWithClusterAss, file = "mkvHistsHealthyClusMultiMkt_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiMktLengs_3.csv")
save(kk, file = "kk.RData")

# Generate Test Set Wallet

lengs <- c()
histsMkt <- c() 
i <- 0

# wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

for(id in unique(testSummaryDataEncoded$customer_id)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  
  tempHist <- cbind(c1HistSum,
                    c1HistCount,
                    c1HistMean,
                    c1HistGM,
                    c1HistMax,
                    c1HistMin,
                    c1HistSD)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsMkt <- rbind(histsMkt, tempHist)
  i <- i+1
}


library(DeducerExtras)
assignK <- applyModel(kk, histsMkt[,1:7])

colnames(assignK)[8] <- "k_class"
assignK <- cbind(assignK, histsMkt$customer_id)
colnames(assignK)[9] <- "customer_id"
save(assignK, file = "assignK.RData")

write.csv(assignK, file = "mkvHistsTestClusMultiMarket_3.csv")


