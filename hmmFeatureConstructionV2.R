# Load training and validation sets

load("trainSummaryDataEncoded.RData")
load("testSummaryDataEncoded.RData")


## Marketplace
lengs <- c()
histsMkt <- c() 
i <- 0

# Get healthy transaction name


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
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,   as.numeric(as.character(salesOrderBlockedSingle$selling_price)))
  
  tempHist <- cbind(c1HistSum,
                    c1HistCount,
                    c1HistMean,
                    c1HistMax,
                    c1HistMin,
                    c1HistSD)
  tempHist <- cbind(tempHist, rep(id, nrow(c1HistMean)))
  colnames(tempHist)[ncol(tempHist)] <- "customer_id"
  histsMkt <- rbind(histsMkt, tempHist)
  i <- i+1
}

# histMKt: Marketplace histories
# Run K means training
kMTrain <- subset(histsMkt, histsMkt[,1] > 0)
kk <- kmeans(kMTrain[1:400,1:7], 3, nstart = 2000)
kk$centers

save(kk, file = "kMeansMarketTrain.RData")
write.csv(kk$centers, file = "kMeansMarketTrain.csv")

# Run K Means Assignment
histsMktWithClusterAss <- cbind(histsMkt[1:400,], kk$cluster)
write.csv(histsMktWithClusterAss, file = "mkvHistsHealthyClusMultiMkt_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiMktLengs_3.csv")
save(kk, file = "kk.RData")


# Assign to training data
kMMkt0 <- subset(histsMkt, histsMkt[,1] == 0)
kMMkt0[,ncol(kMMkt0)+1] <- 0
assignKTrain <- applyModel(kk, kMTrain[1:400,1:7])



# Run K Means Assignment
histsWalWithClusterAss <- cbind(kMWal[1:400,], assignKTrain[,ncol(assignKTrain)])
write.csv(histsWalWithClusterAss, file = "mkvHistsHealthyClusMultiMkt_3.csv")


write.csv(histsMktWithClusterAss, file = "mkvHistsHealthyClusMultiWal_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiWalLengs_3.csv")
save(kkw, file = "kkw.RData")













# Generate Test Set


lengs <- c()
histsMktTest <- c() 
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
  histsMktTest <- rbind(histsMktTest, tempHist)
  i <- i+1
}

library(DeducerExtras)
assignK <- applyModel(kk, histsMktTest[,1:7])

colnames(assignK)[8] <- "k_class"
assignK <- cbind(assignK, histsMkt$customer_id)
colnames(assignK)[9] <- "customer_id"
save(assignK, file = "assignK.RData")

write.csv(assignK, file = "mkvHistsTestClusMultiMarket_3.csv")






















