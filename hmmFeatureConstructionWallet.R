# Load training and validation sets
library(DeducerExtras)

load("trainSummaryDataEncoded.RData")
load("testSummaryDataEncoded.RData")



# Get healthy transaction name

wallet_txn_agg1$ca <- as.POSIXct(wallet_txn_agg1$create_timestamp, "%m/%d/%y %H:%M")

healthyCust <- subset(summaryData, summaryData$custom_label != "Blocked")
cid_healthy_train <- trainSummaryDataEncoded$customer_id
healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]
trainHealthy <- healthyCust[which(healthyCust$customer_id %in% cid_healthy_train),]

## Wallet
lengs <- c()
histsWal <- c() 
i <- 0

for(id in unique(trainHealthy$customer_id)){
  wid <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(wid))
  
  wid <- wid[order(wid$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistCount <- sliderWindowCount(wid$ca, timeWindow,   as.numeric(as.character(wid$price)))
  c1HistMean <- sliderWindowMean(wid$ca, timeWindow,   as.numeric(as.character(wid$price)))
  c1HistGM <- sliderWindowGM(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistMax <- sliderWindowMax(wid$ca, timeWindow,   as.numeric(as.character(wid$price)))
  c1HistMin <- sliderWindowMin(wid$ca, timeWindow,   as.numeric(as.character(wid$price)))
  c1HistSD <- sliderWindowSD(wid$ca, timeWindow,   as.numeric(as.character(wid$price)))
  
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

# histMKt: wallet histories
# Run K means training
kMWal <- subset(histsWal, histsWal[,1] > 0)
kkw <- kmeans(kMWal[1:nrow(kMWal),1:7], 3, nstart = 1)
kkw$centers

save(kkw, file = "kMeansWalletTrain.RData")
write.csv(kkw$centers, file = "kMeansWalletTrain.csv")


# Assign to training data
kMWal0 <- subset(histsWal, histsWal[,1] == 0)
kMWal0[,ncol(kMWal0)+1] <- 0

write.csv(histsMktWithClusterAss, file = "mkvHistsHealthyClusMultiWal_3.csv")
write.csv(lengs, file = "mkvHistsHealthyClusMultiWalLengs_3.csv")
save(kkw, file = "kkw.RData")

assignKTrain <- applyModel(kkw, kMWal[1:400,1:7])

# Run K Means Assignment
histsWalWithClusterAss <- cbind(kMWal[1:400,], assignKTrain[,ncol(assignKTrain)])
write.csv(histsWalWithClusterAss, file = "mkvHistsHealthyClusMultiWal_3.csv")





# Generate Test Set


lengs <- c()
histsMktTest <- c() 
i <- 0


for(id in unique(testSummaryDataEncoded$customer_id)){
  wid <- subset(wallet_txn_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengs <- c(lengs, nrow(wid))
  
  wid <- wid[order(wid$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistCount <- sliderWindowCount(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistMean <- sliderWindowMean(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistGM <- sliderWindowGM(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistMax <- sliderWindowMax(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistMin <- sliderWindowMin(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  c1HistSD <- sliderWindowSD(wid$ca, timeWindow,  as.numeric(as.character(wid$price)))
  
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
assignK <- applyModel(kkw, histsMktTest[1:300,1:7])

n=8
colnames(assignK)[n] <- "k_class"
assignK <- cbind(assignK, histsWal$customer_id[1:300])
colnames(assignK)[n+1] <- "customer_id"
save(assignK, file = "assignKW.RData")

write.csv(assignK, file = "mkvHistsTestClusMultiWallet_3.csv")


