library(psych)

# Sliding Window Function -------------------------------------------------

data.frame(value = tapply(cbind(salesOrderBlockedSingle$selling_price), list(cut(salesOrderBlockedSingle$ca, breaks="10 days")),sum))

library(heR.Misc)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
gmx <- function(x) {exp(mean(log(x)))}

sliderWindowSum <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), sum)))
}

sliderWindowCount <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), length)))
}

mostOccur <- function(inVec) {
  names(which.max(table(inVec)))
}

sliderWindowMode <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), mostOccur)))
}

sliderWindowMean <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), mean)))
}

sliderWindowGM <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), geometric.mean)))
}

sliderWindowMax <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), max)))
}

sliderWindowMin <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), min)))
}

sliderWindowSD <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), sd)))
}

tapply(salesOrderBlockedSingle, cycle(salesOrderBlockedSingle), FUN=mean)





# 5 Minute Aggregation Mean -----------------------------------------------


salesOrderBlockedSingle <- head(subset(sales_order_agg1, customer_id %in% c("28f505ca8dcf6cbbeb40bc0980a9d1fc3612bda7") ), -1)
salesOrderBlockedSingle2 <- head(subset(sales_order_agg1, customer_id %in% c("53ccc99a42c2646d71659cfef607510b6cc51a0f") ), -1)
salesOrderHF1 <- head(subset(sales_order_agg1, customer_id %in% c("004777b3f2e127ea73fac36b1b04cf1b81c2d048") ), -1)

# write.csv(sales_order_agg1, file = "sales_order_agg1.csv")

salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
salesOrderBlockedSingle2 <- salesOrderBlockedSingle2[order(salesOrderBlockedSingle2$ca),]

c1Hist <- sliderWindowSum(salesOrderBlockedSingle$ca, "720 mins",  salesOrderBlockedSingle$selling_price)
c2Hist <- sliderWindowSum(salesOrderBlockedSingle2$ca, "720 mins",  salesOrderBlockedSingle2$selling_price)

c1HistMode <- sliderWindowMode(salesOrderBlockedSingle$ca, "360 mins",  salesOrderBlockedSingle$selling_price_class)
c2HistMode <- sliderWindowMode(salesOrderBlockedSingle2$ca, "10 mins",  salesOrderBlockedSingle2$selling_price_class)


c1HistMode <- replace(c1HistMode, c1HistMode==0, "none")
c2HistMode <- replace(c2HistMode, c2HistMode==0, "none")

write.csv(c1HistMode, file = "c1HistMode.csv")
write.csv(c2HistMode, file = "c2HistMode.csv")




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
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  
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


# K-Means Clustering Assignment

kk <- kmeans(histsHealthy[,2:8], 3, nstart = 2000)
kk$centers

histsWithClusterAss <- cbind(histsHealthy, kk$cluster)
write.csv(histsWithClusterAss, file = "mkvHistsHealthyClusMulti_3.csv")

write.csv(kk$centers, file = "mkvHistsKMeansAssignment.csv")

library(DeducerExtras)
assignK <- applyModel(kk,histsTest[,2:8])
write.csv(assignK, file = "mkvHistsTestClusMulti_3.csv")

save(kk, file = "KMeansData_3.RData")

load("KMeansData_3.RData")



#Test Data

lengsTest <- c()
histsTest <- c() 
i <- 0


cid_test_train <- testSummaryDataEncoded$customer_id
summaryDataEncoded[which(summaryDataEncoded$customer_id %in% cid_test_train),]

testDataKMeans <- summaryDataEncoded[which(summaryDataEncoded$customer_id %in% cid_test_train),]


for(id in unique(testDataKMeans$customer_id)){
  salesOrderBlockedSingle <- subset(sales_order_agg1, customer_id %in% c(id) )
  print(i)
  # print(nrow(salesOrderBlockedSingle))
  lengsTest <- c(lengsTest, nrow(salesOrderBlockedSingle))
  
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  
  timeWindow <- "720 mins"
  
  c1HistSum <- sliderWindowSum(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistCount <- sliderWindowCount(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMean <- sliderWindowMean(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistGM <- sliderWindowGM(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMax <- sliderWindowMax(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistMin <- sliderWindowMin(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  c1HistSD <- sliderWindowSD(salesOrderBlockedSingle$ca, timeWindow,  salesOrderBlockedSingle$selling_price)
  
  histsTest <- rbind(hists, cbind(c1HistSum,
                              c1HistCount,
                              c1HistMean,
                              c1HistGM,
                              c1HistMax,
                              c1HistMin,
                              c1HistSD))
  i <- i+1
}


write.csv(histsTest, file = "mkvHistsHealthyMultiTest.csv")
write.csv(lengsTest, file = "mkvLengthsHealthyMultiTest.csv")

# K-Means Clustering Assignment

kk <- kmeans(hists, 3, nstart = 2000)
kk$centers


histsWithClusterAss <- cbind(hists, kk$cluster)
write.csv(histsWithClusterAss, file = "mkvHistsHealthyClusMulti.csv")

write.csv(kk$centers, file = "mkvHistsKMeansAssignment.csv")




histsWithClusterAss <- cbind(hists, kk$cluster)
write.csv(histsWithClusterAss, file = "mkvHistsHealthyClusMulti.csv")





# Sample Plot -------------------------------------------------------------

c3HistXs1 <- sliderWindowSum(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs2 <- sliderWindowCount(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs3 <- sliderWindowMean(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs4 <- sliderWindowGM(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs5 <- sliderWindowMax(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs6 <- sliderWindowMin(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))
c3HistXs7 <- sliderWindowSD(salesOrderHF1$ca, "12 hours",  as.numeric(as.character(salesOrderHF1$selling_price)))


View(data.frame(c3HistXs1$value, c3HistXs2$value, c3HistXs3$value, c3HistXs5$value, c3HistXs6$value, c3HistXs7$value))

write.csv(c1Hist, file = "c1Hist.csv")
write.csv(c2Hist, file = "c2Hist.csv")






par(mfrow=c(3,1))

plot.ts(c3HistXs1, xlab = "12 Hour Interval", ylab = "Sum GMV", main = "Sum GMV per 12 Hour Interval", type = "s")
plot.ts(c3HistXs2, xlab = "12 Hour Interval", ylab = "Count of Transactions", main = "Count Transactions per 12 Hour Interval", type = "s")
plot.ts(c3HistXs3, xlab = "12 Hour Interval", ylab = "Mean GMV", main = "Mean GMV per 12 Hour Interval", type = "s")
# plot.ts(c3HistXs4, xlab = "12 Hour Interval", ylab = "GMV", main = "Geometric Mean GMV per 12 Hour Interval", type = "s")
plot.ts(c3HistXs5, xlab = "12 Hour Interval", ylab = "Max GMV", main = " Max GMV per 12 Hour Interval", type = "s")
plot.ts(c3HistXs6, xlab = "12 Hour Interval", ylab = "Min GMV", main = "Min Sum GMV per 12 Hour Interval", type = "s")
plot.ts(c3HistXs7, xlab = "12 Hour Interval", ylab = "Standard Deviation GMV", main = "Standard Deviation GMV per 12 Hour Interval", type = "s")




uniCid <- unique(sales_order_agg1_Freq$customer_id)

aggDat <- c()

for (cid in uniCid){
  salesOrderBlockedSingle <- head(subset(sales_order_data, customer_id %in% c(cid) ), -1)
  salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$ca),]
  c1Hist <- sliderWindowSum(salesOrderBlockedSingle$ca, "10 mins",  salesOrderBlockedSingle$selling_price)
  aggDat <- rbind(aggDat, c1Hist)
}



# Unique List of Healthy Customers ----------------------------------------

uniCid <- unique(sales_order_agg1$customer_id)
