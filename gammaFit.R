aggFreq <- 60*24
pa1 <- head(sliderWindowMean(salesOrderBlocked$ca, aggFreq,  salesOrderBlocked$selling_price), -1)
pa2 <- head(sliderWindowMean(salesOrderHealthy$ca, aggFreq,  salesOrderHealthy$selling_price), -1)

pasum <- as.data.frame(cbind(na.locf(pa1, fromLast = TRUE), na.locf(pa2, fromLast = TRUE)))

pasumRegularize <- as.data.frame(cbind(na.locf(pasum$..1, fromLast = TRUE), na.locf(pasum$..2, fromLast = TRUE))) 
colnames(pasumRegularize) <- c("Fraud", "Healthy")
pasumRegularize$time <- as.POSIXct(rownames(pasum))

plot(ecdf(pasumRegularize$Healthy))
plot(ecdf(pasumRegularize$Fraud))

# For each customer look at the average time inbetween transaction

library(plyr)

sales_order_agg1_30plus <- subset(sales_order_agg1, sales_order_agg1$custom_label == "30 or More Purchases" )

sales_order_agg1_healthy <- subset(sales_order_agg1, sales_order_agg1$fraud_label != 1 & sales_order_agg1$fraud_label != 2)
sales_order_agg1_fraud <- subset(sales_order_agg1, sales_order_agg1$fraud_label == 1 | sales_order_agg1$fraud_label == 2)
sales_order_agg1_nonFreq <- subset(sales_order_agg1_healthy, !sales_order_agg1_healthy$isFrequent)
sales_order_agg1_Freq <- subset(sales_order_agg1_healthy, sales_order_agg1_healthy$isFrequent)

###

ttAll <- sliderWindowCount(sales_order_agg1_healthy$ca, aggFreq,  sales_order_agg1_healthy$selling_price)
ttFraud <- sliderWindowCount(sales_order_agg1_fraud$ca, aggFreq,  sales_order_agg1_fraud$selling_price)
ttNonFreq <- sliderWindowCount(sales_order_agg1_nonFreq$ca, aggFreq, sales_order_agg1_nonFreq$selling_price)
ttFreq <- sliderWindowCount(sales_order_agg1_Freq$ca, aggFreq, sales_order_agg1_Freq$selling_price)

sales_order_agg1_30plus <- subset(sales_order_agg1, sales_order_agg1$custom_label == "30 or More Purchases" )
sales_order_agg1_30less <- subset(sales_order_agg1, sales_order_agg1$custom_label == "30 Less Purchases" )

ti30plus <- ddply(sales_order_agg1_30plus,~customer_id,summarise, meanDiffTime=mean(diff(ca)))
ti30less <- ddply(sales_order_agg1_30less,~customer_id,summarise, meanDiffTime=mean(diff(ca)))


## Fit Gamma Mixture

HFTTime <- ddply(sales_order_agg1_Freq, ~customer_id, summarise, meanDiffTime = mean(diff(ca)), HFTCount = length(ca))
RTTime <- ddply(sales_order_agg1_nonFreq, ~customer_id, summarise, meanDiffTime = mean(diff(ca)), RTCount = length(ca))

cidSummary <- merge(HFTTime, RTTime, by = "customer_id")
cidSummary$HFratio <- cidSummary$HFTCount/cidSummary$RTCount
hist(log(cidSummary$HFratio), main = "Distribution of log(v)"
     , xlab = "v - HFT/RT Ratio"
     , breaks = 25)

HFTSpending <- ddply(sales_order_agg1_Freq,~customer_id,summarise,meanGMV=mean(as.numeric(as.character(selling_price))))
RTSpending <- ddply(sales_order_agg1_nonFreq,~customer_id,summarise,meanGMV=mean(as.numeric(as.character(selling_price))))

# As 

arrivTimeHist <- hist(log(as.numeric(as.character(sales_order_agg1$selling_price))), 
                      breaks = 40,
                      main = "Average Selling Price on All Transactions",
                      xlab = "log(GMV)")

ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[2:length(arrivTimeHist$counts)], 
                                   arrivTimeHist$counts[2:length(arrivTimeHist$counts)]))

startparam <- mixparam(mu = c(11), 
                       sigma = 2)

fit2 <- mix(ttAllHistDf, startparam, "gamma")
plot(fit2, xlab = "log(GMV)", main ="Average Selling Price on All Transactions")


kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]

arrivDat <- log(as.numeric(as.character(sales_order_agg1$selling_price)))
n <-  length(arrivDat)
n <- 50

sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)

expectedSimAllTrans <- sim1

## KS Test All Transaction Simulation

print(ks.test(expectedSimAllTrans, arrivDat))
hist(arrivDat, breaks = 40)
hist(expectedSimAllTrans, breaks = 40)




fitGammaTimeInbetween30Plus <- function(){
  arrivTimeHist <- hist(log(as.numeric(as.character(ti30plus$meanDiffTime))), breaks = 500,
                        main = "Average Time inbetween Transactions per Customer",
                        xlab = "log(Seconds)")
  
  ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[4:length(arrivTimeHist$counts)], 
                                     arrivTimeHist$counts[4:length(arrivTimeHist$counts)]))
  
  startparam <- mixparam(mu = c(1, 8.5,12.5), sigma = 2) 
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "log(seconds)", main ="Number of Time inbetween Arrivals per Customer")
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  kShape3 <- (fit2$parameters$mu[3]/fit2$parameters$sigma[3])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  thetaScale3 <- (fit2$parameters$sigma[3])^2/fit2$parameters$mu[3]
  
  n <-  length(ttAll[, 1])
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  sim3 <- rgamma(n, shape = kShape3, scale = thetaScale3)
  
  flags <- rmultinom(n, size=1, c(fit2$parameters$pi[1], 
                                  fit2$parameters$pi[2],
                                  fit2$parameters$pi[3]))
  
  flag1 <- rbinom(n,size=1,prob=fit2$parameters$pi[1])
  flag2 <- rbinom(n,size=1,prob=fit2$parameters$pi[2])
  flag3 <- rbinom(n,size=1,prob=fit2$parameters$pi[3])
  
  expectedSimAllTrans <- sim2*flags[2,] + sim1*flags[1,] + sim3*flags[3,]
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- log(as.numeric(as.character(ti30plus$meanDiffTime)))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat[which(is.finite(arrivDat))], expectedSimAllTrans, "K-S Test: Interarrival Time")
  
}
fitGammaTimeInbetween30Plus()

fitGammaTimeInbetweenHFT <- function(){
  arrivTimeHist <- hist(log(as.numeric(as.character(HFTTime$meanDiffTime))), breaks = 500,
                        main = "HFT Average Time inbetween Transactions per Customer",
                        xlab = "log(Seconds)")
  
  ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[4:length(arrivTimeHist$counts)], 
                                     arrivTimeHist$counts[4:length(arrivTimeHist$counts)]))
  
  startparam <- mixparam(mu = c(1, 8.5,12.5), sigma = 2) 
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "log(seconds)", main ="HFT Number of Time inbetween Arrivals per Customer")
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  kShape3 <- (fit2$parameters$mu[3]/fit2$parameters$sigma[3])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  thetaScale3 <- (fit2$parameters$sigma[3])^2/fit2$parameters$mu[3]
  
  n <-  length(ttAll[, 1])/500
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  sim3 <- rgamma(n, shape = kShape3, scale = thetaScale3)
  
  flags <- rmultinom(n, size=1, c(fit2$parameters$pi[1], 
                                  fit2$parameters$pi[2],
                                  fit2$parameters$pi[3]))
  
  flag1 <- rbinom(n,size=1,prob=fit2$parameters$pi[1])
  flag2 <- rbinom(n,size=1,prob=fit2$parameters$pi[2])
  flag3 <- rbinom(n,size=1,prob=fit2$parameters$pi[3])
  
  expectedSimAllTrans <- sim2*flags[2,] + sim1*flags[1,] + sim3*flags[3,]
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- log(as.numeric(as.character(ti30plus$meanDiffTime)))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 50)
  hist(expectedSimAllTrans, breaks = 50)
  
  graphKSTest(arrivDat[which(is.finite(arrivDat))], expectedSimAllTrans, "K-S Test: HFT Interarrival Time")
  
}
fitGammaTimeInbetween30Plus()

fitGammaTimeInbetweenRT <- function(){
  arrivTimeHist <- hist(log(as.numeric(as.character(RTTime$meanDiffTime))), breaks = 100,
                        main = "RT Average Time inbetween Transactions per Customer",
                        xlab = "log(Seconds)")
  
  ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[4:length(arrivTimeHist$counts)], 
                                     arrivTimeHist$counts[4:length(arrivTimeHist$counts)]))
  
  startparam <- mixparam(mu = c(1, 8.5,12.5), sigma = 2) 
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "log(seconds)", main ="RT Number of Time inbetween Arrivals per Customer")
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  kShape3 <- (fit2$parameters$mu[3]/fit2$parameters$sigma[3])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  thetaScale3 <- (fit2$parameters$sigma[3])^2/fit2$parameters$mu[3]
  
  n <-  length(ttAll[, 1])
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  sim3 <- rgamma(n, shape = kShape3, scale = thetaScale3)
  
  flags <- rmultinom(n, size=1, c(fit2$parameters$pi[1], 
                                  fit2$parameters$pi[2],
                                  fit2$parameters$pi[3]))
  
  flag1 <- rbinom(n,size=1,prob=fit2$parameters$pi[1])
  flag2 <- rbinom(n,size=1,prob=fit2$parameters$pi[2])
  flag3 <- rbinom(n,size=1,prob=fit2$parameters$pi[3])
  
  expectedSimAllTrans <- sim2*flags[2,] + sim1*flags[1,] + sim3*flags[3,]
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- log(as.numeric(as.character(RTTime$meanDiffTime)))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat[which(is.finite(arrivDat))], expectedSimAllTrans, "K-S Test: RT Interarrival Time")
  
}
fitGammaTimeInbetween30Plus()

fitGammaHFTGMV <- function(){

  arrivTimeHist <- hist(log(as.numeric(as.character(HFTSpending$meanGMV))), breaks = 40,
                        main = "Average GMV per Transaction per Customer",
                        xlab = "log(Seconds)")
  
  ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[1:length(arrivTimeHist$counts)], 
                                     arrivTimeHist$counts[1:length(arrivTimeHist$counts)]))
  
  startparam <- mixparam(mu = c(12), sigma = 2.5) 
  fit2 <- mix(ttAllHistDf, startparam, "norm")
  plot(fit2, xlab = "Log(Average GMV per Customer)", main ="HFT Probability Density of Gamma Mixture Model - Log(GMV per Customer)")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  
  arrivDat <- log(as.numeric(as.character(HFTSpending$meanGMV)))
  n <-  length(arrivDat)
  
  sim1 <- rnorm(n, fit2$parameters$mu[1], fit2$parameters$sigma[1]^2)
  
  expectedSimAllTrans <- sim1
  
  ## KS Test All Transaction Simulation
  
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 30)
  hist(expectedSimAllTrans, breaks = 30)
}
fitGammaHFTGMV()

fitGammaRTGMV() <- function(){
  arrivTimeHist <- hist(log(as.numeric(as.character(RTSpending$meanGMV))), breaks = 40,
                        main = "Average GMV per Transaction per Customer RT",
                        xlab = "log(Seconds)")
  
  ttAllHistDf <- as.data.frame(cbind(arrivTimeHist$breaks[1:length(arrivTimeHist$counts)], 
                                     arrivTimeHist$counts[1:length(arrivTimeHist$counts)]))
  
  
  
  startparam <- mixparam(mu = c(3, 12), sigma = 2.5) 
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "Log(Average GMV per Customer)", main ="RT Probability Density of Gamma Mixture Model - Log(GMV per Customer)")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  
  arrivDat <- log(as.numeric(as.character(HFTSpending$meanGMV)))
  n <-  length(arrivDat)
  
  sim1 <- rnorm(n, fit2$parameters$mu[1], fit2$parameters$sigma[1]^2)
  
  expectedSimAllTrans <- sim1
  
  ## KS Test All Transaction Simulation
  
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 30)
  hist(expectedSimAllTrans, breaks = 30)
}






hist(log(as.numeric(as.character(RTSpending$meanGMV))), breaks = 200,
     main = "Average Time GMV per Customer for RT",
     xlab = "log(Seconds)")



v1 <- ddply(sales_order_agg1_Freq,~customer_id,summarise,mean=mean(as.numeric(as.character(selling_price))))
v2 <- ddply(sales_order_agg1_nonFreq,~customer_id,summarise,mean=mean(as.numeric(as.character(selling_price))))




hist(log(as.numeric(as.character(ti30less$meanDiffTime))), breaks = 200,
     main = "Average Time inbetween Transactions per Customer",
     xlab = "log(Seconds)")






timeInbetweenTransaction <- ddply(sales_order_agg1_Freq,~customer_id,summarise, meanDiffTime=mean(diff(ca)))
hist(log(as.numeric(as.character(timeInbetweenTransaction$meanDiffTime))), 
     main = "Average Time inbetween Transactions per Customer",
     xlab = "log(Seconds)",
     breaks = 200)



