#### Bimodal Distribution

sliderWindowCount <- function(timeVal, perSecs, vals){
  library(xts)
  xData <- xts(as.numeric(as.character(vals)), timeVal)
  period.apply(xData, endpoints(xData, "seconds", perSecs), nrow)
}

sliderWindowSum <- function(timeVal, freqSecs, vals){
  library(xts)
  xData <- xts(as.numeric(as.character(vals)), timeVal)
  period.apply(xData, endpoints(xData, "seconds", freqSecs), colSums)
}

### Histogram on All Healthy transactions, Healthy vs unhealthy
aggFreq <- 60*24*7

# sales_order_agg1_30plus <- subset(sales_order_agg1, sales_order_agg1$custom_label == "30 or More Purchases" )

sales_order_agg1_healthy <- subset(sales_order_agg1, sales_order_agg1$fraud_label != 1 & sales_order_agg1$fraud_label != 2)
sales_order_agg1_fraud <- subset(sales_order_agg1, sales_order_agg1$fraud_label == 1 | sales_order_agg1$fraud_label == 2)
sales_order_agg1_nonFreq <- subset(sales_order_agg1_healthy, !sales_order_agg1_healthy$isFrequent)
sales_order_agg1_Freq <- subset(sales_order_agg1_healthy, sales_order_agg1_healthy$isFrequent)

###

ttAll <- sliderWindowCount(sales_order_agg1_healthy$ca, aggFreq, sales_order_agg1_healthy$selling_price)
ttFraud <- sliderWindowCount(sales_order_agg1_fraud$ca, aggFreq, sales_order_agg1_fraud$selling_price)
ttNonFreq <- sliderWindowCount(sales_order_agg1_nonFreq$ca, aggFreq, sales_order_agg1_nonFreq$selling_price)
ttFreq <- sliderWindowCount(sales_order_agg1_Freq$ca, aggFreq, sales_order_agg1_Freq$selling_price)

ssAll<- sliderWindowSum(sales_order_agg1_healthy$ca, aggFreq, sales_order_agg1_healthy$selling_price)

library(ggplot2)
ttAll <- as.data.frame( as.numeric(as.character(ttAll[,1])) )
ttFraud <- as.data.frame( as.numeric(as.character(ttFraud[,1])) )

colnames(ttAll) <- c("numArriv")

qplot(numArriv, 
      data=ttAll,
      geom="histogram", 
      binwidth=10,
      xlab = "Number of Arrivals per Week",
      xlim = c(0, 500))


# Fit Mixture Model -------------------------------------------------------


library(mixtools)
library(mixdist)

healthyMixturePlot <- function(){
  wait = as.numeric(as.character(ttAll[,1]))/length(unique(sales_order_agg1_healthy$customer_id))
  
  nBreaks <- 200
  ttAllHist <- hist(wait, breaks = nBreaks)
  
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  startparam <- mixparam(mu = c(0.01,0.037), sigma = 0.03) 
  
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(ttAllHistDf)
  
  plot(fit2, xlab = "Number of Arrivals Per Week per 1 Customer", main ="Probability Density of Gamma Mixture Model - Weekly Arrivals")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  
  x <- seq(0, 400, length=200)
  plot(x, dgamma( sort(x) , shape = kShape1 , scale = thetaScale1 )*fit2$parameters$pi[1], type ="l")
  plot(x, dgamma( sort(x) , shape = kShape2 , scale = thetaScale2 )*fit2$parameters$pi[2], type ="l")
  
  
  ### Generate Mixture Model
  n <-  length(ttAll[, 1])
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  probMix1 <- fit2$parameters$pi[1]
  flag <- rbinom(n,size=1,prob=probMix1)
  
  d <- density(sim2, breaks = 1000)
  expectedSimAllTrans <- sim2*(1 - flag) + sim1*flag
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- as.numeric(as.character(wait))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat, expectedSimAllTrans, "K-S Test: Weekly Arrivals")
}
healthyMixturePlot()

fraudMixturePlot <- function(){
  
  wait = as.numeric(as.character(ttFraud[,1]))/length(unique(sales_order_agg1_fraud$customer_id))
  # mixmdl = poisregmixEM(as.vector(ttAllHist$counts)[1:215] , as.matrix(ttAllHist$breaks)[1:215] )
  
  nBreaks <- 40
  ttAllHist <- hist(wait, breaks = nBreaks)
  
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  startparam <- mixparam(mu = c(0.02), sigma = 0.05) 
  
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "Number of Fraudulent Arrivals Per Week per 1 Customer", main ="Probability Density of Gamma Mixture Model (Fraud) - Weekly Arrivals")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  
  x <- seq(0, 1, length=200)
  # plot(x, dgamma( sort(x) , shape = kShape1 , scale = thetaScale1 )*fit2$parameters$pi[1], type ="l")
  # plot(x, dgamma( sort(x) , shape = kShape2 , scale = thetaScale2 )*fit2$parameters$pi[2], type ="l")
  
  ### Generate Mixture Model
  n <-  length(wait)
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  probMix1 <- fit2$parameters$pi[1]
  flag <- rbinom(n,size=1,prob=probMix1)
  
  expectedSimAllTrans <- sim1
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- as.numeric(as.character(wait))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat, expectedSimAllTrans, "K-S Test: Weekly Fraud Arrivals")
  
}
fraudMixturePlot()

### Segment by Recharge Non-Recharge
### Visualize Bimodal Distribution
# Fit GMM Model

regTransMixturePlot <- function(){
  wait = as.numeric(as.character(ttNonFreq[,1]))/length(unique(sales_order_agg1_healthy$customer_id))
  
  nBreaks <- 160
  ttAllHist <- hist(wait, breaks = nBreaks)
  
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  plot(ttAllHistDf)
  
  startparam <- mixparam(mu = c(0.001,0.007), sigma = 0.005) 
  
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "Number of Arrivals Per Week Regular Transactions", main ="Probability Density of Gamma Mixture Model - Weekly Arrivals")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  
  x <- seq(0, 400, length=200)
  plot(x, dgamma( sort(x) , shape = kShape1 , scale = thetaScale1 ), type ="l")
  plot(x, dgamma( sort(x) , shape = kShape2 , scale = thetaScale2 ), type ="l")
  
  # Simulate and KS Test
  
  n <-  length(ttNonFreq[, 1])*100
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  probMix1 <- fit2$parameters$pi[1]
  flag <- rbinom(n, size=1, prob=probMix1)
  
  hist(sim2, breaks = 300)
  hist(sim1, breaks = 300)
  
  d <- density(sim2, breaks = 1000)
  
  expectedSimAllTrans <- sim2*(1 - flag) + sim1*flag
  plot(density(expectedSimAllTrans))
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- as.numeric(as.character(wait))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat, expectedSimAllTrans, "K-S Test: Weekly Regular Transaction Arrivals")
  
}
regTransMixturePlot()

HFTMixturePlot <- function(){
  wait = as.numeric(as.character(ttFreq[,1]))/length(unique(sales_order_agg1_healthy$customer_id))
  
  nBreaks <- 350
  ttAllHist <- hist(wait, breaks = nBreaks)
  
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  plot(ttAllHistDf)
  
  startparam <- mixparam(mu = c(0.001,0.03), sigma = 0.01) 
  
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(fit2, xlab = "Number of Arrivals Per Week High Frequency Transactions", main ="Probability Density of Gamma Mixture Model - Weekly Arrivals")
  
  
  kShape1 <- (fit2$parameters$mu[1]/fit2$parameters$sigma[1])^2
  kShape2 <- (fit2$parameters$mu[2]/fit2$parameters$sigma[2])^2
  
  thetaScale1 <- (fit2$parameters$sigma[1])^2/fit2$parameters$mu[1]
  thetaScale2 <- (fit2$parameters$sigma[2])^2/fit2$parameters$mu[2]
  
  # Simulate and KS Test
  
  n <-  length(ttNonFreq[, 1])*100
  sim1 <- rgamma(n, shape = kShape1, scale = thetaScale1)
  sim2 <- rgamma(n, shape = kShape2, scale = thetaScale2)
  probMix1 <- fit2$parameters$pi[1]
  flag <- rbinom(n, size=1, prob=probMix1)
  
  hist(sim2, breaks = 300)
  hist(sim1, breaks = 300)
  
  d <- density(sim2, breaks = 1000)
  
  expectedSimAllTrans <- sim2*(1 - flag) + sim1*flag
  plot(density(expectedSimAllTrans))
  
  ## KS Test All Transaction Simulation
  
  arrivDat <- as.numeric(as.character(wait))
  print(ks.test(expectedSimAllTrans, arrivDat))
  
  hist(arrivDat, breaks = 150)
  hist(expectedSimAllTrans, breaks = 150)
  
  graphKSTest(arrivDat, expectedSimAllTrans, "K-S Test: Weekly High Frequency Transaction Arrivals")
  
}
regTransMixturePlot()





