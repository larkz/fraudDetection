# Time Series Visualizations

library(ggplot2)
setwd("/users/eric/Google Drive 2/MASc/Enterprise/EnterpriseSimulation")
setwd("C:\\Users\\larkinliu\\Google Drive\\MASc\\Enterprise\\EnterpriseSimulation")
setwd("/users/admin/Google Drive/Enterprise/EnterpriseSimulation")


source("processGeneration/generativeFunctions.R")
source("processGeneration\\generativeFunctions.R")

# 15 Day Example

dayCount <- 15
all.secs <- seq(1, dayCount*24*3600, 1)
hLambda <- rep(1/3600, dayCount*24*3600)

# Step function plot of inter arrival times for hpp process
hppSample15 <- hppGenerate(dayCount, 1/(3600))
qplot(seq_along(hppSample15$posNH), hppSample15$posNH, geom="step") +
  xlab("Arrival Number") +
  ylab("Time (Seconds)") +
  ggtitle("Simulated Arrival Rates for Homogenous Poisson Process")


qplot(diff(hppSample15$posNH), geom="histogram", binwidth = 400) +
  xlab("Count") +
  ylab("Inter-Arrival Times") +
  ggtitle("Histogram of Simulated Arrival Times for Homogenous Poisson Process")


# Non-homogenous Poisson Distribution Plot

nh2 <- lambdaStepFuncGen(all.secs)
plot(nh2, type = "l")

# Simulated lambda function plot
qplot(seq_along(nh2), nh2, geom="step") +
  xlab("Time Interval") +
  ylab("Lambda") +
  ggtitle("Simulated Lambda Step Function with Respect to Time")


# interArrivSecs <- timeIntervalGeneratorSecs(all.secs)
interArrivSecs <- nhppGenerate(dayCount, nh2)
plot(interArrivSecs$posNH)

# Simulated interarrival seconds
qplot(seq_along(interArrivSecs$posNH), interArrivSecs$posNH, geom="point") +
  xlab("Arrival Number") +
  ylab("Time (Seconds)") +
  ggtitle("Simulated Inter-Arrival Times")

# Generate and plot transaction values
tsMean <- 50
tsSd <- 1.5
txnVal <- stochasticGenerateTransactions(interArrivSecs$posNH, tsMean, tsSd)
plot(txnVal)

qplot(seq_along(txnVal), txnVal, geom="line") +
  xlab("Transaction Number") +
  ylab("Transaction Amount") +
  ggtitle("Simulated Transaction Amounts")

# Merging transaction amounts with transaction 

qplot(interArrivSecs$posNH, txnVal, geom="line") +
  xlab("Time (Seconds)") +
  ylab("Transaction Amount") +
  ggtitle("Simulated Transaction Amounts") + 
  xlim(0, max(all.secs))


plot(interArrivSecs$posNH, txnVal, xlim = c(0, max(all.secs)), type = "l")




p <- ggplot() + xlim(c(0,30)) + geom_histogram(aes(x=ttAll$numArriv, y=..density..), binwidth = 1, colour="black", data=ttAll)  
p + geom_density(aes(x=ss$simPois), colour="blue", adjust=15, data=ss) + xlab("Number of Arrivals per Hour")


p + ggtitle("Number of Arrivals per Week and Parametric Poisson Fit")

p + xlab("Number of Arrivals per Day")
p + title("Number of Arrivals per Week and Parametric Poisson Fit")
p + xlim(c(0,70))


lambda = 0.77

total <- sum(ttNonFreqHist$counts)
expected <- total*dpois(0:(length(observed)-1), lambda = lambda)

ks.test(as.numeric(as.character(ttNonFreq[,1])), rpois(100, 3.2) )


expected <- total*dpois(ttNonFreqHist$breaks, lambda = 3.2)
expectedSim <-  rweibull(length(ttNonFreq), shape =1.22, scale = 2)
expected <- hist(expectedSim, breaks = max())$counts

plot(ttNonFreqHist$counts, type = "l", col = "blue")
lines(expected, type = "l", col = "red")

ks.test(expectedSim, as.numeric(as.character(ttNonFreq[,1])))

####

ttNonFreqHist
expected <-  total*dpois(ttNonFreqHist$breaks, 3.2)

goodness.of.fit = cbind(ttNonFreqHist$breaks, ttNonFreqHist$counts, expected)
colnames(goodness.of.fit) = c('Emissions', 'ObservedCounts', 'ExpectedCounts')
goodness.of.fit

#Probability Density Plot

plot(goodness.of.fit[,1], goodness.of.fit[,2], type = "l")
lines(goodness.of.fit[,1], goodness.of.fit[,3])

rawInterArriv <- as.numeric(as.character(ttNonFreq[,1]))

poissonFit <- fitdistr(rawInterArriv, densfun="poisson")

# Chi Square Test
ks.test(rawInterArriv, rpois(100, 3.5) )
br = 10
chisq.test(hist(rawInterArriv, breaks = br)$counts, hist(rpois(length(rawInterArriv), 2.5), breaks = br)$counts) 

