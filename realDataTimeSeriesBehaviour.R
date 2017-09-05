library(zoo)
library(reshape2)


# Real Time Series Behaviour of Customers --------------------------------------

summaryDataBlocked <-  subset(summaryData, custom_label == "Blocked")

salesOrderBlocked <- subset(sales_order_data, customer_id %in% summaryDataBlocked$customer_id )
salesOrderHealthy <- subset(sales_order_data, !(customer_id %in% summaryDataBlocked$customer_id) )

# plot(salesOrderHealthy$created_at_secs, salesOrderBlocked$selling_proce, col = "Blue")



# Blocked Sample ----------------------------------------------------------

salesOrderBlockedSample <- subset(salesOrderBlocked, customer_id == "c0bc6f439cc91dc5f83cbefa2b9877b95b6240bc")



# Sliding Window Function -------------------------------------------------

sliderWindowMean <- function(timeVal, freqSecs, vals){
  library(xts)
  xData <- xts(as.numeric(as.character(vals)), timeVal)
  period.apply(xData, endpoints(xData, "seconds", freqSecs), colMeans)
}

sliderWindowCount <- function(timeVal, freqSecs, vals){
  library(xts)
  xData <- xts(as.numeric(as.character(vals)), timeVal)
  period.apply(xData, endpoints(xData, "seconds", freqSecs), nrow)
}


# 5 Minute Aggregation Mean -----------------------------------------------

aggFreq <- 60*1
pa1 <- head(sliderWindowMean(salesOrderBlocked$ca, aggFreq,  salesOrderBlocked$selling_price), -1)
pa2 <- head(sliderWindowMean(salesOrderHealthy$ca, aggFreq,  salesOrderHealthy$selling_price), -1)

pasum <- as.data.frame(cbind(na.locf(pa1, fromLast = TRUE), na.locf(pa2, fromLast = TRUE)))

pasumRegularize <- as.data.frame(cbind(na.locf(pasum$..1, fromLast = TRUE), na.locf(pasum$..2, fromLast = TRUE))) 
colnames(pasumRegularize) <- c("Fraud", "Healthy")
pasumRegularize$time <- as.POSIXct(rownames(pasum))

# ts.plot(pasumRegularize,gpars= list(col=rainbow(10)))

pasumGG <- melt(pasumRegularize, id = c("time"))

p <- ggplot(data=pasumGG, aes(x=time, y=value, group=variable))
p + geom_line(aes(color = variable)) + 
  labs(title = "5 Minute Time Window Aggregation") +
  xlab("Time (Seconds)") + 
  ylab("Average GMV") +
  scale_color_discrete(name = "Label")


# 3 Day Time Window Aggregation -------------------------------------------

aggFreq <- 60*60*24*3
pa1 <- head(sliderWindowMean(salesOrderBlocked$ca, aggFreq,  salesOrderBlocked$selling_price), -1)
pa2 <- head(sliderWindowMean(salesOrderHealthy$ca, aggFreq,  salesOrderHealthy$selling_price), -1)

pasum <- as.data.frame(cbind(na.locf(pa1, fromLast = TRUE), na.locf(pa2, fromLast = TRUE)))

pasumRegularize <- as.data.frame(cbind(na.locf(pasum$..1, fromLast = TRUE), na.locf(pasum$..2, fromLast = TRUE))) 
colnames(pasumRegularize) <- c("Fraud", "Healthy")
pasumRegularize$time <- as.POSIXct(rownames(pasum))

ts.plot(pasumRegularize,gpars= list(col=rainbow(10)))

pasumGG <- melt(pasumRegularize, id = c("time"))

p <- ggplot(data=pasumGG, aes(x=time, y=value, group=variable))
p + geom_line(aes(color = variable)) + 
  labs(title = "3 Day Time Window Aggregation") +
  xlab("Time (Seconds)") + 
  ylab("Average GMV") +
  scale_color_discrete(name = "Label")


# Single Fraudster Examination --------------------------------------------

salesOrderBlockedSingle <- subset(sales_order_data, customer_id %in% c("28f505ca8dcf6cbbeb40bc0980a9d1fc3612bda7") )
salesOrderBlockedSingle <- salesOrderBlockedSingle[order(salesOrderBlockedSingle$created_at_secs),]

plot(salesOrderBlockedSingle$created_at_secs, salesOrderBlockedSingle$selling_price, type = "h")

library(lubridate)
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

ggplot(salesOrderBlocked, aes(x=ca, y= log(as.numeric(as.character(selling_price))/1000+1)  )) + 
  geom_bar(stat = "identity", colour="black") +
  labs(title = "Arrivals of Marketplace Transaction for Single Customer") +
  xlab("Date") + 
  ylab("Value")

hist(as.numeric(as.character(salesOrderBlocked$selling_price))/1000, breaks=400)

aggFreq <- 60*5
pa1 <- head(sliderWindowCount(salesOrderBlockedSingle$ca, aggFreq,  salesOrderBlockedSingle$selling_price), -1)
pa2 <- head(sliderWindowCount(salesOrderBlockedSingle$ca, aggFreq,  salesOrderBlockedSingle$selling_price), -1)

pasum <- as.data.frame(cbind(na.locf(pa1, fromLast = TRUE), na.locf(pa2, fromLast = TRUE)))

pasumRegularize <- as.data.frame(cbind(na.locf(pasum$..1, fromLast = TRUE), na.locf(pasum$..2, fromLast = TRUE))) 
colnames(pasumRegularize) <- c("Fraud", "Healthy")
pasumRegularize$time <- as.POSIXct(rownames(pasum))

# ts.plot(pasumRegularize,gpars= list(col=rainbow(10)))

pasumGG <- melt(pasumRegularize, id = c("time"))

p <- ggplot(data=pasumGG, aes(x=time, y=value, group=variable))
p + geom_line(aes(color = variable)) + 
  labs(title = "5 Minute Time Window Aggregation") +
  xlab("Time (Seconds)") + 
  ylab("Average GMV")



# Scrap Code --------------------------------------------------------------

Data <- data.frame(Time=Sys.time()+1:20,x=rnorm(20))
xData <- xts(Data[,-1], Data[,1])

xData <- xts(as.numeric(as.character(salesOrderBlocked$selling_price)), salesOrderBlocked$ca)
pa <- period.apply(xData, endpoints(xData, "seconds", 50000), colMeans)

