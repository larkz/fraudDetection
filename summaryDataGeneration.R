library(plyr)
library(ggplot2)
library(ROCR)

#setwd("/users/eric/Google Drive 2/MASc/Enterprise/EnterpriseSimulation")
setwd("C:\\Users\\Larkin\\Google Drive\\MASc\\Enterprise\\EnterpriseSimulation")
setwd("/users/admin/Google Drive/Enterprise/EnterpriseSimulation")

# source("processGeneration/generativeFunctions.R")


# Data Validtion ----------------------------------------------------------

# Sales Order Data
sales_order_data_unfilt <- read.csv("data/sales_order_test.csv", header = TRUE)
sales_order_data <- sales_order_data_unfilt[sales_order_data_unfilt$selling_price != "selling_price", ]
nrow(sales_order_data_unfilt) - nrow(sales_order_data)
rm(sales_order_data_unfilt)

sales_order_data$ca <- as.POSIXct(strptime(sales_order_data$created_at, "%m/%d/%y %H:%M"))
sales_order_data$created_at_secs <- as.integer(as.POSIXct(strptime(sales_order_data$created_at, "%m/%d/%y %H:%M")))

# Wallet Transactions
wallet_txn_unfilt <- read.csv("data/wallet_txns_test.csv", header = TRUE)
wallet_txn <- wallet_txn_unfilt[wallet_txn_unfilt$txn_amount != "txn_amount", ]
nrow(wallet_txn_unfilt) - nrow(wallet_txn)
wallet_txn$created_at_date <- as.POSIXct(wallet_txn$create_timestamp, "%y-%m-%d %H:%M:%OS3")
wallet_txn$created_at_secs <- as.integer(wallet_txn$created_at_date) 
rm(wallet_txn_unfilt)

# Sales Returns (build moving window, needs to be done.)
sales_returns_agg_unfilt <- read.csv("data/sales_returns_test.csv", header = TRUE)
sales_returns_agg <- sales_returns_agg_unfilt[sales_returns_agg_unfilt$customer_id != "customer_id", ]
rm(sales_returns_agg_unfilt)

# Need to be joined
# Customer List
customer_list_unfilt <- read.csv("data/customer_list_test.csv", header = TRUE)
customer_list <- customer_list_unfilt[customer_list_unfilt$customer_id != "customer_id", ]
rm(customer_list_unfilt)

fraud_list <- customer_list[customer_list$custom_label == "Fraud", ]
blocked_list <- customer_list[customer_list$custom_label == "Blocked", ]

# Customer Info
customer_info_unfilt <- read.csv("data/customer_info_test.csv")
customer_info <- customer_info_unfilt[customer_info_unfilt$customer_id != "customer_id" ,]
rm(customer_info_unfilt)


# Feature Effective Checking ----------------------------------------------

# Sales_order
# 1 - Total GMV Puchasing
# 2 - Total Number of Purchases
# 3 - Average time inbetween transactions
# 4 - Mean spending (GMV) per purchase
# 5 - Number of unique categories purchased in

sales_order_agg0 <- merge(sales_order_data, customer_list, by="customer_id") 
getStemString <- function(str){
  if (is.na(str)){return("other")} else{
    if (as.character(str) != "null"){return(strsplit(as.character(str), "/")[[1]][1])}
    else {return("other")}
  }
}

clusterTxnValue <- function(str){
  if (is.na(str)){return(0)} else{
      if(as.numeric( as.character(str)) < exp(10) ){return("low")}
      else if(as.numeric( as.character(str)) > exp(14) ){return("high")}
      else{return("med")}
    }
}


sales_order_agg0$t1_category <- sapply(sales_order_agg0$category, getStemString)
sales_order_agg0$selling_price_class <- sapply(sales_order_agg0$selling_price, clusterTxnValue)

sales_order_agg1  <- sales_order_agg0[with(sales_order_agg0, order(created_at_secs)),]

sales_order_agg1$price <-  sapply(sales_order_agg1$selling_price, function(x) as.integer(as.character(x)) )
# sales_order_agg1$price <-  apply(sales_order_agg1, sales_order_agg1$price, function(x) as.integer(as.character(x)))

sales_order_agg1$isTopup <- grepl("topup", sales_order_agg1$category)
sales_order_agg1$isRecharge <- grepl("recharge", sales_order_agg1$category)
sales_order_agg1$isFrequent <- sales_order_agg1$isRecharge | sales_order_agg1$isTopup

### Time Series Aggregation

sales_order_eff1 <- ddply(sales_order_agg1, ~customer_id, summarise, sum_gmv=sum(price))
sales_order_eff2 <- ddply(sales_order_agg1, ~customer_id, summarise, count_txn=length(price))

# 3
mean_diff <- function(x){mean(diff(x), na.rm = TRUE)}
sales_order_eff3 <- ddply(sales_order_agg1, ~customer_id, summarise, mean_diff_purc=mean_diff(created_at_secs))
#sales_order_eff4 <- sales_order_eff1$sum_gmv/sales_order_eff2$count_txn
sales_order_eff5 <- ddply(sales_order_agg1, ~customer_id, summarise, count_distinct_cat=length(unique(category)))


# Append All
# sales_order_effAll <- merge(merge(merge(sales_order_eff1, sales_order_eff2, by ="customer_id"), 
#                             sales_order_eff3, by ="customer_id"), sales_order_eff5, "customer_id")

sales_order_effAll <- Reduce(function(x, y) merge(x, y, "customer_id"), list(sales_order_eff1, 
                                                                             sales_order_eff2, 
                                                                             sales_order_eff3,
                                                                             sales_order_eff5))

sales_order_effAll$mean_gmv_per_txn <- sales_order_effAll$sum_gmv/sales_order_effAll$count_txn

sales_order_effAll_withLabel <- merge(sales_order_effAll, customer_list, by = "customer_id")



# boxplot(log(sum_gmv/count_txn) ~ as.factor(custom_label),
#         data = sales_order_effAll_withLabel, main="Feature Effectiveness Data", 
#         xlab="Fraud Label", ylab="Log(Average Spending)")

# Wallet Transactions
# 1 - Total GMV Puchasing
# 2 - Total number of wallet transactions
# 3 - Average time inbetween transactions

wallet_agg0 <- merge(wallet_txn, customer_list, by = "customer_id") 
wallet_txn_agg1  <- wallet_agg0[with(wallet_agg0, order(created_at_secs)),]

wallet_txn_agg1$price <-  sapply(wallet_txn_agg1$txn_amount, function(x) as.integer(as.character(x)) )
wallet_txn_eff1 <- ddply(wallet_txn_agg1, ~customer_id, summarise, sum_gmv_wal=sum(price))

wallet_txn_eff2 <- ddply(wallet_txn_agg1, ~customer_id, summarise, count_txn_wal =length(price))
wallet_txn_eff3 <- ddply(wallet_txn_agg1, ~customer_id, summarise, mean_diff_purc_wal=mean_diff(created_at_secs))

wallet_txn_effAll <- Reduce(function(x, y) merge(x, y, "customer_id"), list(wallet_txn_eff1, 
                                                                            wallet_txn_eff2, 
                                                                            wallet_txn_eff3))

wallet_txn_effAll$mean_gmv_per_txn_wal <- wallet_txn_effAll$sum_gmv_wal/wallet_txn_effAll$count_txn_wal

wallet_txn_effAll_withLabel <- merge(sales_order_effAll_withLabel, wallet_txn_effAll, by = "customer_id")


# Registration Data Join ------------------------------------------------

summaryData <- merge(wallet_txn_effAll_withLabel, customer_info, by = "customer_id")
summaryData$wallet_type[summaryData$wallet_type == "null"] <- "NO_WALLET"

summaryData$isFraud <- summaryData$custom_label == "Blocked"
summaryData$wallet_type_SCW <- summaryData$wallet_type == "SCW"
summaryData$wallet_type_NO_WALLET <- summaryData$wallet_type == "NO_WALLET"
summaryData$wallet_type_PRIME <- summaryData$wallet_type == "PRIME"

summaryDataEncoded <- summaryData

library(data.table)
old_names <- c("phone_isverified",
               "email_isverified",
               "wallet_type_SCW",
               "wallet_type_NO_WALLET",
               "wallet_type_PRIME",
               "sum_gmv", 
               "count_txn", 
               "mean_diff_purc",
               "count_distinct_cat",
               "mean_gmv_per_txn",
               "sum_gmv_wal",
               "count_txn_wal",
               "mean_diff_purc_wal",
               "mean_gmv_per_txn_wal")

new_names <- c("b1",
               "b2",
               "b3",
               "b4",
               "b5",
               "v1",
               "v2",
               "v3",
               "v4",
               "v5",
               "v6",
               "v7",
               "v8",
               "v9")

setnames(summaryDataEncoded, old=old_names, new=new_names )
names(summaryDataEncoded)

summaryRegress <- summaryDataEncoded[,c("isFraud", new_names, "customer_id")]
cols <- sapply(summaryRegress[, !names(summaryRegress) %in% c("customer_id")] , is.logical)
summaryRegress[,cols] <- lapply(summaryRegress[,cols], as.numeric)
summaryRegress$customer_id <- summaryDataEncoded$customer_id


toNumeric <- function(s){
  as.numeric(as.character(s))
}

# summaryRegress <- as.data.frame(sapply( summaryRegress, toNumeric ))

# Join with wallet data
summary1 <- merge(summaryRegress, sales_order_effAll)
drops <- c("fraud_label","collusion_id", "custom_label")
summary1 <- summary1[ , !(names(summary1) %in% drops)]
setnames(summary1, old = c("sum_gmv", 
                           "count_txn",
                           "mean_diff_purc",
                           "count_distinct_cat",
                           "mean_gmv_per_txn",
                           "sum_gmv",
                           "count_txn",
                           "mean_diff_purc",
                           "mean_gmv_per_txnl"), new = c(
                            "w1",
                            "w2",
                            "w3",
                            "w4",
                            "w5",
                            "w6",
                            "w7",  
                            "w8",
                            "w9"))

save(summary1, file = "summary1.RData")


# fit1 <- glm(isFraud ~ ., data = summaryRegress, family = binomial)




# Time Series Window Features ---------------------------------------------

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




