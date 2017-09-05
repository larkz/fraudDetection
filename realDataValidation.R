library(plyr)
library(ggplot2)
library(ROCR)

setwd("/users/eric/Google Drive 2/MASc/Enterprise/EnterpriseSimulation")
setwd("C:\\Users\\larkinliu\\Google Drive\\MASc\\Enterprise\\EnterpriseSimulation")
setwd("/users/admin/Google Drive/Enterprise/EnterpriseSimulation")

source("processGeneration/generativeFunctions.R")


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
sales_order_agg1  <- sales_order_agg0[with(sales_order_agg0, order(created_at_secs)),]

sales_order_agg1$price <-  sapply(sales_order_agg1$selling_price, function(x) as.integer(as.character(x)) )
# sales_order_agg1$price <-  apply(sales_order_agg1, sales_order_agg1$price, function(x) as.integer(as.character(x)))

sales_order_eff1 <- ddply(sales_order_agg1, ~customer_id, summarise, sum_gmv=sum(price))
sales_order_eff2 <- ddply(sales_order_agg1, ~customer_id, summarise, count_txn=length(price))

# 3
mean_diff <- function(x){mean(diff(x))}
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
summaryData[summaryData$wallet_type == "null", ] <- "NO_WALLET"

summaryData$isFraudPred <- summaryData$custom_label == "Blocked"
summaryData$wallet_type_SCW <- summaryData$wallet_type == "SCW"
summaryData$wallet_type_NO_WALLET <- summaryData$wallet_type == "NO_WALLET"
summaryData$wallet_type_PRIME <- summaryData$wallet_type == "PRIME"



# Encoding ----------------------------------------------------------------

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

new_names <- c("xb1",
               "xb2",
               "xb3",
               "xb4",
               "xb5",
               "xv1",
               "xv2",
               "xv3",
               "xv4",
               "xv5",
               "xv6",
               "xv7",
               "xv8",
               "xv9")

setnames(summaryDataEncoded, old=old_names, new=new_names )
names(summaryDataEncoded)



# Sales Order Data --------------------------------------------------------
p <- ggplot(sales_order_effAll_withLabel, aes(log(sum_gmv), fill=custom_label))
p + geom_density(alpha=0.25) + ggtitle("Total Spending and Fraud") + labs(x = "Log(sum_gmv)")  + guides(fill=guide_legend(title="Customer Label"))

p <- ggplot(sales_order_effAll_withLabel, aes(log(sum_gmv), fill=custom_label))
p + geom_histogram(alpha=0.25) + ggtitle("Total Spending and Fraud") + labs(x = "Log(sum_gmv)")  + guides(fill=guide_legend(title="Customer Label"))


p <- ggplot(sales_order_effAll_withLabel, aes(log(sum_gmv)))
p + geom_histogram(alpha=0.75) + ggtitle("Total Spending and Fraud") + labs(x = "Log(sum_gmv)")

p <- ggplot(sales_order_effAll_withLabel, aes(sum_gmv))
p + geom_histogram(alpha=0.75) + ggtitle("Total Spending and Fraud") + labs(x = "Log(sum_gmv)")


p <- ggplot(sales_order_effAll_withLabel, aes(factor(custom_label), log(sum_gmv), fill=custom_label))
p + geom_boxplot() + ggtitle("Total Spending and Fraud") + labs(x = "Fraud Label", y = "Log(sum_gmv)")  + guides(fill=guide_legend(title="Customer Label"))

p <- ggplot(sales_order_effAll_withLabel, aes(factor(custom_label), log(count_txn)))
p + geom_boxplot() + ggtitle("Total Transactions and Fraud") + labs(x = "Fraud Label", y = "Log(Number of Transactions)") 

p <- ggplot(sales_order_effAll_withLabel, aes(factor(custom_label), log(sum_gmv/count_txn), fill=custom_label))
p + geom_boxplot() + ggtitle("Average Spending and Fraud") + labs(x = "Fraud Label", y = "Log(Average Transaction Amount)") + guides(fill=guide_legend(title="Customer Label"))

p <- ggplot(sales_order_effAll_withLabel, aes(factor(custom_label), log(mean_diff_purc), fill=custom_label))
p + geom_boxplot() + ggtitle("Average Time Inbetween Purchases and Fraud") + labs(x = "Fraud Label", y = "Log(Average Seconds in Between Purchases)") + guides(fill=guide_legend(title="Customer Label"))

p <- ggplot(sales_order_effAll_withLabel, aes(factor(custom_label), log(count_distinct_cat)))
p + geom_boxplot() + ggtitle("Number of Distinct Categories and Fraud") + labs(x = "Fraud Label", y = "Log(Category Count)") 


# Wallet Transaction Data -------------------------------------------------

p <- ggplot(wallet_txn_effAll_withLabel, aes(factor(custom_label), log(sum_gmv_wal), fill=custom_label) )
p + geom_boxplot( ) + ggtitle("Total Wallet Transaction & Fraud") + labs(x = "Fraud Label", y = "Log(Total Wallet Spending)") + guides(fill=guide_legend(title="Customer Label"))


p <- ggplot(wallet_txn_effAll_withLabel, aes(factor(custom_label), log(count_txn_wal), fill=custom_label))
p + geom_boxplot() + ggtitle("Total Wallet Transaction & Fraud") + labs(x = "Fraud Label", y = "Log(Total Wallet Transactions)") 

p <- ggplot(wallet_txn_effAll_withLabel, aes(factor(custom_label), log(mean_diff_purc_wal)))
p + geom_boxplot() + ggtitle("Wallet: Average Time Inbetween Purchases and Fraud") + labs(x = "Fraud Label", y = "Log(Average Seconds in Between Purchases)") 


p <- ggplot(wallet_txn_effAll_withLabel, aes(factor(custom_label), log(mean_gmv_per_txn_wal)))
p + geom_boxplot() + ggtitle("Wallet: Average Spending (GMV) per Transaction") + labs(x = "Fraud Label", y = "Log(GMV)") 










# Feature Effectiveness ROC Curves ----------------------------------------

# Fit on 1 feature Comparison ---------------------------------------------

summaryData$isFraudPred <- summaryData$custom_label == "Blocked"

is_test <- runif(nrow(summaryData)) > 0.75
train <- summaryData[is_test==FALSE,]
test <- summaryData[is_test==TRUE,]

summary(fit_mean_gmv_per_txn <- glm(isFraudPred ~ mean_gmv_per_txn, data = summaryData))

prob <- predict(fit_mean_gmv_per_txn, newdata=test, type="response")
pred <- prediction(prob, test$isFraudPred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Average Marketplace GMV Per Transaction ROC Curve w/ AUC=", auc))


summary(fit_mean_diff_purc <- glm(isFraudPred ~ mean_diff_purc, data = wallet_txn_effAll_withLabel))

prob <- predict(fit_mean_diff_purc, newdata=test, type="response")
pred <- prediction(prob, test$isFraudPred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Average Marketplace Time inbetween Transactions ROC Curve w/ AUC=", auc))


# Logistic Regression Registration Features ---------------------------------------------------

summary(fit_reg <- glm(isFraudPred ~ 
                       email_isverified
                       + phone_isverified
                       + wallet_type, data = summaryData))

summary(fit_reg_enc <- glm(isFraudPred ~ xb1 + xb2 + xb3 + xb4 + xb5, data = summaryDataEncoded))



# Manual Risk Ratio Calculations ------------------------------------------

nrow(subset(summaryData, isFraudPred == TRUE))/nrow(subset(summaryData))
(nrow(subset(subset(summaryData, isFraudPred == TRUE), email_isverified == 1))/nrow(subset(summaryData, isFraudPred == TRUE)))/(nrow(subset(subset(summaryData, isFraudPred == FALSE), email_isverified == 1))/nrow(subset(summaryData, isFraudPred == FALSE)))
(nrow(subset(subset(summaryData, isFraudPred == TRUE), phone_isverified == 1))/nrow(subset(summaryData, isFraudPred == TRUE)))/(nrow(subset(subset(summaryData, isFraudPred == FALSE), phone_isverified == 1))/nrow(subset(summaryData, isFraudPred == FALSE)))
(nrow(subset(subset(summaryData, isFraudPred == TRUE), wallet_type == "SCW"))/nrow(subset(summaryData, isFraudPred == TRUE)))/(nrow(subset(subset(summaryData, isFraudPred == FALSE), wallet_type == "SCW"))/nrow(subset(summaryData, isFraudPred == FALSE)))
(nrow(subset(subset(summaryData, isFraudPred == TRUE), wallet_type == "NO_WALLET"))/nrow(subset(summaryData, isFraudPred == TRUE)))/(nrow(subset(subset(summaryData, isFraudPred == FALSE), wallet_type == "NO_WALLET"))/nrow(subset(summaryData, isFraudPred == FALSE)))
(nrow(subset(subset(summaryData, isFraudPred == TRUE), wallet_type == "PRIME"))/nrow(subset(summaryData, isFraudPred == TRUE)))/(nrow(subset(subset(summaryData, isFraudPred == FALSE), wallet_type == "PRIME"))/nrow(subset(summaryData, isFraudPred == FALSE)))


# Set T at 50% ------------------------------------------------------------

TPRFPRarray <- c()

tprCheckBinary <- function( data, dataLab){
  data[is.na(data)] <- 0
  detP <- as.data.frame(cbind(as.numeric(as.character(data)), dataLab))
  colnames(detP) <- c("pred", "lab")
  TP <- nrow(subset(subset(detP, pred == 1), lab == 1))
  FP <- nrow(subset(subset(detP, pred == 1), lab == 0))
  TN <- nrow(subset(subset(detP, pred == 0), lab == 0))
  FN <- nrow(subset(subset(detP, pred == 0), lab == 1))
  TPR <- TP/(TP + FN)
  FPR <- FP/(FP + TN)
  c(TP, FP, TN, FN, TPR, FPR  )
}

featureListBin <- c("phone_isverified",
                    "email_isverified",
                    "wallet_type_SCW",
                    "wallet_type_NO_WALLET",
                    "wallet_type_PRIME")


for (n in featureListBin){
  TPRFPRarray <- rbind(TPRFPRarray, c( n,tprCheckBinary( summaryData[[n]], summaryData$isFraudPred )))
}

tprCheckBinary( summaryData[[n]], summaryData$isFraudPred)
TPFPdfBin <- as.data.frame( TPRFPRarray)


tprCheck <- function( data, dataLab){
  
  T_val <- median(data, na.rm = TRUE)
  detP <- as.data.frame(cbind( as.numeric(as.matrix(data > T_val)), dataLab))
  colnames(detP) <- c("pred", "lab")
  TP <- nrow(subset(subset(detP, pred == 1), lab == 1))
  FP <- nrow(subset(subset(detP, pred == 1), lab == 0))
  TN <- nrow(subset(subset(detP, pred == 0), lab == 0))
  FN <- nrow(subset(subset(detP, pred == 0), lab == 1))
  TPR <- TP/(TP + FN)
  FPR <- FP/(FP + TN)
  c(TP, FP, TN, FN, TPR, FPR  )
}


tprCheck( summaryData$count_distinct_cat, summaryData$isFraudPred )

featureList <- c("sum_gmv",
                 "count_txn",
                 "mean_diff_purc", 
                 "count_distinct_cat", 
                 "mean_gmv_per_txn", 
                 "sum_gmv_wal", 
                 "count_txn_wal", 
                 "mean_diff_purc_wal", 
                 "mean_gmv_per_txn_wal")

TPRFPRarray <- c()

for (n in featureList){
  TPRFPRarray<- rbind(TPRFPRarray, tprCheck( summaryData[[n]], summaryData$isFraudPred ))
}
TPFPdf <- as.data.frame(cbind(featureList, TPRFPRarray))






prob <- predict(fit_reg, newdata=test, type="response")
pred <- prediction(prob, test$isFraudPred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Registration Features ROC Curve w/ AUC=", auc))





# Logistic Regression on Time Series Features -----------------------------

summary(fit_tsagg <- glm(isFraudPred ~ 
                         sum_gmv
                       + count_txn 
                       + mean_diff_purc 
                       + count_distinct_cat 
                       + mean_gmv_per_txn 
                       + sum_gmv_wal 
                       + count_txn_wal 
                       + mean_diff_purc_wal 
                       + mean_gmv_per_txn_wal, data = summaryData, family = binomial))

summary(fit_tsagg_end <- glm(isFraudPred ~ 
                           xv1
                         + xv2 
                         + xv3 
                         + xv4 
                         + xv5 
                         + xv6
                         + xv7 
                         + xv8 
                         + xv9, data = summaryDataEncoded, family = binomial))


prob <- predict(fit_tsagg, newdata=test, type="response")
pred <- prediction(prob, test$isFraudPred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Aggregate Time Series Features ROC Curve w/ AUC=", auc))

# All features classifier LR ----------------------------------------------

summary(fit_all <- glm(isFraudPred ~ 
                         mean_diff_purc_wal
                       + mean_diff_purc 
                       + wallet_type 
                       + count_txn_wal 
                       + count_distinct_cat 
                       + sum_gmv 
                       + email_isverified 
                       + sum_gmv_wal 
                       + phone_isverified
                       + mean_gmv_per_txn_wal, data = summaryData, family = binomial))

prob <- predict(fit_all, newdata=test, type="response")
pred <- prediction(prob, test$isFraudPred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Combined Feature Set ROC Curve w/ AUC=", auc))





# Table Export ------------------------------------------------------------

sum_fit_all <- summary(fit_all)

HR <- coef(sum_fit_all)
CI <- round(confint(sum_fit_all), 2)
colnames(CI) <- c("Lower", "Higher")

