
# Aggregate Statistics by customer_id -------------------------------------

agg_df <- sales_order_agg1
sales_order_meangmv_cid <- ddply(agg_df, ~customer_id, summarise, mean_gmv=mean(price))
sales_order_meandiff_cid <- ddply(agg_df, ~customer_id, summarise, mean_diff_purc=mean_diff(created_at_secs))
sales_order_numpurc_cid <- ddply(agg_df, ~customer_id, summarise, num_purc=length(order_id))
sales_order_unicat_cid <- ddply(agg_df, ~customer_id, summarise, unicat=length(unique(category)))
sales_order_sumgmv_cid <- ddply(agg_df, ~customer_id, summarise, sum_gmv=sum(price))

sales_order_summary_cid <- Reduce(function(x, y) merge(x, y, "customer_id"), list(sales_order_numpurc_cid, 
                                                                             sales_order_meandiff_cid, 
                                                                             sales_order_unicat_cid,
                                                                             sales_order_meangmv_cid, sales_order_sumgmv_cid
                                                                             ))

sales_order_summary_cid_withLabel <- merge(sales_order_summary_cid, customer_list, by = "customer_id")

# Population Count --------------------------------------------------------

nrow(subset(sales_order_summary_cid_withLabel, sales_order_summary_cid_withLabel$custom_label == "Blocked"))
nrow(subset(sales_order_summary_cid_withLabel, sales_order_summary_cid_withLabel$custom_label == "Fraud"))
nrow(subset(sales_order_summary_cid_withLabel, sales_order_summary_cid_withLabel$custom_label == "30 or More Purchases"))
nrow(subset(sales_order_summary_cid_withLabel, sales_order_summary_cid_withLabel$custom_label == "30 Less Purchases"))


sales_order_summary_cid_withLabel_blocked 
sales_order_summary_cid_withLabel_fraud

sales_order_summary_cid_filt <- subset(sales_order_summary_cid, sales_order_summary_cid$num_purc > 1)

gmvPerCid <- function(){
  nBreaks <- 100
  ttAllHist <- hist(log(sales_order_summary_cid_filt$mean_gmv), breaks = nBreaks, main = "Average Spending (GMV) per Customer", xlab = "log(GMV)")
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  startparam <- mixparam(mu = c(10), sigma = 1.5) 
  
  fit2 <- mix(ttAllHistDf, startparam, "lnorm")
  plot(ttAllHistDf)
  plot(fit2, xlab = "Log(GMV)", main ="Average GMV per Customer")
}
gmvPerCid()

meanDiffPerCid <- function(){
  nBreaks <- 100
  ttAllHist <- hist(log(sales_order_summary_cid_filt$mean_diff), breaks = 100, main = "Average Time (Seconds) inbetween Transactions per Customer", xlab = "log(Seconds)")
  ttAllHistDf <- as.data.frame(cbind(ttAllHist$breaks[1:length(ttAllHist$counts)], ttAllHist$counts))
  startparam <- mixparam(mu = c(4, 13), sigma = 1.5) 
  
  fit2 <- mix(ttAllHistDf, startparam, "gamma")
  plot(ttAllHistDf)
  plot(fit2, xlab = "Log(Seconds)", main ="Average Time (Seconds) inbetween Transactions per Customer")
}
meanDiffPerCid()

hist(log(sales_order_summary_cid_filt$num_purc), breaks = 50, main = "Total Number of Transactions per Customer", xlab = "log(Number of Transactions)")
hist(sales_order_summary_cid_filt$unicat, breaks = 50, main = "Total Number of Unique Categories per Customer", xlab = "Number of Unique Customers")


# Aggregate Statistics by Customer Id Fraud -------------------------------

sales_order_meangmv_cid_fraud <- ddply(sales_order_agg1_fraud, ~customer_id, summarise, mean_gmv=mean(price))
sales_order_meandiff_cid_fraud <- ddply(sales_order_agg1_fraud, ~customer_id, summarise, mean_diff_purc=mean_diff(created_at_secs))
sales_order_numpurc_cid_fraud <- ddply(sales_order_agg1_fraud, ~customer_id, summarise, num_purc=length(order_id))
sales_order_unicat_cid_fraud <- ddply(sales_order_agg1_fraud, ~customer_id, summarise, unicat=length(unique(category)))


sales_order_summary_cid_fraud <- merge(merge(merge(sales_order_numpurc_cid_fraud, sales_order_meandiff_cid_fraud, by = "customer_id"), 
                                       sales_order_unicat_cid_fraud, by = "customer_id"), 
                                 sales_order_meangmv_cid_fraud, by = "customer_id")
sales_order_summary_cid_fraud_filt <- subset(sales_order_summary_cid_fraud, sales_order_summary_cid_fraud$num_purc > 1)



# Fraud vs Non-Fraud Comparative Statistics -------------------------------
library(pastecs)
library(psych)
View(describe(sales_order_summary_cid))

stat.desc(sales_order_summary_cid) 
stat.desc(sales_order_summary_cid_fraud) 




# Aggregate Statistics by Category ----------------------------------------


sales_order_meangmv_cat <- ddply(sales_order_agg1_healthy, ~category, summarise, mean_gmv=mean(price))
sales_order_meandiff_cat <- ddply(sales_order_agg1_healthy, ~category, summarise, mean_diff_purc=mean_diff(created_at_secs))
sales_order_numpurc_cat <- ddply(sales_order_agg1_healthy, ~category, summarise, num_purc=length(order_id))
sales_order_unicid_cat <- ddply(sales_order_agg1_healthy, ~category, summarise, unicid=length(unique(customer_id)))

sales_order_summary_cat <- merge(merge(merge(sales_order_numpurc_cat, sales_order_meandiff_cat, by = "category"), 
                                       sales_order_unicid_cat, by = "category"), 
                                      sales_order_meangmv_cat, by = "category")
sales_order_summary_cat_filt <- subset(sales_order_summary_cat, sales_order_summary_cat$unicid > 2)


hist(log(sales_order_summary_cat_filt$mean_gmv), breaks = 30, main = "Average Spending (GMV) per Category", xlab = "log(GMV)")
hist(log(sales_order_summary_cat_filt$mean_diff), breaks = 30, main = "Average Time (Seconds) inbetween Transactions per Category", xlab = "log(Seconds)")
hist(log(sales_order_summary_cat_filt$num_purc), breaks = 30, main = "Total Number of Transactions per Category", xlab = "log(Number of Transactions)")
hist(log(sales_order_summary_cat_filt$unicid), breaks = 30, main = "Total Number of Unique Customers per Category", xlab = "log(Number of Unique Customers)")


# Aggregate Statistics by T1 Category -------------------------------------


sales_order_meangmv_cat_t1 <- ddply(sales_order_agg1_healthy, ~t1_category, summarise, mean_gmv=mean(price))
sales_order_meandiff_cat_t1 <- ddply(sales_order_agg1_healthy, ~t1_category, summarise, mean_diff_purc=mean_diff(created_at_secs))
sales_order_numpurc_cat_t1 <- ddply(sales_order_agg1_healthy, ~t1_category, summarise, num_purc=length(order_id))
sales_order_unicid_cat_t1 <- ddply(sales_order_agg1_healthy, ~t1_category, summarise, unicid=length(unique(customer_id)))
sales_order_sumgmv_cat_t1 <- ddply(sales_order_agg1_healthy, ~t1_category, summarise, sum_gmv=sum(as.numeric(price)))


sales_order_summary_cat_t1 <- Reduce(function(x, y) merge(x, y, "t1_category"), list(sales_order_meangmv_cat_t1, 
                                                                                  sales_order_meandiff_cat_t1, 
                                                                                  sales_order_numpurc_cat_t1,
                                                                                  sales_order_unicid_cat_t1, 
                                                                                  sales_order_sumgmv_cat_t1))

sales_order_summary_cat_filt <- subset(sales_order_summary_cat, sales_order_summary_cat$unicid > 2)



# Subset and Histogram ----------------------------------------------------
par(mfrow=c(4,3))

count <- 0
for (n in sales_order_summary_cat_t1$t1_category){
  count <- count + 1
  dat <- subset(sales_order_agg1_healthy, sales_order_agg1_healthy$t1_category == n)
  if(nrow(dat) > 500 ) {hist(log(as.numeric(as.character(dat$selling_price))), main = n, xlab = "log(Selling Price)")}
}

par(mfrow=c(4,3))

count <- 0
for (n in sales_order_summary_cat_t1$t1_category){
  count <- count + 1
  dat <- subset(sales_order_agg1_healthy, sales_order_agg1_healthy$t1_category == n)
  if(nrow(dat) > 500 ) { hist(log(diff(as.numeric(as.character(dat$created_at_secs)))), main = n, xlab = "Seconds")  }
}


par(mfrow=c(1,1))



