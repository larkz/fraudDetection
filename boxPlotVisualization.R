library(ggplot2)

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

