library(plyr)
library(ggplot2)
library(ROCR)

setwd("/users/eric/Google Drive 2/MASc/Enterprise/EnterpriseSimulation")
setwd("C:\\Users\\larkinliu\\Google Drive\\MASc\\Enterprise\\EnterpriseSimulation")
setwd("/users/admin/Google Drive/Enterprise/EnterpriseSimulation")

source("processGeneration/generativeFunctions.R")
source("summaryDataGeneration.R")

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


# Combined Feature Set ----------------------------------------------------

summary(fit_all <- glm(isFraudPred ~ ., data = summaryRegress, family = binomial))

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


# Regression Summary ------------------------------------------------------

# Table Export ------------------------------------------------------------

sum_fit_all <- summary(fit_all)

HR <- coef(sum_fit_all)
CI <- round(confint(sum_fit_all), 2)
colnames(CI) <- c("Lower", "Higher")

