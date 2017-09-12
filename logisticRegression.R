library(ROCR)
library(psych)

# Data Split  -------------------------------------------------------------

load("summary1.RData")

summary1$w1[is.nan(summary1$w1)] <- 0
summary1$w2[is.nan(summary1$w2)] <- 0
summary1$w3[is.nan(summary1$w3)] <- 0
summary1$w4[is.nan(summary1$w4)] <- 0
summary1$w5[is.nan(summary1$w5)] <- 0
summary1$w6[is.nan(summary1$w6)] <- 0
summary1$w7[is.nan(summary1$w7)] <- 0
summary1$w9[is.nan(summary1$w9)] <- 0


smp_size <- floor(0.7 * nrow(summary1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(summary1)), size = smp_size)

trainSummaryDataEncoded <- summary1[train_ind, ]
testSummaryDataEncoded <- summary1[-train_ind, ]

trainSummaryRegress <- summary1[train_ind, ]
testSummaryRegress <- summary1[-train_ind, ]


save(trainSummaryDataEncoded, file = "trainSummaryDataEncoded.RData")
save(testSummaryDataEncoded, file = "testSummaryDataEncoded.RData")

save(trainSummaryRegress, file = "trainSummaryRegress.RData")
save(testSummaryRegress, file = "testSummaryRegress.RData")

load("trainSummaryRegress.RData")
load("testSummaryRegress.RData")

load("trainSummaryDataEncoded.RData")
load("testSummaryDataEncoded.RData")

# Logistic Regression and ROC on all Features --------------------

cor(data.matrix(trainSummaryRegress[,c("b1", "b2", "b3", "b4", "b5", "v1", "v3", "v4", "v5", "v6", "v7", "v9")]))




summary(fit_tsagg_end <- glm(isFraud ~ b1 + b2 + b3 + b4 + v1 + v3 + v4 + v5 + v6 + v7 + v9, data = trainSummaryDataEncoded, family = binomial))


prob <- predict(fit_tsagg_end, newdata=testSummaryRegress, type="response")

testSummaryRegress$prob_lrfull <- prob

pred_lrfull <- prediction(prob, testSummaryRegress$isFraud)
perf_lrfull <- performance(pred_lrfull, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Logistic Regression ROC Curve on Select Features")) +
  xlim(0, 0.1)

# Model Evaluation

threshLogR <- 0.05

Pos <- subset(testSummaryRegress, testSummaryRegress$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_lrfull > threshLogR)) ==1 ))/nrow(Pos)

Fal <- subset(testSummaryRegress, testSummaryRegress$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_lrfull > threshLogR)) ==1 ))/nrow(Fal)

TP <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_lrfull > threshLogR)) ==1 ))
FN <- nrow(Pos) - TP

FP <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_lrfull > threshLogR)) ==1 ))
TN <- nrow(Fal) - FP

TPR
FPR

TP
FN
FP
TN


## Registration Features Only

summary(fit_tsagg_end <- glm(isFraud ~ b1
                             + b2
                             + b3
                             + b4
                             + b5, data = trainSummaryRegress, family = binomial))


prob <- predict(fit_tsagg_end, newdata=testSummaryRegress, type="response")

testSummaryRegress$prob_lrreg <- prob

pred_lrreg <- prediction(prob, testSummaryRegress$isFraud)
perf_lrreg <- performance(pred_lrreg, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve on Registration Features AUC = ", auc))

# Model Evaluation

threshLogR <- 0.07

Pos <- subset(testSummaryRegress, testSummaryRegress$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_lrreg > threshLogR)) ==1 ))/nrow(Pos)

Fal <- subset(testSummaryRegress, testSummaryRegress$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_lrreg > threshLogR)) ==1 ))/nrow(Fal)

TP <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_lrreg > threshLogR)) ==1 ))
FN <- nrow(Pos) - TP

FP <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_lrreg > threshLogR)) ==1 ))
TN <- nrow(Fal) - FP

TPR
FPR

TP
FN
FP
TN
