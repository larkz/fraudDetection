## Random Forests algorithm

library(randomForest)


rf1 <- randomForest( isFraud ~ b2 + b2 + b3 + b4 + b5 + v1 + v2 , trainSummaryRegress, proximity=TRUE)

rf1 <- randomForest( isFraud ~ b1 
                    + b2 
                    + b3
                    + b4
                    + b5
                    + v1
                    + v2
                    # + v3
                    # v4
                    # + v5
                    # + v6
                    # + v7
                    # + v8
                    + v9, data=trainSummaryRegress, proximity=TRUE, keep.forest=TRUE)


rf1Preds <- predict(rf1, testSummaryRegress)

res <- cbind(testSummaryRegress, as.numeric(as.character(rf1Preds)))
colnames(res)[which(names(res) == "as.numeric(as.character(rf1Preds))")] <- "prob_rf"

# preds <- cbind(rf1Preds, rf1Preds >= 0.5)


# Performance Checking


threshLogR <- 0.05

Pos <- subset(res, res$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_rf > threshLogR)) ==1 ))/nrow(Pos)

Fal <- subset(res, res$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_rf > threshLogR)) ==1 ))/nrow(Fal)

TP <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_rf > threshLogR)) ==1 ))
FN <- nrow(Pos) - TP

FP <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_rf > threshLogR)) ==1 ))
TN <- nrow(Fal) - FP

TPR
FPR

TP
FN
FP
TN

### ROC making

pred_rf <- prediction(res$prob_rf, testSummaryRegress$isFraud)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")




