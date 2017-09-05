# Random Forests


rfFullFit <- cforest(isFraudFactor ~ b1 
                   + b2
                   + b3
                   + b4
                   + b5
                   + v1
                   + v2
                   + v3
                   + v4
                   + v5
                   + v6
                   + v7
                   + v8
                   + v9, data=trainSummaryRegress)


predsRf <- predict(rfFullFit, newdata = testSummaryRegress)

preds <- predsRf

pos <- as.numeric(as.character(preds)) + testSummaryRegress$isFraudInt
TP <- sum(pos == 2) 
TN <- sum(pos == 0)

negs <- as.numeric(as.character(preds))  - testSummaryRegress$isFraudInt
FP <- sum(negs == 1)
FN <- sum(negs == -1)

TPR <- TP/(TP + FN)
FPR <- FP/(TN + FP)

recall <- TP/(TP + FN)
prec <- TP/(TP+ FP)

