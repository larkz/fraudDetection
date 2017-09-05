# Naive Bayes Calculator

library(e1071)

mapTF <- function(S){
  if (S == 1){return("T")}else {return("F")}
}

map01 <- function(S){
  if (S == 1){return(1)}else {return(0)}
}

summaryRegress$b1 <-  sapply(summaryRegress$b1, map01)
summaryRegress$b2 <-  sapply(summaryRegress$b2, map01)
summaryRegress$b3 <-  sapply(summaryRegress$b3, map01)
summaryRegress$b4 <-  sapply(summaryRegress$b4, map01)
summaryRegress$b5 <-  sapply(summaryRegress$b5, map01)
summaryRegress$isFraudInt <- sapply(summaryRegress$isFraud, as.integer)
summaryRegress$isFraud <- sapply(as.integer(summaryRegress$isFraudInt), mapTF)

smp_size <- floor(0.75 * nrow(summaryRegress))


## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(summaryRegress)), size = smp_size)

trainSummaryRegress <- summaryRegress[train_ind, ]
testSummaryRegress <- summaryRegress[-train_ind, ]

###  start here

nb <- naiveBayes(isFraud ~ ., data = trainSummaryRegress[,1:6])
nb

testSubset <- testSummaryRegress[,1:6]
preds <- predict(nb, testSubset, type="raw")

res <- cbind(testSummaryRegress, preds)
res[["0"]] <- NULL
colnames(res)[which(names(res) == "1")] <- "prob_nb"


pred_nb <- prediction(res$prob_nb, testSummaryRegress$isFraud)
perf_nb <- performance(pred_nb, measure = "tpr", x.measure = "fpr")


# Naive Bayes Evaluation --------------------------------------------------



threshLogR <- 0.2943402923

Pos <- subset(res, res$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_nb > threshLogR)) ==1 ))/nrow(Pos)

Fal <- subset(res, res$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_nb > threshLogR)) ==1 ))/nrow(Fal)

TP <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_nb > threshLogR)) ==1 ))
FN <- nrow(Pos) - TP

FP <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_nb > threshLogR)) ==1 ))
TN <- nrow(Fal) - FP

TPR
FPR

TP
FN
FP
TN





