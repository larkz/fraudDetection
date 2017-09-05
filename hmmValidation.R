# Test and Validation
# Libraries

library(HMM)

########

load("assignK.RData")
load("assignKW.RData")

testCid <- as.data.frame(unique(assignK$customer_id))
colnames(testCid) <- c("customer_id")

# Join with logistic regression prediction
testSummaryRegress <- merge(summaryDataEncoded, testCid, by = "customer_id")

summary(fit_tsagg_end <- glm(isFraud ~ v1, data = summaryDataEncoded, family = binomial))

summary(fit_tsagg_end <- glm(isFraud ~ 
                               b1
                             + b2 
                             + b3 
                             + b4 
                             + b5, data = trainSummaryRegress, family = binomial))

prob <- predict(fit_tsagg_end, newdata=testSummaryRegress, type="response")
pred <- prediction(prob, testSummaryRegress$isFraud)
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
  ggtitle(paste0("ROC Curve on Registration Features AUC = ", auc))

logRlabReg <- cbind(testSummaryRegress, prob)  


# Marketplace: AssignK
# assignK <- read.csv(file = "mkvHistsHealthyClusMulti_3.csv")
load("assignK.RData")

[[ 0.34811563  0.58824686  0.06363751]
[ 0.34653656  0.60121859  0.05224485]
[ 0.30792853  0.40651322  0.28555825]]
[[  9.25670732e-01   2.82357360e-02   7.36505017e-04   4.53570265e-02]
[  9.85145142e-01   1.28094180e-03   1.52205406e-03   1.20518617e-02]
[  4.29358086e-01   2.20277563e-01   6.58337686e-02   2.84530582e-01]]


# Healthy Model (kmeans + 0 factor)

tm <- t(matrix(t(c( 0.34811563,  0.58824686,  0.06363751,
                    0.34653656,  0.60121859,  0.05224485,
                    0.30792853,  0.40651322,   0.28555825)), 3))

emat <-t(matrix(c(  9.25670732e-01,   2.82357360e-02,   7.36505017e-04, 4.53570265e-02,
                    9.85145142e-01,   1.28094180e-03,   1.52205406e-03, 1.20518617e-02,
                    4.29358086e-01,   2.20277563e-01,   6.58337686e-02, 2.84530582e-01), 4))

hmm_ <- initHMM(c("s0", "s1","s2"), c("none", "low", "med", "high"),  transProbs=tm, emissionProbs=emat)
forward(hmm_, c("none", "low", "none", "none"))

cid <- "00046ee478677cdcf14be70ad4c723edbd63fec2" #fraud
spend_seq <- subset(assignK, assignK$customer_id == cid)$k_class
# forward(hmm_, spend_seq)
# viterbi(hmm_, spend_seq)
exp(apply(forward(hmm_, spend_seq), 2, max))
diff(exp(apply(forward(hmm_, spend_seq), 2, max)))
head(abs(diff(exp(apply(forward(hmm_, spend_seq), 2, max))))/exp(apply(forward(hmm_, spend_seq), 2, max)), -1)

hmmProb <- function(cid){
  spend_seq <- subset(assignK, as.character(assignK$customer_id) == cid)$k_class
  if (length(spend_seq) > 1) {
    exp(apply(forward(hmm_, spend_seq), 2, max))
    diff(exp(apply(forward(hmm_, spend_seq), 2, max)))
    print(cid)
    p <- head(abs(diff(exp(apply(forward(hmm_, spend_seq), 2, max))))/exp(apply(forward(hmm_, spend_seq), 2, max)), -1)
    return( as.numeric(as.character(p)) )
  } else{
    return(NULL)
  }
} 

## Retrieve labels
load("labelMkt2.RData")

labelMkt <- merge(assignK, summaryData[c("customer_id", "fraud_label")], by ="customer_id")
labelMkt2 <- labelMkt

hmmProbMkt <- as.data.frame(cbind( lapply(unique(labelMkt2$customer_id), hmmProb)))
hmmProbMkt$customer_id <- unique(labelMkt2$customer_id)


max1 <- function(list){
  if(!is.null(list)){  return(max(list, na.rm = TRUE))}
  else { NULL}
}

min1 <- function(list){
  if(!is.null(list)){  return(min(list, na.rm = TRUE))}
  else { NULL}
}

hmmProbMkt$minProbMkt <- lapply(hmmProbMkt$V1, min1)
hmmProbMkt$maxProbMkt <- lapply(hmmProbMkt$V1, max1)

save(hmmProbMkt, file = "hmmProbMkt.RData")
# save(hmmEvalMkt, file = "hmmEvalMkt.RData")

unlisty <- function(list){
  return (unlist(list[[1]]))
}

# hmmEvalSubMkt <- hmmEvalMkt[,c("customer_id", "isFraud", "prob", "minProb")]
load("hmmProbMkt.RData")




##################################################

# Wallet: AssignKW

load("assignKW.RData")

lengs <- c()
hists <- c() 
i <- 0

max1 <- function(list){
  return(max(list))
}

min1 <- function(list){
  return(min(list))
}




[[ 0.26847652  0.24071921  0.49080427]
[ 0.22936469  0.3182654   0.45236991]
[ 0.2642623   0.24860105  0.48713665]]
[[  9.83326005e-01   2.26257772e-03   1.44114177e-02 0]
[  8.67539464e-01   3.91781017e-04   1.32068755e-01 0]
[  9.68462459e-01   2.81659098e-04   3.12558818e-02 0]]

# Healthy Model (kmeans + 0 factor)

tm <- t(matrix(t(c( 0.26847652,  0.24071921,  0.49080427,
                    0.22936469,  0.3182654,  0.45236991,
                    0.2642623,  0.24860105,   0.48713665)), 3))

emat <-t(matrix(c(  9.83326005e-01,   2.26257772e-03,   0, 1.44114177e-02,
                    8.67539464e-01,   3.91781017e-04,   0, 1.32068755e-01,
                    9.68462459e-01,   2.81659098e-04,   0, 3.12558818e-02), 4))

hmm_ <- initHMM(c("s0", "s1","s2"), c("none", "low", "med", "high"),  transProbs=tm, emissionProbs=emat)
forward(hmm_, c("none", "low", "none", "none"))




cid <- "805f64693917d01a75ea5a46b98a4ee170cbdeff" #fraud
spend_seq <- subset(assignKW, assignKW$customer_id == cid)$k_class
# forward(hmm_, spend_seq)
# viterbi(hmm_, spend_seq)
exp(apply(forward(hmm_, spend_seq), 2, max))
diff(exp(apply(forward(hmm_, spend_seq), 2, max)))
head(abs(diff(exp(apply(forward(hmm_, spend_seq), 2, max))))/exp(apply(forward(hmm_, spend_seq), 2, max)), -1)

hmmProb <- function(cid){
  spend_seq <- subset(assignKW, as.character(assignKW$customer_id) == cid)$k_class
  if (length(spend_seq) > 1) {
    # exp(apply(forward(hmm_, spend_seq), 2, max))
    # diff(exp(apply(forward(hmm_, spend_seq), 2, max)))
    print(cid)
    p <- head(abs(diff(exp(apply(forward(hmm_, spend_seq), 2, max))))/exp(apply(forward(hmm_, spend_seq), 2, max)), -1)
    return(as.numeric(as.character(p)))
  } else{
    return(NULL)
  }
} 

## Retrieve labels
# labelWal <- wallet_txn_effAll_withLabel[c("customer_id", "fraud_label")]

hmmProbWal <- lapply( unique(assignKW$customer_id), hmmProb)

hmmWal <-  lapply(unique(labelMkt2$customer_id), hmmProb)

hmmWal <- as.data.frame(cbind(hmmWal))
hmmWal$customer_id <- unique(assignKW$customer_id)

hmmWal$minProbWal <- lapply(hmmWal$hmmWal, min1)
hmmWal$maxProbWal <- lapply(hmmWal$hmmWal, max1)
save(hmmWal, file = "hmmWal.RData")

load("hmmWal.RData")

# labelWal$minProb <- lapply(labelWal$hmmProb, min1)
hmmEvalWal <- merge(hmmWal, logRlabReg, by = c("customer_id"))

unlisty <- function(list){
  return (unlist(list[[1]]))
}

# hmmEvalSubWal <- hmmEvalWal[,c("customer_id", "isFraud", "prob", "minProb")]
save(hmmEvalWal, file = "hmmEvalWal.RData")


## ROC on HMM

threshHMMWal <- 0.99
threshHMMMkt <- 0.99
threshLogR <- 0.11

#hmmEvalMkt$hmmProbMkt <- lapply(hmmEvalMkt$hmmProb, min1)
#hmmEvalMktJoin <-  hmmEvalMkt[,c("customer_id","hmmProbMkt")]

hmmEvalZeta <- merge(hmmEvalWal, hmmProbMkt, by = "customer_id", all.x = TRUE)
hmmEvalZetaSubset <- hmmEvalZeta[,c("customer_id", "isFraud", "minProbWal", "maxProbWal", "minProbMkt", "maxProbMkt", "prob")]

# hmmEvalZeta$hmmProbMkt[is.null(hmmEvalZeta$hmmProbMkt)] <- -Inf
# hmmEvalZeta$hmmProbMkt <- hmmEvalZeta[unlist(lapply(hmmEvalZeta$hmmProbMkt , is.null))] <- NA

save(hmmEvalZeta, file = "hmmEvalZeta.RData")


hmmEvalZeta$minProb <- as.numeric(as.character(hmmEvalZeta$minProb))
hmmWalProb <- as.numeric(as.character(hmmEvalZeta$minProb))
hmmEvalZeta$hmmMktProb <- hmmMktProb 

hmmWalProb[is.na(hmmWalProb)] <-  Inf

hmmMktProb <- as.numeric(as.character(hmmEvalZeta$hmmProbMkt))
hmmMktProb[is.na(hmmMktProb)] <-  Inf

hmmEvalZeta$hmmMktProb <- hmmMktProb 
hmmEvalZeta$hmmWalProb <- hmmProb 

# Evaluation

threshHMMWal <- 0
# threshHMMWal <- 0.343561530716134
threshHMMWal <- 0.998345826348647
threshHMMWal <- -Inf

# threshHMMMkt <- 0.579348830065737
threshHMMMkt <- 0.981074235416238
threshLogR <- 0.0027143245106007

Pos <- subset(hmmEvalZetaSubset, hmmEvalZetaSubset$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob > threshLogR))
                   + as.integer(as.logical(unlist(Pos$maxProbWal) < threshHMMWal)) 
                   + as.integer(as.logical(unlist(Pos$minProbMkt) < threshHMMMkt))  > 1 ))/nrow(Pos)

Fal <- subset(hmmEvalZetaSubset, hmmEvalZetaSubset$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob > threshLogR))
                   + as.integer(as.logical(unlist(Fal$maxProbWal) < threshHMMWal)) 
                   + as.integer(as.logical(unlist(Fal$minProbWal) < threshHMMMkt)) > 1 ))/nrow(Fal)
TPR
FPR



prob <- predict(fit_tsagg_end, newdata=testSummaryRegress, type="response")
pred <- prediction(prob, hmmEvalSubWal$isFraud)
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
  ggtitle(paste0("ROC Curve on Registration Features AUC = ", auc))

logRlabReg <- cbind(testSummaryRegress, prob)  













