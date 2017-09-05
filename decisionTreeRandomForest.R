# Decision Tree Constructor

library(rpart)

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
summaryRegress$isFraudFactor <- as.factor(summaryRegress$isFraudInt)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

summaryRegress[is.nan(summaryRegress)] <- 0


smp_size <- floor(0.75 * nrow(summaryRegress))

train_ind <- sample(seq_len(nrow(summaryRegress)), size = smp_size)

trainSummaryRegress <- summaryRegress[train_ind, ]
testSummaryRegress <- summaryRegress[-train_ind, ]


fit <- rpart(isFraud ~ b1 
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
             + v9, method="class", data=trainSummaryRegress)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# for illustration
library(party)
airct <- ctree(isFraudFactor ~ b1 
               + v9, data=trainSummaryRegress, 
               controls = ctree_control(maxsurrogate = 1))

plot(airct)  #default plot, some crowding with N hidden on leafs



plot(airct, type="simple",           # no terminal plots
     inner_panel=node_inner(airct,
                            abbreviate = FALSE,            # short variable names
                            pval = FALSE,                 # no p-values
                            id = TRUE),                  # no id of node
     terminal_panel=node_terminal(airct, 
                                  abbreviate = TRUE,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE)
)


### Decision Tree on all values

# for illustration

dtFullFit <- ctree(isFraudFactor ~ b1 
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
               + v9, data=trainSummaryRegress, 
               controls = ctree_control(maxsurrogate = 1))


printcp(dtFullFit) # display the results 
plotcp(dtFullFit) # visualize cross-validation results 
summary(dtFullFit) # detailed summary of splits








## CART Decision Tree

library(rpart)

# grow tree 
fit <- rpart(isFraud ~ b1 
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
             + v9,
             method="class", data=trainSummaryRegress)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

### Predictions

preds <- predict(fit, newdata = testSummaryRegress)
res <- cbind(testSummaryRegress, preds)
colnames(res)[which(names(res) == "1")] <- "prob_dt"

write.csv(res, file = "decisionTreePerformance.csv")
             

# Performance Evaluation


# Model Evaluation

threshLogR <- 0.02

Pos <- subset(res, res$isFraud ==1)
TPR <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_dt > threshLogR)) ==1 ))/nrow(Pos)

Fal <- subset(res, res$isFraud ==0)
FPR <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_dt > threshLogR)) ==1 ))/nrow(Fal)

TP <- nrow(subset(Pos, as.integer(as.logical(Pos$prob_dt > threshLogR)) ==1 ))
FN <- nrow(Pos) - TP

FP <- nrow(subset(Fal, as.integer(as.logical(Fal$prob_dt > threshLogR)) ==1 ))
TN <- nrow(Fal) - FP

TPR
FPR

TP
FN
FP
TN


## ROC Curve
library(ROCR)

pred_dt <- prediction(res$prob_dt, testSummaryRegress$isFraud)
perf_dt <- performance(pred_dt, measure = "tpr", x.measure = "fpr")

plot.new()



lwd1 <- 1.4
plot(perf_dt, col = "blue", lty = 1, lwd = lwd1, main = "ROC Plot Summary")
plot(perf_lrfull, col = "red", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_rf, col = "green", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_lrreg, col = "purple", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_nb, col = "orange", lty = 1, lwd = lwd1, add = TRUE)
legend("bottomright", legend = c("Decision Tree 1 (DT1)", 
                                 "Logistic Regression 2 (LR2)", 
                                 "Random Forest 1 (RF1)", 
                                 "Logistic Regression 1 (LR1)", 
                                 "Naive Bayesian Classifier 1 (NB1)"), 
                            lty = 1, 
                            lwd = 2, 
                            bty = 10, 
                            col = c("blue", "red", "green", "purple", "orange"))


lwd1 <- 1.4
plot(perf_dt, col = "blue", lty = 1, lwd = lwd1, main = "ROC Plot Summary - Low FPR", xlim = c(0, 0.05))
plot(perf_lrfull, col = "red", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_rf, col = "green", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_lrreg, col = "purple", lty = 1, lwd = lwd1, add = TRUE)
plot(perf_nb, col = "orange", lty = 1, lwd = lwd1, add = TRUE)
legend("bottomright", legend = c("Decision Tree 1 (DT1)", 
                                 "Logistic Regression 2 (LR2)", 
                                 "Random Forest 1 (RF1)", 
                                 "Logistic Regression 1 (LR1)", 
                                 "Naive Bayesian Classifier 1 (NB1)"), 
       lty = 1, 
       lwd = 2, 
       bty = 10, 
       col = c("blue", "red", "green", "purple", "orange"))









