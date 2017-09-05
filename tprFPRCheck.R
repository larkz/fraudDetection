


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

featureListBin <- c("xb1",
                    "xb2",
                    "xb3",
                    "xb4",
                    "xb5")

for (n in featureListBin){
  TPRFPRarray <- rbind(TPRFPRarray, c( n,tprCheckBinary( summaryRegress[[n]], summaryData$isFraudPred )))
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

featureList <- c("xv1",
                 "xv2",
                 "xv3", 
                 "xv4", 
                 "xv5", 
                 "xv6", 
                 "xv7", 
                 "xv8", 
                 "xv9")

TPRFPRarray <- c()

for (n in featureList){
  TPRFPRarray<- rbind(TPRFPRarray, tprCheck( summaryData[[n]], summaryData$isFraudPred ))
}
TPFPdf <- as.data.frame(cbind(featureList, TPRFPRarray))
