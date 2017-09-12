
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


trainSummaryDataEncoded <- trainSummaryDataEncoded[complete.cases(trainSummaryDataEncoded), ]



md1 = glm(isFraud ~ b1 
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
              + v9, data = trainSummaryDataEncoded, family = binomial)

nothing <- glm(isFraud ~ 1, data = trainSummaryRegress ,family=binomial)
summary(nothing)



step(md1, scope=list(lower=formula(md1),upper=formula(nothing)), direction="backward")


step(md1)

