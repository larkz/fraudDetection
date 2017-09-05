library(psych)

# Sliding Window Function -------------------------------------------------

data.frame(value = tapply(cbind(salesOrderBlockedSingle$selling_price), list(cut(salesOrderBlockedSingle$ca, breaks="10 days")),sum))

library(heR.Misc)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
gmx <- function(x) {exp(mean(log(x)))}

sliderWindowSum <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), sum)))
}

sliderWindowCount <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), length)))
}

mostOccur <- function(inVec) {
  names(which.max(table(inVec)))
}

sliderWindowMode <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), mostOccur)))
}

sliderWindowMean <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), mean)))
}

sliderWindowGM <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), geometric.mean)))
}

sliderWindowMax <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), max)))
}

sliderWindowMin <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), min)))
}

sliderWindowSD <- function(timeVal, freqStr, vals){
  na.zero(data.frame(value = tapply(cbind(vals), list(cut(timeVal, breaks=freqStr)), sd)))
}
