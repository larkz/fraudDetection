set.seed(101)

y <- as.matrix(summaryRegress$isFraud)
Z <- as.matrix(summaryRegress[ ,!(colnames(summaryRegress) == "isFraud")])
Z <- as.matrix(summaryRegress[, c("v1", "v2", "v3", "v4")])

g1 <- glm(y~Z, data.frame(y,Z),family="binomial")  


NRfit <- function(y,X,start,n.iter=10,tol=1e-4,verbose=TRUE) {
  ## used X rather than Z just because it's more standard notation
  n.pred <- ncol(X)
  B <-  matrix(NA,ncol=n.iter,
               nrow=n.pred)
  B[,1] <- start
  for (i in 2:n.iter) {
    if (verbose) cat(i,"\n")
    p <- plogis(X %*% B[,i-1])
    v.2 <- diag(c(p*(1-p)))
    score.2 <- t(X) %*% (y - p) # score function
    increm <- solve(t(X) %*% v.2 %*% X) 
    B[,i] <- B[,i-1]+increm%*%score.2
    #if (all(abs(B[,i]-B[,i-1]) < tol)) return(B)
  }
  B
}

matplot(res2 <- t(NRfit(y,Z,start=rep(0,ncol(Z))))  )
res2
g1
matplot(res1 <- t(NRfit(y,Z,start=coef(g1)))  )

all.equal(res2[6,],unname(coef(g1))) ## TRUE


###############







# sigmoid
g = function (z) {
  return (1 / (1 + exp(-z) ))
} # plot(g(c(1,2,3,4,5,6)))

# hypothesis 
h = function (x,th) {
  return( g(x %*% th) )
} # h(x,th)

# cost
J = function (x,y,th,m) {
  return( 1/m * sum(-y * log(h(x,th)) - (1 - y) * log(1 - h(x,th))) )
} # J(x,y,th,m)

# derivative of J (gradient)
grad = function (x,y,th,m) {
  return( 1/m * t(x) %*% (h(x,th) - y))
} # grad(x,y,th,m)

# Hessian
H = function (x,y,th,m) {
  return (1/m * t(x) %*% x * diag(h(x,th)) * diag(1 - h(x,th)))
} # H(x,y,th,m)





plot(summaryRegress$v1[summaryRegress$isFraud == 0], summaryRegress$v2[summaryRegress$isFraud == 0], xlab="test1", ylab="test2", , col="red")
points(summaryRegress$v2[summaryRegress$isFraud == 1], summaryRegress$v2[summaryRegress$isFraud == 1], col="blue", pch=2)


# setup variables
j = array(0,c(10,1))
m = length(summaryRegress$v1)
x = matrix(c(rep(1,m), summaryRegress$v2, summaryRegress$v4), ncol=3)
y = matrix(summaryRegress$isFraud, ncol=1)
th = matrix(0,3)
tt <- c( t(th))

# iterate 
# note that the newton's method converges fast, 10x is enough
for (i in 1:40) {
  j[i] = J(x,y,th,m) # stores each iteration Cost
  
  th <- th - solve(H(x,y,th,m)) %*% grad(x,y,th,m)
  tt <- rbind(tt, t(th) )
  print(tt)
}
cbind(0:40, tt)


g2 <- glm(isFraud ~ v2 + v4, summaryRegress, family="binomial")
g2













