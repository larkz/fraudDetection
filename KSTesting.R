
# KS Testing --------------------------------------------------------------

sample1 <- sort(arrivDat)
sample2 <- sort(expectedSimAllTrans)
group <- c(rep("Arrivals", length(sample1)), rep("Expected Arrivals", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

plot(cdf1, verticals=TRUE, do.points=FALSE, col="blue", main = "Empirical Distribution test") 
plot(cdf2, verticals=TRUE, do.points=FALSE, col="green", add=TRUE) 
## alternatine, use standard R plot of ecdf 
#plot(f.a, col="blue") 
#lines(f.b, col="green") 

points(c(x0, x0), c(y0, y1), pch=16, col="red") 
segments(x0, y0, x0, y1, col="red", lty="dotted") 

ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=0.5) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Observations") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "solid", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
  ggtitle("K-S Test: Weekly Arrivals") +
  theme(legend.title=element_blank())


 graphKSTest <- function(arrivDat, expectedSimAllTrans, title){
   sample1 <- sort(arrivDat)
   sample2 <- sort(expectedSimAllTrans)
   group <- c(rep("Arrivals", length(sample1)), rep("Expected Arrivals", length(sample2)))
   dat <- data.frame(KSD = c(sample1,sample2), group = group)
   # create ECDF of data
   cdf1 <- ecdf(sample1) 
   cdf2 <- ecdf(sample2) 
   # find min and max statistics to draw line between points of greatest distance
   minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
   x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
   y0 <- cdf1(x0) 
   y1 <- cdf2(x0) 
   
   plot(cdf1, verticals=TRUE, do.points=FALSE, col="blue", main = "Empirical Distribution test") 
   plot(cdf2, verticals=TRUE, do.points=FALSE, col="green", add=TRUE) 
   ## alternatine, use standard R plot of ecdf 
   #plot(f.a, col="blue") 
   #lines(f.b, col="green") 
   
   points(c(x0, x0), c(y0, y1), pch=16, col="red") 
   segments(x0, y0, x0, y1, col="red", lty="dotted") 
   
   ggplot(dat, aes(x = KSD, group = group, color = group))+
     stat_ecdf(size=0.5) +
     theme_bw(base_size = 28) +
     theme(legend.position ="top") +
     xlab("Observations") +
     ylab("ECDF") +
     #geom_line(size=1) +
     geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                  linetype = "solid", color = "red") +
     geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
     geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
     ggtitle(title) +
     theme(legend.title=element_blank())
   
 }
