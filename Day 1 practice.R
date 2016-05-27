# Practice Day 1
dir()

a.df<-read.csv("O:/Richard files Bio302-master/Bio302-master/data/combinedETE.csv", header=T, sep=',')
head(a.df)
names(a.df)
##Find the range, mean, median and variance of variable percAgg.
range(a.df$percAgg)
mean(a.df$percAgg)->paMean
median(a.df$percAgg)->paMedian
quantile(a.df$percAgg)
var(a.df$percAgg)
#or
summary(a.df$percAgg)
## What is the standard error of the mean of percAgg.
(sd(a.df$percAgg)/sqrt( length(a.df$percAgg) ))

##Plot a histogram of variable percAgg.
hist(a.df$percAgg)
## Use abline to mark the position of the mean and median.
abline(v=c((mean(a.df$percAgg)),(median(a.df$percAgg)) ), col=c(2,3), lty=c(1,2))
  # or
abline(v=mean(a.df$percAgg), lwd=2)
abline(v=median(a.df$percAgg), lty=2)
# by ggplot2 method
library("ggplot2")
g <- ggplot(a.df, aes(x = percAgg)) + 
  geom_histogram() +
  geom_vline(xintercept = c(mean = paMean, median = paMedian), colour = c(2, 4))
print(g) 
## Plot percAgg against numSites
plot(a.df$percAgg, a.df$numSites)
with(a.df, plot(numSites, percAgg, col=c(2,3), pch=c(16,17)), )

#Find the covariance and correlation of percAgg and numSites
cov(a.df$percAgg, a.df$numSites)
cor(a.df$percAgg, a.df$numSites)->cor.1
with(a.df, cor(percAgg, numSites))
with(a.df, var(percAgg, numSites))

#Is the correlation significant? (hint use cor.test())
cor.test(a.df$percAgg, a.df$numSites)
with(a.df, cor.test(percAgg, numSites))

with(subset(a.df, land.island == "ETE"), cor(percAgg, numSites))

by(a.df, a.df$land.island, function(x)cor.test(x$percAgg, x$numSites))

#How should the p-value be interpreted?
p-value less than 0.5, which is significant
