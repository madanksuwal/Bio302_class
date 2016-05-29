setwd("O:/Richard files Bio302-master/Bio302/data")
read.csv("amount.csv")->a.df
head(a.df)

## Does heteroscedastic matter
#1. simulate some data with no relationship between the predictor and the response, but with changing variance
#- hint use `x <- 1:100`
#- `y <- rnorm(100, mean = 0, sd = seq(1, 10, length = 100))`

# Fit a model. Is it significant?
#3. Repeat this 1000 times. How many models are significant? How many would you expect?
#- hint use `replicate` extract p value from `anova`

x<-1:100
y<-rnorm(100, mean=0, sd=seq(1,10,length=100))
#plot(x,y)
fit.lm<-lm(y~x)
anova(fit.lm)
# replicated 1000 times and exract p-valu
p.value<- replicate(n=1000, expr = {
    x<-1:100
    y<-rnorm(100, mean=0, sd=seq(1,10,length=100))
    fit.lm <- lm(y~x)
    val.p <-anova(fit.lm)$"Pr(>F)"[1]
    })
p.value<-(round(p.value, 3))  
#transform p-values to TRUE FALSE
TF<-transform(p.value, x=p.value<=0.05)
head(TF)
#count number of Trues
table(TF$x)["TRUE"]


# NLS

#- Import data amount.csv
read.csv("amount.csv")->a.df
head(a.df)
plot(a.df$calcium, a.df$amount)
#- Do a non-linear regression
library(nlme)

fit.nls<- nls(amount~a+ c*exp(b*calcium), start = list(a=20, b=-0.5, c=2 ), data=a.df)
summary(fit.nls)
#anova(fit.nls)
plot(fit.nls)
plot(a.df$calcium, a.df$amount)
lines(a.df$calcium, fitted(fit.nls), lty = 2, col = "red", lwd = 2)


- Interpret the results
- What is the expected value if calcium = 10?
predict(fit.nls, newdata=data.frame(calcium=10))

###########################
#### session 2 ####

#1. Import bird1.csv
bird.df<-read.csv("bird1.csv")
head(bird.df)
#2. Fit a model using lm
b.lm<-lm(weight~temp, data=bird.df)
anova(b.lm)
summary(b.lm)

3. Pick a point and calculate the likelihood given the mean and residual 
# L(y;μ,σ) = {√(2πσ2)}-1 exp{(-2σ2)-1 (yi - μ)2} 
#σ = SD of residuals, 
#μ = expected value of Y
sg<-sd(b.lm$residuals)
mu<-b.lm$fitted.values
L<- (1 / ((sqrt(2*pi*sg^2))) * exp(1/(-2*sg^2) * (bird.df$weight - mu)^2)) # for whole set
L
# for single point
L13<- (1 / ((sqrt(2*pi*sg^2))) * exp(1/(-2*sg^2) * (bird.df$weight[13] - mu[13])^2)) # for 13th value
L13


#4. For the same point estimate the log-likelihood
# log(L(y;μ,σ))
L13log<- log(L13)
L13log

#5. Estimate the log-likelihood for all points
Llog<-log(L)
Llog

#6. Use different  regression coefficients and redo question 2 to 4
#original regression
fittedValue<- coef(b.lm)[1] + coef(b.lm)[2] * bird.df$temp
coef(b.lm)
#cofficient modify
new.fitted<- 5 + 0.6* bird.df$temp
Resid1 <- bird.df$weight - new.fitted
sum(Resid1^2)

sg2<-sd(modResid )
mu2<-new.fitted
L2<- 1 / ((sqrt(2*pi*sg2^2)) * exp(1/(-2*sg2^2) * (bird.df$weight - mu2)^2)) # for whole set
L2
cbind(L, L2)


#7. Use lm and glm to estimate the relationship of weight and temperature
names(bird.df)
b.glm<-glm(weight~temp, data=bird.df) #GLM
b.lm<-lm(weight~temp, data=bird.df)   #LM
plot(bird.df$temp, bird.df$weight)
lines(bird.df$temp, fitted (b.lm), col=2, lwd=2) 
lines(bird.df$temp, fitted (b.glm), col=3, lwd=2, lty=3) 
p13.l<-predict(b.lm, newdata = data.frame(temp=13))
p13.g<-predict(b.glm, newdata = data.frame(temp=13))

summary(b.glm)

#8. Compare the results


#9. Calculate the likelihood for the saturated model (mu~i~ = y~i~)


#10. Use the log-likelihoods to estimate the Deviance

    #D = 2{ILmax(y;μ,σ) - IL(y; y,σ)} #Deviance
    # for LM
I= sum(log(L))
y=bird.df$weight
sg<-sd(b.lm$residuals)
mu<-b.lm$fitted.values

IL<-log((1 / ((sqrt(2*pi*sg^2))) * exp(1/(-2*sg^2) * (bird.df$weight - bird.df$weight)^2)))
sum(IL)
   #D = saturate model Likelihood
D=2*(I-sum(IL))
D


11. What is the relationship between the Deviance and the sum of squared residuals, 
    assuming Normal distribution?

#need to work
