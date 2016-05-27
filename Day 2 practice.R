---
  title: "Linear models"
author: "Richard J. Telford"
date: "May 26, 2016"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Linear Modelling Exercises

#1. Import the data found in bird1.csv
a.df<-read.csv("O:/Richard files Bio302-master/Bio302-master/Bio302/data/bird1.csv")
head(a.df)

#2. Fit a linear model, and interpret the coefficients.
fit.lm<-lm(weight~temp, data=a.df)
summary(fit.lm)

#3. Examine the diagnostics plot. Is everything OK?
x11()
par(mfrow=c(2,2))
plot(fit.lm)

# 3. Calculate the residual sum of squares 
#(find sum the squared difference between the estimates calculated from the coefficients and the observed values).
sum((a.df$weight - fit.lm$fitted.values)^2) #SSR 
b.s<-(a.df$weight - fit.lm$fitted.values)
b.sq<-b.s^2
sum(b.sq)

# Sum of Square of Residuals
sum((a.df$weight- (fit.lm$coefficients[1] + fit.lm$coefficients[2]*a.df$temp))^2) 

#4. Recalculate the Residual sum of squares using different values for 
    #the coefficients (in the formula yi = Beta + Beta xi + εi).
sum((a.df$weight- (17 + fit.lm$coefficients[2]*a.df$temp))^2)  
sum((a.df$weight- (5 + fit.lm$coefficients[2]*a.df$temp))^2) 

#5. Is the new RSS larger or smaller?
# larger than previous one

# 6. Estimate the slope using covariance and variance.
   #Slope:
   #β1 = Σ(xi – μx)(yi – μy) / √(Σ(xi – μx)2Σ(xi – μx)2)
   #β1 = Sxy / √(Sx2Sx2)
   
   #intersept 
   #β0 = μy - β1μx

wtCov<- with(a.df, cov(weight, temp)) #Covariance
tVar<- with(a.df, var(temp)) #variance
Slope.lm<- wtCov/sqrt(tVar*tVar) # slope, B1 estimating
Slope.lm

#7. Do you get the same result?
#YES

#8. Create a linear model (one-way anova) with the factor of C & T as the predictor
fit.lm1<- lm(weight~treat, data=a.df)
anova(fit.lm1)
anova(fit.lm)

#9. Compare the SST with the regression model.
Sum of Sq Total=SS Residual + SS Regress
SStot = Σ(yi – ŷ)2
SSres = Σ(yi – (β0 + β1xi))2
SSreg = Σ(β0 + β1xi - ŷ)2

SStot = SSreg + SSres
# Sum of Square
sum((a.df$weight -  (fit.lm1$fitted.values))^2) # SS residual
sum((a.df$weight -  mean(fit.lm1$fitted.values))^2) # SS Total
sum((fit.lm$fitted.values -  mean(fit.lm1$fitted.values))^2) # SS Regression

anova(fit.lm)
SSres<-sum((a.df$weight-(fit.lm1$coefficients[1]+fit.lm1$coefficients[2]*a.df$treat))^2)
# for temperature 
SSres<-sum((a.df$weight - (fit.lm$coefficients[1] + fit.lm$coefficients[2]*a.df$temp))^2)
SSreg<-sum((fit.lm$coefficients[1] + fit.lm$coefficients[2]*a.df$temp - mean (fit.lm$fitted.values))^2)
SStot<-SSres+SSreg
SStot
anova(fit.lm)

#10. Are there any difference.
#NO

#12. How should the coefficients be interpreted?
# The change in temperature has positive impact with weight

#13. Import the data found in bird2.csv
b.df<-read.csv("O:/Richard files Bio302-master/Bio302-master/Bio302/data/bird2.csv")
head(b.df)

#14. Fit a linear model with both temperature and gender as predictors.
b.lm<-lm(weight~temp*gender, data=b.df)
summary(b.lm)

#15. How should we interpret the results?
# there is no significant difference of weight for male and female
# 
16. What is the difference in intercept between male and female birds?

17. How much does the slope differ between male and female?
18. Is this significant?

