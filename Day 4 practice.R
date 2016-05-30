---
  title: "GLM & GAM"
author: "Richard J. Telford"
date: "May 30, 2016"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
setwd("O:/Richard files Bio302-master/Bio302/data")
#Exercises GLM & GAM

##Poisson GLM
# 1. Import sorich.csv
sorich.df <- read.csv("sorich.csv")
head(sorich.df)

#2. Fit a linear model for species richness against cover assuming a Normal distribution
fit.lm<- lm(nsp~ cover , data = sorich.df)
summary (fit.lm)
anova(fit.lm, test = "F")

#3. Study the diagnostic plots
x11()
par (mfrow=c(2,2))
plot(fit.lm)
# non normal distirubtion, patter in first plot

#4. What distribution is the response?
head(sorich.df)
str(sorich.df)
sorich.df$grasherb<-as.factor(sorich.df$grasherb) #grasherb is factorical 0, 1 
str(sorich.df)
plot(sorich.df$cover, sorich.df$nsp)
str(sorich.df)
   # Non linear distribution 

#5. Do an appropriate analysis
fit.glm<- glm(nsp~ cover, data = sorich.df, family = "poisson")
summary(fit.glm)
anova(fit.glm)
 # Residual deviance is very high than Residual DF. 
 # hence go for quasi poisson
fit.qglm<- glm(nsp~ cover, data = sorich.df, family = "quasipoisson")
summary(fit.qglm)
anova(fit.qglm)

anova(fit.glm, fit.qglm, test ="Chi") # DONOT  do it


#GAM
library(mgcv)
fit.gam<- gam(nsp ~ s(cover), data = sorich.df, family = "poisson")
summary(fit.gam)
anova(fit.gam)
plot(fit.gam)
plot(sorich.df$nsp, fit.gam$residuals)

plot(fit.gam$fitted.values, fit.gam$residuals)
qq.gam(fit.gam, pch=16, cex=.5)
gam.check(fit.gam)

fit.qgam<- gam(nsp ~ s(cover), data = sorich.df, family = "quasipoisson")
summary(fit.qgam)
anova(fit.qgam)

anova(fit.gam, fit.qgam, test = "Chi")

anova(fit.qglm, fit.qgam, test="Chi")


#6. Check for over-dispersion
    # overly dispersed. 

#7. Interpret the results
  # GAM is better, due to over dispersion, family quasi-poisson is taken.
  # cover has significant effect on number of species (nsp)


#8. How does the width of the confidence interval at cover = 10 change when over dispersion is allowed for
pre<- predict(fit.qgam, newdata = data.frame (cover=10))
pre
newD<- seq (min(sorich.df$cover), max(sorich.df$cover), length = 100)
preD<- predict(fit.qgam, newdata = data.frame(cover=newD), se.fit=TRUE)
upL<-preD$fit+1.96*preD$se.fit # upper limit, 95 % confidence interval 
lowL<-preD$fit-1.96*preD$se.fit # lower limit, 95 % convidence interval
# for cover = 10
pre<- predict(fit.qgam, newdata = data.frame (cover=10), se.fit = TRUE)
# estimate 
exp(pre$fit)
exp(pre$fit + 1.96 * pre$se.fit) # upper limit for cover = 10
exp(pre$fit - 1.96 * pre$se.fit) # lower limit for cover = 10


#9. Do the grasses differ from the herb, i.e. include the factor grasherb and test its significance?
fit.qgam1<- gam(nsp ~ s(cover)+ (grasherb) , data = sorich.df, family = "quasipoisson")
summary(fit.qgam1)
anova(fit.qgam1)

 # Grass Herb is significant
  
#head(sorich.df)
## How much does over-dispersion affect results
1. Use `sample()` to randomise the response in sorich
2. Test if a Poisson GLM with cover as a predictor is significant
3. Repeat 1000 times with `replicate`

## Binomial GAM

1. Open library mgcv
2. Import data pot.csv
3. What type of distribution is the response variable?
4. What type of link-function do we use?
5. Do an appropriate GLM analysis?
6. Interpret the results?
7. Do a GAM analysis
8. Compare the GLM and GAM models.
9. Which model would you prefer, and why?