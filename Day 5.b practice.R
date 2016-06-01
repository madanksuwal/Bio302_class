---
  title: "Mixed Effect Models"
author: "Richard J. Telford"
date: "June 1, 2016"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown
Exercises 

# 1) Import data LME.csv <br>
    ##LME
  # 1. Import sorich.csv
  ```{r}
if(interactive()){
  lme.df <- read.csv("data/LME.csv")
}else{
  lme.df <- read.csv("../../data/LME.csv")
}
str(lme.df)
```
lme.df$ab<-as.factor(lme.df$ab)
lme.df$AB<-as.factor(lme.df$AB)
#id gives the group id, y is the response, ab and AB are predictors

#2) What is a random effect?
   # Random Effect = "id"

#3) What is a fixed effect?
   # fixed effect = "ab" and "AB"

#4) Fit an appropriate a model with y and AB
library(nlme)
fit.n<- lme(y ~ AB, random = ~ + 1|id, data = lme.df)
summary (fit.n)
anova (fit.n, test ="m")

fit.na<-glm(y~AB, data = lme.df, family= poisson)
summary(fit.na)
anova (fit.na, test ="Chi")

#5) Interpret the results
  # there is no significant effect of AB on Y
  # the model is not significant.
  # there is no cluster (tank) effect
  # GLM is appropriate with the provided data

#6) Fit an appropriate a model with y and ab
fit.n1<- lme (y~ab, random = ~+1|id, data = lme.df)
summary(fit.n1)
anova (fit.n1)
#plot(lme.df$ab, lme.df$y)

#7) Interpret the results
  # the model is significant, there is a relation between y and ab 
  # cluster (tank) effect is larger than individual (residual)

#8) Include both AB and ab as predictors and do a backward elimination

fit.m <- lme(Informed.liking ~ Gender+Information+Product +(1|Consumer), data=ham)
fit.m<- lme (y~ab + AB + ab:AB, random= ~+1|id, data = lme.df)
summary(fit.m)
anova(fit.m)
step(fit.m)

library(lmerTest)
stepAIC(fit.m)

#9) What type of contrast is used?



10) What is the expected mean for the different levels of AB and ab?

#11) Use a linear model with AB and compare the results to question 4
fit.na<-glm(y~AB, data = lme.df, family= poisson)
summary(fit.na)
anova (fit.na, test ="Chi")
   # GLM is significant

#12) Use a linear model with ab and compare the results to question 6
fit.n1a<- glm (y~ab, data = lme.df, family=poisson)
summary(fit.n1a)
anova (fit.n1a, test="Chi")
  # GLM not significant

#15) Import data length.csv <br>
  id is the individual id, day is the predictor, length is the response
```{r}
if(interactive()){
  lme.df1 <- read.csv("data/length.csv")
}else{
  lme.df1 <- read.csv("../../data/length.csv")
}
str(lme.df1)

fit1.ln<- lme (len~days, random = ~+1|id, data = lme.df1)
summary(fit1.ln)


16) Find the best model to explain the data