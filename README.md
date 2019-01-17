---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages
```{r}
library(MCMCpack)
library(descr)
library(ggplot2)
library(BEST)
library(psych)
```
Want to make this about whether an effect is significantly different from zero or not.  So do the HDI's not include zero
So the only way that zero can be included is if the lower 2.5 is negative and the upper 97.5 is the positive.
```{r}
matt_power_effect = function(){
intercept = 0
n = 200
intervention  = c(rep(1,round(n*.5,1)), rep(0,round(n*.5,0)))
length(intervention)

treat1v0 =  list(.1,.2,.3,.4,.5)
y = list()
intervention_out = list()
dat_out = list()
for(i in 1:length(treate2v1)){
  y[[i]] = intercept + intervention*treat1v0[[i]]  + rnorm(n =n, mean = 0, sd = .2)
  intervention_out[[i]]  = intervention
  dat_out[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]])
}
## Now grab the 2.5 and 97.5 and then 
post_prior = list()
cred_inter_2.5 = list()
cred_inter_97.5 = list()
cred_inter_sum = list()
cred_inter_power = list()
for(i in 1:length(dat_out)){
post_prior[[i]] = MCMCregress(y ~ intervention,data = dat_out[[i]])
post_prior[[i]] = summary(post_prior[[i]])
cred_inter_2.5[[i]] = post_prior[[i]]$quantiles[2,c(1)]
cred_inter_97.5[[i]] = post_prior[[i]]$quantiles[2,c(5)]
cred_inter_2.5[[i]] = ifelse(cred_inter_2.5[[i]] < 0,1,0)
cred_inter_97.5[[i]] = ifelse(cred_inter_97.5[[i]] > 0,1,0)
cred_inter_sum[[i]] = sum(cred_inter_2.5[[i]], cred_inter_97.5[[i]])
cred_inter_power[[i]] = ifelse(cred_inter_sum[[i]] < 2, 1,0)
}
return(cred_inter_power)
}

```
Now try to rep the function
```{r}
reps = 10
power = replicate(reps, matt_power_effect())
power
```


Power for treatment two versus treatment one
This is observed power 
```{r}
matt_power = function(){
n = 109
intercept = -0.4215
treat2v1 = .583
treat3v1 = .815
intervention  = c(rep(1,round(n*42/109,0)), rep(2,round(n*38/109,0)), rep(3,round(n*(29)/109,0)))
length(intervention)

intervention1 = ifelse(intervention == 1, 1, 0)
intervention2 = ifelse(intervention == 2,1,0)
intervention3 = ifelse(intervention == 3, 1, 0)

y = intercept + intervention2*treat2v1 + intervention3*treat3v1 + rnorm(n =n, mean = 0, sd = 1)

dat_bayes_power = data.frame(y = y, intervention2, intervention3)

post_prior = MCMCregress(y ~ intervention2 + intervention3,data = dat_bayes_power)

post_prior_summary = summary(post_prior)
cred_inter_2.5 = post_prior_summary$quantiles[2,c(1)]
cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

### Now we want to get rid of really wide CI so we want everything this lower than -.2 on the lower 2.5 and is higher than .2 on the 97.5
cred_inter_check = ifelse(cred_inter_97.5 > .2 & cred_inter_2.5 < -.2, 1,0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#
## This works, because the only way you can have two is if you have ones for both of the above.  Then if you get three, this means you also meet the criteria where you are above and below each threshold 
cred_inter = sum(cred_inter_2.5, cred_inter_97.5, cred_inter_check)
cred_inter = ifelse(cred_inter == 2, 1,0)
cred_inter
}

reps = 1000
power = data.frame(replicate(reps, matt_power()))
power = apply(power, 2, sum)/reps
power
```
