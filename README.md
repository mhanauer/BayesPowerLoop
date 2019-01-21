---
title: "Bayes Loop Results"
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
library(psych)
```
###################
Now try varying N's
###################
Want to make this about whether an effect is significantly different from zero or not.  So do the HDI's not include zero
So the only way that zero can be included is if the lower 2.5 is negative and the upper 97.5 is the positive.
```{r}
matt_power_sample = function(){
intercept = 0
### Might need a for loop here
n = list(30,40,50,60,70,80,90)
intervention_out = list()
treat1v0 =  .2
y = list()
dat_out = list()
for(i in 1:length(n)){
  intervention_out[[i]]= c(rep(1,round(n[[i]]*.5,1)), rep(0,round(n[[i]]*.5,0)))
  y[[i]] = intercept + intervention_out[[i]]*treat1v0  + rnorm(n =n[[i]], mean = 0, sd = .3)
  dat_out[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]])
}
## Now grab the 2.5 and 97.5 and then 
post_prior = list()
cred_inter_2.5 = list()
cred_inter_97.5 = list()
cred_inter_sum = list()
cred_inter_power = list()
for(i in 1:length(dat_out)){
post_prior[[i]] = MCMCregress(y ~ intervention, b0 = .2, B0 = 1/.4^2,data = dat_out[[i]])
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
reps = 100
power_rep = replicate(reps, matt_power_sample())
power_unlist= unlist(power_rep)
n = c(30,40,50,60,70,80,90)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(n), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```
Now get the graph for power at different effect sizes
```{r}
sample_size = unlist(n)
power_dat = data.frame(sample_size, power)
power_dat
plot(power_dat, main = "Power with effect size of .2")
```
#############################
Varying effect sizes same N's
#############################
Want to make this about whether an effect is significantly different from zero or not.  So do the HDI's not include zero
So the only way that zero can be included is if the lower 2.5 is negative and the upper 97.5 is the positive.
```{r}
matt_power_effect = function(){
intercept = 0
n = 50
intervention  = c(rep(1,round(n*.5,1)), rep(0,round(n*.5,0)))
length(intervention)

treat1v0 =  list(.1,.2,.3,.4,.5)
y = list()
intervention_out = list()
dat_out = list()
for(i in 1:length(treate2v1)){
  y[[i]] = intercept + intervention*treat1v0[[i]]  + rnorm(n =n, mean = 0, sd = .4)
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
reps = 100
power_rep = replicate(reps, matt_power_effect())
power_unlist = unlist(power_rep)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(treat1v0), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```
Now get the graph for power at different effect sizes
```{r}
treat1v0 =  c(.1,.2,.3,.4,.5)
power_dat = data.frame(treat1v0, power)
power_dat
plot(power_dat)
```
Type one error rate for sample size
Need the probability that we reject the null hypothesis, so it is the same?
```{r}
matt_power_sample_type_one = function(){
intercept = 0
### Might need a for loop here
n = list(30,40,50,60,70,80,90)
intervention_out = list()
treat1v0 =  0
y = list()
dat_out = list()
for(i in 1:length(n)){
  intervention_out[[i]]= c(rep(1,round(n[[i]]*.5,1)), rep(0,round(n[[i]]*.5,0)))
  y[[i]] = intercept + intervention_out[[i]]*treat1v0  + rnorm(n =n[[i]], mean = 0, sd = .3)
  dat_out[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]])
}
## Now grab the 2.5 and 97.5 and then 
post_prior = list()
cred_inter_2.5 = list()
cred_inter_97.5 = list()
cred_inter_sum = list()
cred_inter_power = list()
for(i in 1:length(dat_out)){
post_prior[[i]] = MCMCregress(y ~ intervention, b0 = .2, B0 = 1/.4^2,data = dat_out[[i]])
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
reps = 100
power_rep = replicate(reps, matt_power_sample_type_one())
power_unlist= unlist(power_rep)
n = c(30,40,50,60,70,80,90)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(n), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```



