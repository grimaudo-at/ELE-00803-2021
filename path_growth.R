library(lme4)
library(brms)

dat<-read.csv("path_growth.csv")
dat$site<-as.factor(dat$site)
dat$origin_site<-as.factor(dat$origin_site)
dat$unique.cage<-as.factor(dat$unique.cage)

# Model of pathogen growth using 'site', 'origin site', and their interaction as explanatory variables and 
# caged ID as a random effect: 

m1<-lmer(d.lgdL.pg~site*origin_site+(1|unique.cage), data=dat);summary(m1)
# Singularity issue, suggestive of overly complex random effects structure. 

# Remove random effect: 
m2<-lm(d.lgdL.pg~site*origin_site, data=dat);summary(m2)
# Model reported in the manuscript. 

# m1 re-ran in Bayesian framework to test for significantly different results 
# with inclusion of random effects:

pr<-set_prior("normal(0,5)")
# Setting weakly informed priors. 

b.load<-brm(d.lgdL.pg~site*origin_site+(1|unique.cage), data=dat, family=gaussian(), prior=pr, )
summary(b.load)
# Qualitatively, these are the same results as m1, which omitted the Cage ID random effect. 
# This suggests that the removal of the random effect term did not change model interpretations. 

