library(psych)
library(pastecs)
library(ggplot2)
library(ggplotgui)
library(rstanarm)
library(loo) 
library(dplyr)
library(tidybayes)
library(brms)
library(graphics)
library(ggpubr)

set.seed(200)
N=2000 # number of survey respondents

## 1) Set the intercept ##

intercept=-5 ## mean value of lidar use when risk perception equals zero?

## 2) 

## Downsized your predictors here, to reflect the ones you focused on in your paper:
severity_flood= sample(1:5, N, replace=TRUE) 
worst_flood= sample(1:3, N, replace=TRUE)
incr_no_flood= sample(1:3, N, replace=TRUE)
currentmap= sample(0:1, N, replace=TRUE) # changed this to 0 and 1, since it's a binary question

prepared= sample(1:5, N, replace=TRUE) 
gender= sample(1:3, N, replace=TRUE) # remember to use as factor, since it's not ordered.
pol_party= sample(1:4, N, replace=TRUE) # also a factor/not ordered.

science_trust= sample(1:5, N, replace=TRUE) 
gov_trust= sample(1:5, N, replace=TRUE) 

## The section I forwarded you from McElreath's book describes why using "ordered categories" as IVs or response vars is a bit different than assuming they are continuous with a true numerical value. In other words, the difference between 1 and 4 in your science trust category doesn't actually mean 3 units of change, since they are these categorical units. But if you just put them in as numbers, the model is assuming that there are values of science trust between 1 and 4. This is a fine approach for now, but as you collect your real survey data, you will want to consider these ordered categories.

## 3) 
## These slopes were based on my hypotheses and certain trends I am trying to pull out which are that direct experiences have a larger effect than any other risk perception predictors. Beliefs also play a significant role. 
b1=1 #severity_flood
b2=.8 #worst_flood
b3=0 #incr_no_flood
b4=0 #current_map
b5= 0 #prepared
b6 = -0.1 #gender
b7=-0.1 #pol_party
b8=0.5 #science_trust
b9=0.3 #gov_trust

p <- intercept+(b1*severity_flood)+(b2*worst_flood)+(b3*currentmap)+(b4*incr_no_flood)+(b5*prepared)+(b6*gender)+(b7*pol_party)+(b8*science_trust)+(b9*gov_trust)
pr <- plogis(p)

## 4) Set the response variable ##

lidaruse <- rbinom(N,1,pr)  ## Bumped up the number of simulated responses so that we can recover the parameters. You can set this back to a lower, more realistic number, but because we can't recover the pars as well when the sample size is smaller, wanted to give you a sense of the extent to which we can identify them when sample is large.

## 5) Combine data into dataframe ##

sim.survey <- data.frame(lidaruse, severity_flood, worst_flood, currentmap, incr_no_flood, prepared, gender, pol_party, science_trust, gov_trust)

# ASW: Be sure to include the variables that are meant to be factors accordingly.Thank you for this reminder!
sim.survey.fullmod <- stan_glm(lidaruse~ severity_flood+ worst_flood+ currentmap+ incr_no_flood+ prepared+ as.factor(gender)+ as.factor(pol_party)+ science_trust+ gov_trust, data=sim.survey, family="binomial")

# plot model
plot(sim.survey.fullmod, pars="beta")
plot(sim.survey.fullmod, "areas", prob=0.95)
posterior_interval(sim.survey.fullmod, prob=0.95)
plogis(posterior_interval(sim.survey.fullmod, prob=0.95))

## let's look at the model parameters and see how the model is picking them back up: 
summary(sim.survey.fullmod)

## Model fit with bayes R2:
median(bayes_R2(sim.survey.fullmod))

### Now let's look at other models to see how including and excluding certain variables effects model fit

## Model 1: variables whose 90% credible interval did not overlap with zero 
sim.survey.mod1 <- stan_glm(lidaruse~ severity_flood+ worst_flood+ science_trust+ gov_trust, data=sim.survey, family="binomial")

## Model 2
sim.survey.mod2 <- stan_glm(lidaruse~ severity_flood+ worst_flood, data=sim.survey, family="binomial")

### Model Comparison
# LOOIC comparison
loo_compare(loo(sim.survey.fullmod), loo(sim.survey.mod1), loo(sim.survey.mod2)) # loo compares all you to easily view the values relative to one another
# based on the LOOIC comparison, Mod1 has the greatest comparative out-of-sample predictive performance. 
# R2 comparison
#median(bayes_R2(sim.survey.fullmod))
median(bayes_R2(sim.survey.mod1))
#median(bayes_R2(sim.survey.mod2))

### Best fit model: Mod1

# Now I am going to focus on analyzing this specific model. 

## Compare with non-zero model to see if there is a difference
zip.model <- brm(data = sim.survey, family = zero_inflated_binomial,
                 lidaruse ~ severity_flood+ worst_flood+ science_trust+ gov_trust)
summary(zip.model)

# compare non-zero and orignial model
pp_check(zip.model) 
pp_check(sim.survey.mod1)
pp_check(sim.survey.mod1, plotfun="ppc_hist", nreps=3)+
  labs(x = "Lidar Use", y = "Number of Lidar Users") +
  theme_bw() 

## b) Compare the two models on the basis of their LOOIC scores. Which appears to have the better estimated out of sample predictive ability?
#loo_compare(loo(zip.model), loo(sim.survey.mod1))
# results show that the sim.survey.mod1 has the lowest OOS prediction error

## Examine priors
prior_summary.stanreg <- prior_summary(sim.survey.mod1)
summary(sim.survey.mod1)

# set the priors
mod1.priors <- stan_glm(lidaruse~ severity_flood+ worst_flood+ science_trust+ gov_trust, data=sim.survey,
                     prior=normal(0.85, 0.5), ## Set prior for betas here
                     prior_intercept=normal(-5.9,0.4), # set prior for intercept here
                     family="binomial")

#loo_compare(loo(sim.survey.mod1), loo(mod1.priors))

#Model statistics
plot(sim.survey.mod1, pars="beta")
plot(sim.survey.mod1, "areas", prob=0.95)
posterior_interval(sim.survey.mod1, prob=0.95)
plogis(posterior_interval(sim.survey.mod1, prob=0.95))

## Create simulated data for the variable we'd like to examine:

## Flood severity 

severity.sim <-  rep(c(1:5), each=400)

# set other predictor variables to their lowest affect at their minimum

simdata <-add_fitted_draws(newdata=data.frame(severity_flood= severity.sim,
                                              worst_flood=1,
                                              science_trust=1,
                                              gov_trust=1), 
                           sim.survey.mod1) 

## Plot the results 
sev.plot <- ggplot(simdata, aes(as.factor(severity_flood), .value)) + 
  geom_boxplot() + 
  labs(x = "Flood severity experienced \nby survey respondent", y = "Effect on probability of LiDAR adoption \n while holding other IVs at lowest values") +
  theme_bw() 
  
## Worst flood
worst.sim <-  rep(c(1:5), each=400)

# set other predictor variables to their lowest affect at their minimum

simdata1 <-add_fitted_draws(newdata=data.frame(worst_flood= worst.sim,
                                              severity_flood=1,
                                              science_trust=1,
                                              gov_trust=1), 
                           sim.survey.mod1) 

## Plot the results 
worst.plot <- ggplot(simdata1, aes(as.factor(worst_flood), .value)) + 
  geom_boxplot() + 
  labs(x = "Worst flood experienced \nby survey respondent", y = "Effect on probability of LiDAR adoption \n while holding other IVs at lowest values") +
  theme_bw() 

## Trust in science
science.sim <-  rep(c(1:5), each=400)

# set other predictor variables to their lowest affect at their minimum

simdata2 <-add_fitted_draws(newdata=data.frame(science_trust= science.sim,
                                              worst_flood=1,
                                              severity_flood=1,
                                              gov_trust=1), 
                           sim.survey.mod1) 

## Plot the results 
science.plot <- ggplot(simdata2, aes(as.factor(science_trust), .value)) + 
  geom_boxplot() + 
  labs(x = "Trust in science experienced \nby survey respondent", y = "Effect on probability of LiDAR adoption \n while holding other IVs at lowest values") +
  theme_bw() 

## Trust in government
gov.sim <-  rep(c(1:5), each=400)

# set other predictor variables to their lowest affect at their minimum

simdata3 <-add_fitted_draws(newdata=data.frame(gov_trust= gov.sim,
                                              worst_flood=1,
                                              science_trust=1,
                                              severity_flood=1), 
                           sim.survey.mod1) 

## Plot the results 
gov.plot <- ggplot(simdata3, aes(as.factor(gov_trust), .value)) + 
  geom_boxplot() + 
  labs(x = "Trust in government experienced \nby survey respondent", y = "Effect on probability of LiDAR adoption \n while holding other IVs at lowest values") +
  theme_bw() 

# plot graphs all together 

ggarrange(sev.plot, worst.plot, science.plot, gov.plot, 
          labels= c("A", "B", "C", "D"), 
          ncol = 2, nrow = 2 )

### Assess predictive preformance
# run a null model

sim.survey.nullmod <- stan_glm(lidaruse~1, data=sim.survey, family="binomial")

# compare LOOIC values

#loo_compare(loo(sim.survey.nullmod), loo(sim.survey.mod1))
# results display that mod1 has lowest OOS prediction error

## Compilation of all loo_compare values

loo_compare(loo(sim.survey.mod1), loo(sim.survey.fullmod), loo(sim.survey.mod2), loo(sim.survey.null), loo(mod1.priors))