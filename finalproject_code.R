library(psych)
library(pastecs)
library(ggplot2)
library(ggplotgui)
library(rstanarm)
library(loo) 
library(dplyr)
library(tidybayes)

set.seed(200)
N=200 # number of survey respondents

## 1) Set the intercept ##

intercept=0 ## mean value of lidar use when risk perception equals zero?

## 2) Set the predictor variables ##
severity_flood= sample(1:5, N, replace=TRUE) # range of severity of floods, 1 being minor and 5 being disastrous
worst_flood= sample(1:5, N, replace=TRUE) # range of severity of floods, 1 being minor and 5 being disastrous
currentmap= sample(1:2, N, replace=TRUE) # questions asks if respondents thinks their floodmaps accurately reflect their flood risk: 1 means no and 2 means yes
incr_no_flood= sample(1:3, N, replace=TRUE) # this questions asks if respondents think the number of floods in their community is changings: 1 means increase, 2 means decrease, 3 means stay the same
incr_sev_flood= sample(1:3, N, replace=TRUE) # this questions asks if respondents think the severity of floods in their community is changings: 1 means increase, 2 means decrease, 3 means stay the same
prepared= sample(1:5, N, replace=TRUE) #this questions asks if respondents think their community is preparead for a flood event: 1 means completely prepared and 5 means completely unprepared
gender= sample(1:3, N, replace=TRUE) # gender for this is: 1 male, 2 female, and 3 other
age= sample(1:5, N, replace=TRUE) # age is categorical: where 1 is less than 20 years, 2:20-29 years, 3:30-39 years, 4:40-49 years, 5:50+ years
education= sample(1:6, N, replace=TRUE) # this is the level of education of respondent: 1: some high school, 2:high school diploma, 3: college education, did not garduate, 4: associate's degree, 5: bachelor's degreee, and 6: advanced degree
pol_party= sample(1:4, N, replace=TRUE) # there are options for the respondent to choose: 1: democrat, 2: independent, 3: republican, 4: other
science_trust= sample(1:5, N, replace=TRUE) # this question asks if respondent trusts science: 1: strongly trust to 5: strongly distrust
gov_trust= sample(1:5, N, replace=TRUE) # this question asks if respondent trusts government: 1: strongly trust to 5: strongly distrust

## 3) Potential correlations in data ##

# there is potential for incr_no_flood and incr_sev_flood to be correlated, so for the purposes of this example I am going to leave out the incr_sev_flood variable
# there is potential for age and pol_party to be correlated, so for the purposes of this example I am going to leave out age
# in addition, I am going to leave out education in order to simplify the number of variables I have in this simulation. 

## Simulating the response based on these variables ##
b1=.5
b2=0
b3=0
b4=0
#b5= 0
b6=0
b7=0
#b8= 0
#b9= 0
b10=.005
b11=.01
b12=.01
p <- intercept+(b1*severity_flood)+(b2*worst_flood)+(b3*currentmap)+(b4*incr_no_flood)+(b6*prepared)+(b7*gender)+(b10*pol_party)+(b11*science_trust)+(b12*gov_trust)
pr <- plogis(p)

## 4) Set the response variable ##

lidaruse <- rbinom(200,1,pr)

## 5) Combine data into dataframe ##

sim.survey <- data.frame(lidaruse, severity_flood, worst_flood, currentmap, incr_no_flood, prepared, gender, pol_party, science_trust, gov_trust)

write.csv(sim.survey, "sim_survey_results.csv")

# bring in the simulated data
sim.survey <- read.csv("sim_survey_results.csv")
# run the first model
sim.survey.fullmod <- stan_glm(lidaruse~ severity_flood+ worst_flood+ currentmap+ incr_no_flood+ prepared+ gender+ pol_party+ science_trust+ gov_trust, data=sim.survey, family="binomial")
# plot model
plot(sim.survey.fullmod, pars="beta")

# bring in the simulated data
sim.survey <- read.csv("sim_survey_results.csv")
# run the first model
sim.survey.fullmod <- stan_glm(lidaruse~ severity_flood+ worst_flood+ currentmap+ incr_no_flood+ prepared+ gender+ pol_party+ science_trust+ gov_trust, data=sim.survey, family="binomial")
# plot model
plot(sim.survey.fullmod, pars="beta")

sim.survey.mod1 <-  stan_glm(lidaruse~severity_flood+worst_flood+pol_party, data=sim.survey, family="binomial")

sim.survey.mod2 <- stan_glm(lidaruse~severity_flood+ worst_flood+ currentmap, data=sim.survey, family="binomial")

# look at the summary of model outputs
summary(sim.survey.fullmod)
summary(sim.survey.mod1)
summary(sim.survey.mod2)
# compare paramter estiamtes
plot(sim.survey.fullmod, pars="beta")
plot(sim.survey.mod1, pars="beta")
plot(sim.survey.mod2, pars="beta")
# compare the information criteria (loo for bayesian) to see which model loses the least amount of "realness"
loo_compare(loo(sim.survey.fullmod), loo(sim.survey.mod1), loo(sim.survey.mod2)) # loo compares all you to easily view the values relative to one another

# RMSE function:
rmse <- function(y, ypred) {
  rmse = sqrt(mean((y - ypred)^2))
  return(rmse)
}

#MAE function:
mae <- function(y, ypred) {
  mae = (mean(abs(y - ypred)))
  return(mae)
}

# Identify model's predicted yhat:
yhat.full <- posterior_predict(sim.survey.fullmod) 
yhat.full <- apply(yhat.full, 2, median) 
yhat.1 <- posterior_predict(sim.survey.mod1) 
yhat.1 <- apply(yhat.1, 2, median) 
yhat.2 <- posterior_predict(sim.survey.mod2) 
yhat.2 <- apply(yhat.2, 2, median) 
## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)? I am running for all three models to see how it changes
rmse(sim.survey$lidaruse, yhat.full)
mae(sim.survey$lidaruse, yhat.full)
rmse(sim.survey$lidaruse, yhat.1)
mae(sim.survey$lidaruse, yhat.1)
rmse(sim.survey$lidaruse, yhat.2)
mae(sim.survey$lidaruse, yhat.2)

# we want make a graph that shows the effect of severity_flood on lidar use while holding worst_flood and pol_party at their minimums
## Make a sequence of flood severity
sev.flood.gradient <- round(rep(seq(min(sim.survey$severity_flood),
                                    max(sim.survey$severity_flood),length.out=200),1)) 

worst.flood.min <- min(sim.survey$worst_flood) # min flood value
pol.party.min <- min(sim.survey$pol_party) # min political affiliation

preds.sev <- add_fitted_draws(sim.survey.mod1, 
                              newdata=data.frame(sev.flood.g=sev.flood.gradient,
                                                 worst.flood.m=worst.flood.min, # this is the min value of worst flood 
                                                 pol.party.m=pol.party.min ), # this is the min value of political party affiliation 
                              re_formula=NA,
                              draws = 200,  	type="response")

#This line loads the original data (actual collected points)
ggplot(preds.sev, aes(x=sev.flood.g,  
                      y=.value)) +  
  stat_lineribbon(.width = c(0.5, 0.95)) +   
  scale_fill_brewer(palette = "Greys") + 
  labs(y="Lidar Use", x = "Effect of community flood severity") +
  geom_point(data=sim.survey, aes(x=severity_flood,y=lidaruse)) +
  ggtitle("The effect of community flood severity on the number of LiDAR users") +
  theme_bw()  

mean(preds.sev$.value)
mean(sim.survey$severity_flood)

# we want make a graph that shows the effect of worst_flood on lidar use while holding sev_flood and pol_party at their minimums

worst.flood.gradient <- round(rep(seq(min(sim.survey$worst_flood),
                                      max(sim.survey$worst_flood),length.out=200),1)) 

sev.flood.min <- min(sim.survey$severity_flood)
pol.party.min <- min(sim.survey$pol_party)

preds.worst <- add_fitted_draws(sim.survey.mod1, 
                                newdata=data.frame(worst.flood.g=worst.flood.gradient,
                                                   sev.flood.m=sev.flood.min, 
                                                   pol.party.m=pol.party.min ), 
                                re_formula=NA,
                                draws = 200,  	type="response")

#This line loads the original data (actual collected points)
ggplot(preds.worst, aes(x=worst.flood.g,  
                        y=.value)) +  
  stat_lineribbon(.width = c(0.5, 0.95)) +   
  scale_fill_brewer(palette = "Greys") + 
  labs(y="Lidar Use", x = "Effect of the worst flood a community has seen") +
  geom_point(data=sim.survey, aes(x=worst_flood,y=lidaruse)) +
  ggtitle("The effect of the worst flood a community has seen on the number of LiDAR users") +
  theme_bw()   

mean(preds.worst$.value)
mean(sim.survey$worst_flood)

# we want make a graph that shows the effect of pol_party on lidar use while holding worst_flood and sev_flood at their minimums
## Make a sequence of flood severity
pol.party.gradient <- round(rep(seq(min(sim.survey$pol_party),
                                    max(sim.survey$pol_party),length.out=200),1)) 

preds.pol <- add_fitted_draws(sim.survey.mod1, 
                              newdata=data.frame(pol.party.g=worst.flood.gradient,
                                                 sev.flood.m=sev.flood.min, 
                                                 worst.flood.m=worst.flood.min ), 
                              re_formula=NA,
                              draws = 200,  	type="response")

#This line loads the original data (actual collected points)
ggplot(preds.pol, aes(x=pol.party.g,  
                      y=.value)) +  
  stat_lineribbon(.width = c(0.5, 0.95)) +   
  scale_fill_brewer(palette = "Greys") + 
  labs(y="Lidar Use", x = "Effect of political party association") +
  geom_point(data=sim.survey, aes(x=pol_party,y=lidaruse)) +
  ggtitle("The effect of policial party association on the number of LiDAR users") +
  theme_bw()  

mean(preds.pol$.value)
mean(sim.survey$pol_party)

