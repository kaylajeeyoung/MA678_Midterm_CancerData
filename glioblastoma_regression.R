source("glioblastoma_data.R")
pacman::p_load(rstanarm, arm, bayesplot, lme4, tidyverse, gridExtra)

#try out some simple linear regressions 
##incidence
lm_1 <- lm(`Crude Rate` ~ Year, data = brain_incident)
summary(lm_1)
#plot(lm_1)

lm_2 <- lm(`Crude Rate` ~ Year + States, data = brain_incident)
summary(lm_2)
#plot(lm_2)

lm_3 <- lm(`Crude Rate` ~ age_group, data = brain_incident)
summary(lm_3)
#plot(lm_3)

lm_4 <- lm(`Crude Rate` ~ age_group + Year, data = brain_incident)
summary(lm_4)
#plot(lm_4)

##the diagnostics look strange; try other regressions
stan_1 <- stan_glm(`Crude Rate` ~ age_group + Year, data = brain_incident,refresh=0)
stan_1_pred  <- posterior_predict(stan_1)
ppc_dens_overlay(brain_incident$`Crude Rate`,stan_1_pred[1:50,])


fit <- lmer(`Crude Rate` ~ age_group + Year + 
                   (1 + Year | States), data = brain_incident)
print(fit, digits=2)
fixef(fit)
ranef(fit)[[1]][1:10,]

fit2 <- stan_glm(`Crude Rate` ~ age_group + Year + Region, data = region_incident, refresh = 0)
summary(fit2)
stan_2_pred <- posterior_predict(fit2)
ppc_dens_overlay(region_incident$`Crude Rate`, stan_2_pred[1:50,])



