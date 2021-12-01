source("glioblastoma_data.R")
library(rstanarm)

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
ppc_dens_overlay(brain_incident$`Crude Rate`,postt[1:50,])




