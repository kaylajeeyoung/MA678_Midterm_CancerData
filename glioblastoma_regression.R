source("glioblastoma_data.R")
pacman::p_load(rstanarm, arm, bayesplot, lme4, tidyverse, gridExtra, mixtools)

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

lm_4 <- lm(`Crude Rate` ~ age_group + sqrt(age_group) + Year, data = brain_incident)
summary(lm_4)
#plot(lm_4)

##the diagnostics look strange; try other regressions
stan_1 <- stan_glm(`Crude Rate` ~ age_group + Year, data = brain_incident,refresh=0)
stan_1_pred  <- posterior_predict(stan_1)
ppc_dens_overlay(brain_incident$`Crude Rate`,stan_1_pred[1:50,])


fit <- lmer(`Crude Rate` ~ age_group + sqrt(age_group) + Year + 
                   (1 + Year | States), data = brain_incident)
print(fit, digits=2)
fixef(fit)
ranef(fit)[[1]][1:10,]
brain_incident$fit <- predict(fit)
lmer_inc_plot <- ggplot(brain_incident) + 
  geom_point(aes(x = `Crude Rate`, y = fit, color = age_group)) + 
  ggtitle("Predicted vs Observed Incidence") + xlab("Observed") + ylab("Predicted") +
  theme_bw() +theme(legend.position = "none")
lmer_inc_diagnostic <- ggplot(brain_incident) + 
  geom_jitter(aes(x = age_group, y = fit, color = Year)) + 
  ggtitle("Predicted Values") + ylab("Predicted Rate") + theme_bw()

fitb <- lmer(`Crude Rate` ~ age_group + sqrt(age_group) + Year + 
               (1 + Year | State), data = brain_death)
brain_death$fit <- predict(fitb)
lmer_death_plot <- ggplot(brain_death) + 
  geom_point(aes(x = `Crude Rate`, y = fit, color = age_group)) + 
  ggtitle("Predicted vs Observed Death") + xlab("Observed") + ylab("Predicted") +
  theme_bw() + 
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"), legend.box.just = "right")

fit2 <- stan_glm(`Crude Rate` ~ age_group + Year + Region, data = region_incident, refresh = 0)
summary(fit2)
stan_2_pred <- posterior_predict(fit2)
ppc_dens_overlay(region_incident$`Crude Rate`, stan_2_pred[1:50,])

fit3 <- stan_glm(`Crude Rate` ~ age_group + Year + Region, data = region_incident, 
                 refresh = 0)

#fit4 <- stan_glm(`Crude Rate` ~ age_group + Year + Region, data = region_incident, 
#                 family="inverse.gaussian", refresh = 0)
#summary(fit4)

#mix_model <- spEMsymloc(age_group, mu = c(1, 75))

#mix_reg <- regmixEM(y = region_incident$`Crude Rate`, 
#                    x = cbind(region_incident$age_group,region_incident$Year))

#mix_reg$beta


##########
