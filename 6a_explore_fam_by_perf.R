library(purrr)
library(stringr)
library(tidyverse)

library(ggplot2)
library(sjPlot)
library(sjmisc)
#fam_by_perf_df <- merge(fam_metrics, df_perf, by = 'log_id')


Peds_OR_Service <- map(unique(fam_by_perf_df$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk
Peds_OR_Service <- str_subset(Peds_OR_Service, 'OPH Pediatric Strabismus', negate = TRUE) # All of these patients appear to be quite old



fam_by_perf_df %>%
  filter(or_service %in% Peds_OR_Service) %>%
  #filter(or_service %in% c('Pediatric Cardiac Surgery')) %>%
  filter(between(team_size,4,15)) %>%
  ggplot() +
    aes(x = log(room_time), y = zeta_prime_52) +
    geom_point(size = 1L, colour = "#0c4c8a") +
    geom_smooth() +
    facet_wrap(~ asa_rating_c) #asa_rating_c
    theme_minimal()


fam_by_perf_df %>%
  filter(or_service %in% Peds_OR_Service) %>%
  filter(between(team_size,4,15)) %>%
  ggplot() +
    aes(x = room_time) +
    geom_histogram(bins = 100, fill = "#0c4c8a") +
    theme_minimal()

library(lme4)
library(lmerTest)
library(glmmTMB)

fam_by_perf_df <- merge(fam_metrics, df_perf, by = 'log_id')
fam_by_perf_df <- fam_by_perf_df %>%
  filter(or_service %in% Peds_OR_Service) %>%
  #filter(or_service != 'Pediatric Cardiac Surgery') %>%
  filter(between(team_size,4,15)) %>%
  filter(asa_rating_c != '5') %>%
  filter(asa_rating_c != 'NULL')# %>%
  #filter(caseclass == 'Elective')
  #filter(or_patientclass == 'Inpatient')

#fam_by_perf_df$room_time <- log(fam_by_perf_df$room_time)

lm_df <- fam_by_perf_df %>%
  select(room_time, los,primarysurgeonid, asa_rating_c, zeta_prime_52) %>%
  filter(complete.cases(.))
x.1 <- lmer(log(room_time) ~ 1 + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.2 <- lmer(log(room_time) ~ asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.3 <- lmer(log(room_time) ~ zeta_prime_52 + asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.4 <- lmer(log(room_time) ~ zeta_prime_52*asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
anova(x.1,x.2)
anova(x.2,x.3)

summary(x.1)
anova(x.1)

summary(x.2)
anova(x.2)

summary(x.3)
anova(x.3)

summary(x.4)
anova(x.4)


sjPlot::plot_model(x.4, type = 'int', show.p = TRUE, ci.lvl = NA) + theme_classic() + theme(legend.position=c(.8, .8)) #+ legend_style(inside = TRUE, pos = 'top right')# + theme_classic()
tab_model(x.1, x.2, x.3, x.4)
#########################
########## Power analysis
#########################

library(simr)
lm_df_pwr <- fam_by_perf_df %>%
  select(room_time, los,primarysurgeonid, asa_rating_c, zeta_prime_52, surgery_date) %>%
  filter(complete.cases(.))
lm_df_pwr$surgery_date <- as.factor(lm_df_pwr$surgery_date)
pwr.1 <- lmer(log(room_time) ~ 1 + (1|surgery_date), data = lm_df_pwr, REML = TRUE)
pwr.2 <- lmer(log(room_time) ~ asa_rating_c + (1|surgery_date), data = lm_df_pwr, REML = TRUE)
pwr.3 <- lmer(log(room_time) ~ zeta_prime_52 + asa_rating_c + (1|surgery_date), data = lm_df_pwr, REML = TRUE)
pwr.4 <- lmer(log(room_time) ~ zeta_prime_52*asa_rating_c + (1|surgery_date), data = lm_df_pwr, REML = TRUE)
anova(pwr.1,pwr.2)
anova(pwr.2,pwr.3)

summary(pwr.1)
anova(pwr.1)

summary(pwr.2)
anova(pwr.2)

summary(pwr.3)
anova(pwr.3)

summary(pwr.4)
anova(pwr.4)

pc <- simr::powerSim(pwr.3, nsim = 10, test = fixed('zeta_prime_52')) # fcompare(log(room_time) ~ zeta_prime_52 + asa_rating_c)
print(pc)

pwr.4_precov <- extend(pwr.4, along = 'surgery_date', n = 1800)
pc_precov <- simr::powerSim(pwr.4_precov, nsim = 10, test = fcompare(log(room_time) ~ zeta_prime_52 + asa_rating_c))
print(pc_precov)

pwr.4_postcov <- extend(pwr.4, along = 'surgery_date', n = 3000)
pc_postcov <- simr::powerSim(pwr.4_postcov, nsim = 10, test = fcompare(log(room_time) ~ zeta_prime_52 + asa_rating_c))
print(pc_postcov)
