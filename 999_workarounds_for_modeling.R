#########################################
#########################################
########## Work arounds for modeling data on MAC & Running metrics on PC
#########################################
#########################################

#df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases.csv')
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/all_cases_w50per_rt.csv')

df_cmb <- df_cases %>%
  full_join(df_fam)

print(paste('# of cpt codes',length(unique(df_cmb$cpt))))
print(paste("# of cases...",nrow(df_cmb)))
df_cmb <- df_cmb %>%
  group_by(cpt) %>%
  filter(n() > 200) %>%
  ungroup()
print(paste('# of cpt codes',length(unique(df_cmb$cpt))))
print(paste("# of cases...",nrow(df_cmb)))

library(ggplot2)
library(dplyr)

p <- df_cmb %>%
  filter(facility == 'THE JOHNS HOPKINS HOSPITAL') %>%
  #mutate()
  ggplot(aes(y=zeta_52, x=team_size)) +
  geom_point() + 
  xlab("")
p

library(purrr)
library(stringr)
library(tidyverse)

library(ggplot2)
library(sjPlot)
library(sjmisc)

fam_by_perf_df <- df_cmb %>%
  mutate(room_time = as.numeric((out_or_dttm - in_or_dttm), units = "mins")) %>%
  mutate(los = as.numeric((disdate - surgery_date), units = 'days'))
Peds_OR_Service <- map(unique(fam_by_perf_df$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk
#Peds_OR_Service <- str_subset(Peds_OR_Service, 'OPH Pediatric Strabismus', negate = TRUE) # All of these patients appear to be quite old



fam_by_perf_df %>%
  filter(!(or_service %in% Peds_OR_Service)) %>%
  #filter(or_service %in% c('Pediatric Cardiac Surgery')) %>%
  #filter(between(team_size,4,15)) %>%
  filter(room_time > 15 & room_time < 1000) %>%
  filter(caseclass == 'Elective') %>%
  ggplot() +
  aes(x = zeta_prime_1, y = zeta_1) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth() +
  facet_wrap(~ asa_rating_c) +#asa_rating_c
  theme_minimal()


fam_by_perf_df %>%
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(room_time > 15 & room_time < 1000) %>%
  filter(caseclass == 'Elective') %>%
  #filter(between(team_size,4,15)) %>%
  ggplot() +
  #aes(x = zeta_prime_52) +
  aes(x = room_time) +
  #aes(x = slack_time) +
  #aes(x = log(ontime)) +
  #aes(x = log(los)) +
  #aes(x = log(room_time)) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

library(lme4)
library(lmerTest)
library(glmmTMB)


fam_by_perf_df <- fam_by_perf_df %>%
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(room_time > 15 & room_time < 1000) %>%
  filter(los < 40) %>%
  filter(or_patientclass %in% c('Elective','Surgery Admit','Extended Surgical Recovery')) %>%
  filter(caseclass == 'Elective') %>%
  filter(between(team_size,4,15)) %>%
  filter(asa_rating_c != '5') %>%
  filter(asa_rating_c != '6') %>%
  filter(asa_rating_c != 'NULL')# %>%
  #filter(facility == 'THE JOHNS HOPKINS HOSPITAL')

lm_df <- fam_by_perf_df %>%
  select(room_time, los,primarysurgeonid, asa_rating_c, zeta_4, 
         team_size,zeta_prime_4, facility, cpt, cpt_grouping) %>%
  filter(complete.cases(.)) %>%
  mutate(across(starts_with(c('zeta','team_size')), ~ scale(.,center = TRUE))) %>%
  mutate(across(ends_with('grouping'), ~ as.factor(.)))

x.0 <- lm(as.formula('los ~ 1'), data = lm_df)
x.1 <- lmer(log(room_time) ~ 1 + (1|cpt) + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.2 <- lmer(log(room_time) ~ asa_rating_c + cpt_grouping, data = lm_df, REML = TRUE)
x.3 <- lmer(log(room_time) ~ asa_rating_c + cpt_grouping + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.4 <- lmer(log(room_time) ~ asa_rating_c + zeta_4 + zeta_prime_4 + team_size + cpt_grouping + (1|primarysurgeonid), data = lm_df, REML = TRUE)

x.3 <- lmer(los ~ zeta_1*team_size + asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.4 <- lmer(los ~ asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)

anova(x.0, x.1)
anova(x.1,x.2)
anova(x.2,x.3)
anova(x.3,x.4)

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


#########################################
#########################################
########## Functions for prepping data for analysis
#########################################
#########################################

prep_data_for_quartile_charts <- function(df, outcome) {
  RT.by.fam.quart <- df %>% group_by(primarysurgeonid) %>%
    mutate(Fam.quart = ntile(zeta_prime_52,4)) %>%
    group_by(primarysurgeonid,Fam.quart) %>%
    summarize(room_time_quart_mean = mean(room_time)) %>%
    pivot_wider(
      names_from = Fam.quart, 
      values_from = room_time_quart_mean, 
      names_prefix = "RT.by.fam.quart.") %>%
    mutate(RT.by.fam.quart.diff = RT.by.fam.quart.4 - RT.by.fam.quart.1)
  
  Surg_ID_descr <- lm_df %>% group_by(primarysurgeonid) %>%
    summarize(case_vol = n(),
              rt.avg = mean(room_time),
              fam.avg = mean(zeta_prime_52),
              rt.IQR = IQR(room_time),
              fam.IQR = IQR(zeta_prime_52))
  
  summ_data <- dplyr::inner_join(RT.by.fam.quart, Surg_ID_descr, by = 'primarysurgeonid')
}