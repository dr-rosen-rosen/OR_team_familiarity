#########################################
#########################################
########## Work arounds for modeling data on MAC & Running metrics on PC
#########################################
#########################################
library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(purrr)
library(stringr)
library(tidyverse)

library(ggplot2)
library(sjPlot)
library(sjmisc)


df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases.csv')
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/all_cases_w50per_rt.csv')

df_cmb <- df_cases %>%
  full_join(df_fam)

p <- df_cmb %>%
  filter(facility == 'THE JOHNS HOPKINS HOSPITAL') %>%
  #mutate()
  ggplot(aes(y=zeta_52, x=team_size)) +
  geom_point() + 
  xlab("")
p



Peds_OR_Service <- map(unique(df_cmb$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk
#Peds_OR_Service <- str_subset(Peds_OR_Service, 'OPH Pediatric Strabismus', negate = TRUE) # All of these patients appear to be quite old

fam_by_perf_df <- df_cmb %>%

  ####### ADULT filters
  # filter(!(or_service %in% Peds_OR_Service)) %>%
  # filter(or_patientclass %in% c('Elective','Surgery Admit','Extended Surgical Recovery')) %>%
  # filter(caseclass == 'Elective') %>%
  # filter(age >= 18) %>%
  # filter(asa_rating_c %in% c('1','2','3','4')) %>%
  
  ######## PEDS filters
  filter((or_service %in% Peds_OR_Service)) %>%
  filter(asa_rating_c %in% c('1','2','3')) %>%
  filter(caseclass == 'Elective') %>%
  
  #filter(los < 40) %>%
  
  ######## Everybody filters
  mutate(room_time = as.numeric((out_or_dttm - in_or_dttm), units = "mins")) %>%
  mutate(los = as.numeric((disdate - surgery_date), units = 'days')) %>%
  filter(between(room_time, 15,1000)) %>%
  filter(between(team_size,4,15)) %>%
  filter(surgery_date < lubridate::ymd('2020-01-01')) %>%
  filter(facility == 'THE JOHNS HOPKINS HOSPITAL') %>%
  mutate(los = ifelse(los >= 30, 30, los))

# drop cases with uncommon procedure types
fam_by_perf_df <- fam_by_perf_df %>%
   group_by(cpt) %>%
   filter(n() > 30) %>%
   ungroup()
fam_by_perf_df <- fam_by_perf_df %>%
  group_by(primarysurgeonid) %>%
  filter(n() > 30) %>%
  ungroup()

fam_by_perf_df <- fam_by_perf_df %>%
  group_by(cpt) %>%
  mutate(ext_rt = ifelse(room_time <= mean(room_time), 0, 1)) %>%
  mutate(ext_los = ifelse(los <= mean(los), 0, 1)) %>%
  ungroup()

lm_df <- fam_by_perf_df %>%
  select(room_time, los, ext_los, ext_rt, primarysurgeonid, asa_rating_c, zeta_52, 
         team_size, zeta_prime_52, facility, cpt, cpt_grouping, age, number_of_procedures) %>%
  filter(complete.cases(.)) %>%
  #filter(!(zeta_52 < 0 & zeta_prime_52 > 1.5)) %>% # gets rid of two where there are HUGE differences between z and z'
  filter(!(zeta_52 > 7.5)) %>%
  mutate(multiple_procedures = ifelse(number_of_procedures > 1, 1, 0)) %>%
  mutate(across(starts_with(c('zeta','team_size')), ~ scale(.,center = TRUE))) %>%
  mutate(room_time_log_scaled = scale(log(room_time),center = TRUE)) %>%
  mutate(across(ends_with('grouping'), ~ as.factor(.))) %>%
  mutate(los = scale(los,center = TRUE))

x.0 <- lm(as.formula('log(room_time) ~ 1'), data = lm_df)
# Add cpt code as random var
f <- as.formula(log(room_time) ~ 1 + (1|cpt))
x.1 <- lmer(f, data = lm_df, REML = TRUE)
anova(x.0,x.1)
summary(x.1)
lattice::qqmath(x.1)

# x.2 <- lmer(f, data = lm_df, REML = TRUE)
x.2 <- update(x.1, . ~ . + (1|primarysurgeonid))
anova(x.1,x.2)
summary(x.2)
lattice::qqmath(x.2)

# Add ASA
x.3 <- update(x.2, . ~ . + asa_rating_c + multiple_procedures)
anova(x.2,x.3)
summary(x.3)
lattice::qqmath(x.3)

# Add familiarity
x.4 <- update(x.3, . ~ . + zeta_52)
anova(x.3,x.4)
summary(x.4)
lattice::qqmath(x.4)

# Add team size
x.5 <- update(x.4, .~. + team_size)
anova(x.4, x.5)
summary(x.5)
lattice::qqmath(x.5)

# Add interaction terms
x.6 <- update(x.5, .~. 
              + zeta_52:asa_rating_c 
              + zeta_52:multiple_procedures
              #+ asa_rating_c:multiple_procedures
              #+ zeta_52:asa_rating_c:multiple_procedures
              )
anova(x.5, x.6)
summary(x.6)
lattice::qqmath(x.6)

# Add interaction terms
x.7 <- update(x.6, .~. 
              #+ zeta_52:asa_rating_c 
              #+ zeta_52:multiple_procedures
              #+ asa_rating_c:multiple_procedures
              + zeta_52:asa_rating_c:multiple_procedures
)
anova(x.6, x.7)
summary(x.7)
lattice::qqmath(x.7)

p_peds <- sjPlot::plot_model(
  x.6, 
  type = 'pred', 
  #show.p = TRUE, 
  #ci.lvl = NULL,
  #mdrt.values = "meansd",
  legend.title = 'Multiple\nProcedures',
  # PEDS
  #terms = c('zeta_52','asa_rating_c','multiple_procedures')) + 
  #terms = c('asa_rating_c','zeta_52 [-1.9, 6.0]','multiple_procedures')
  terms = c('zeta_52','multiple_procedures')
  ) + 
  theme_tufte() +
  scale_color_hue(labels = c("No", "Yes")) +
  labs(title = 'PEDIATRIC: Predicted values of room time', y = 'Room time (minutes)', x = 'Zeta')
show(p_peds)

p_peds <- p_peds + labs(title = 'Pediatric')
p_adult <- p_adult + labs(title = 'Adult')
p_adult + p_peds + 
  plot_annotation(title = 'Predicted values of room time') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

tab_model(
  x.0,x.2, x.3, x.5,x.6,
  show.ci = FALSE,
  dv.labels = c('M0: Null model','M1: Nesting','M2: Risk','M3: Team composition','M4: Interactions'),
  # Peds labels
  pred.labels = c(
    "Intercept", "ASA [2]","ASA [3]","Mult. procs.","Zeta","Team size",
    "ASA [2] * Zeta","ASA [3] * Zeta", "Mult. procs. * Zeta"),
  # Adult labels
  # pred.labels = c(
  #   "Intercept", "ASA [2]","ASA [3]","ASA [4]","Mult. procs.","Zeta","Team size",
  #   "ASA [2] * Zeta","ASA [3] * Zeta", "ASA [4] * Zeta", "Mult. procs.) * Zeta"),
  file = here('Model_table_peds.html')
)

?plot_model

#########################################
#########################################
########## GLMER on dichotomous outcomes
#########################################
#########################################


z.0 <- glmer(ext_los ~ (1|primarysurgeonid) + (1|cpt_grouping), data = lm_df, family = binomial)
summary(z.0)
z.1 <- update(z.0, .~. + asa_rating_c + number_of_procedures + age)
summary(z.1)
z.2 <- update(z.1, .~. + zeta_52 + team_size)
summary(z.2)

tab_model(z.0, z.1, z.2)


?glmer
#########################################
#########################################
########## GAM
#########################################
#########################################

library(mgcv)

y.0 <- mgcv::gam(log(room_time) ~ asa_rating_c + s(zeta_52), data = lm_df, method = "REML")
summary(y.0)
plot(y.0, 
     residuals = TRUE, 
     rug = TRUE,
     #pch = 1,
     #cex = 1,
     shade = TRUE,
     pages = 1,
     shift = coef(y.0)[1]
     )
gam.check(y.0)
concurvity(y.0, full = TRUE)




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