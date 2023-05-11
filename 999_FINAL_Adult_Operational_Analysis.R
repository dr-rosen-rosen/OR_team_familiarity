#########################################
#########################################
########## Work arounds for modeling data on MAC & Running metrics on PC
#########################################
#########################################
library(ggplot2)
library(ggthemes)
library(patchwork)
library(sjPlot)
library(sjmisc)
library(dplyr)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(purrr)
library(stringr)
library(tidyverse)
library(here)
library(gtsummary)

df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases.csv')
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/all_cases_w50per_rt.csv')

# skimr::skim(df_fam$team_size)

df_cmb <- df_cases %>%
  full_join(df_fam)

# skimr::skim(df_cmb$team_size)

# Define peds service cases
Peds_OR_Service <- map(unique(df_cmb$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk

hch <- df_cmb %>%
  filter(facility == "HOWARD COUNTY GENERAL HOSPITAL") %>%
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(or_patientclass %in% c('Inpatient','Surgery Admit','Extended Surgical Recovery')) %>%
  filter(caseclass == 'Elective') %>%
  filter(age >= 18)

asa_levels <- c('1','2','3','4') 
fam_by_perf_df <- df_cmb %>%

  ####### ADULT filters
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(or_patientclass %in% c('Inpatient','Surgery Admit','Extended Surgical Recovery')) %>%
  filter(caseclass == 'Elective') %>%
  filter(age >= 18) %>%
  filter(facility != "THE JOHNS HOPKINS ALL CHILDREN'S HOSPITAL") %>%

  ######## Everybody filters
  mutate(room_time = as.numeric((out_or_dttm - in_or_dttm), units = "mins")) %>%
  mutate(los = as.numeric((disdate - surgery_date), units = 'days')) %>%
  filter(surgery_date < lubridate::ymd('2020-01-01')) %>%
  mutate(cpt = factor(cpt)) %>%
  mutate(cpt_grouping = factor(cpt_grouping)) %>%
  mutate(multiple_procedures = ifelse(number_of_procedures > 1, 1, 0)) %>%
  mutate(multiple_procedures = factor(multiple_procedures)) %>%
  filter(asa_rating_c %in% asa_levels) %>%
  mutate(asa_rating_c = factor(asa_rating_c))  %>% 
  filter(facility != "HOWARD COUNTY GENERAL HOSPITAL") #%>% # there were very few cases
  #filter(facility == "THE JOHNS HOPKINS HOSPITAL")
# drop cases with uncommon procedure types
fam_by_perf_df <- fam_by_perf_df %>%
   group_by(cpt) %>%
   filter(n() > 30) %>%
   ungroup()
# drop low frequency surgeons
fam_by_perf_df <- fam_by_perf_df %>%
  group_by(primarysurgeonid) %>%
  filter(n() > 30) %>%
  ungroup()

lm_df <- fam_by_perf_df %>%
  dplyr::select(log_id,room_time, los, primarysurgeonid, asa_rating_c, 
         facility, cpt, multiple_procedures,
         team_size, 
         zeta_52, zeta_prime_52, 
         # zeta_12, zeta_prime_12,
         # zeta_4,zeta_prime_4,
         # zeta_1,zeta_prime_1,
         team_size
         )  %>%
  filter(complete.cases(.)) %>%
  filter(room_time >= 1) %>% # there are a few negative numbers... not sure why?
  left_join(fam_by_perf_df[,c('log_id','cpt_grouping')], by = 'log_id') %>% # since there are some NA's int he CPT rangese
  label_cpt_groups(.) %>%
  group_by(cpt) %>%
  mutate(ext_rt = ifelse(room_time <= quantile(room_time,probs = c(.75), na.rm = TRUE)[['75%']], 0, 1)) %>%
  mutate(ext_los = ifelse(los <= quantile(los,probs = c(.75), na.rm = TRUE)[['75%']], 0, 1)) %>%
  ungroup()

library(gtsummary)
sect_properties <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = officer::page_mar()
)
lm_df %>%
  select(
    cpt_grouping, facility) %>%
  tbl_summary(
    by = facility
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = 'autofit') %>%
  flextable::save_as_docx(., path = here('output','tables','cpt_facility.docx'), pr_section = sect_properties)

lm_df %>%
  select(
    asa_rating_c, multiple_procedures, 
    team_size, zeta_52, 
    ext_los, ext_rt, facility) %>%
  tbl_summary(
    by = facility
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = 'autofit') %>%
  flextable::save_as_docx(., path = here('output','tables','predictors_outcomes.docx'), pr_section = sect_properties)

# Scale variables 
lm_df <- lm_df %>%
  mutate(across(starts_with(c('zeta','team_size')), ~ scale(.,center = TRUE))) 


#########################################
#########################################
########## GLMER on dichotomous outcomes
#########################################
#########################################


z.0 <- lm(ext_los ~ 1, data = lm_df)
summary(z.0)

z.1 <- glmer(ext_los ~ (1|primarysurgeonid) + (1|cpt), data = lm_df, family = binomial)
summary(z.1)
anova(z.1,z.0)
z.2 <- update(z.1, .~. + asa_rating_c + multiple_procedures)
summary(z.2)
anova(z.1,z.2)
z.3 <- update(z.2, .~. + team_size + 
                zeta_prime_52
              )
summary(z.3)
anova(z.2,z.3)
# z.4 <- update(z.3, .~. +
#                 zeta_prime_52*multiple_procedures +
#                 team_size*multiple_procedures +
#                 zeta_prime_52*asa_rating_c +
#                 zeta_prime_52*multiple_procedures*asa_rating_c
#                 )
# summary(z.4)
# tab_model(z.4)
lme4::allFit(z.3)
blmeco::dispersion_glmer(z.3)
overdisp_fun(z.3)
aods3::gof(z.3)
tab_model(z.0, z.1, z.2,z.3,
          title = 'Multi-level Modeling Results for Extended Room Time',
          show.aic = TRUE,
          dv.labels = c(
            'M0: Null model',
            'M1: Nesting',
            'M2: Risk',
            'M3: Team composition'),
          # Peds labels
          pred.labels = c(
            "Intercept", "ASA [2]","ASA [3]", "ASA [4]",
            "Mult. procs.","Team size","Shared work experience (zeta' 52 weeks)"),
          file = here('output','tables','MLM_rt.html')
          )
p_los <- sjPlot::plot_model(z.3,
                   type = 'eff', 
                   terms = 'zeta_prime_52'
                   ) + 
  labs(x = 'Shared work experiences (zeta)',
       y = 'Probability of an extended length of stay',
       title = 'Predicted probabilities of an extended length of stay across shared work experiences') +
  theme_tufte() +
  geom_vline(xintercept = mean(lm_df$zeta_prime_52),  
             color = "red", size=1) +
  geom_vline(xintercept = quantile(lm_df$zeta_prime_52, probs = c(.75), na.rm = TRUE)[['75%']],  
             color = "black", size=.75, linetype="dotted") +
  geom_vline(xintercept = quantile(lm_df$zeta_prime_52, probs = c(.25), na.rm = TRUE)[['25%']],
             color = "black", size=.75, linetype="dotted") +
  geom_vline(xintercept = mean(lm_df$zeta_52) - sd(lm_df$zeta_prime_52),
             color = "red", size=.75, linetype="dashed")  +
  geom_vline(xintercept = mean(lm_df$zeta_52) + sd(lm_df$zeta_prime_52),
             color = "red", size=.75, linetype="dashed") 

p_los / p_rt + plot_layout(guides = "collect") + plot_annotation(caption = "Solid red line is mean;\nBlack dotted lines are upper and lower quartiles;\nDashed red lines are +/- 1 SD")


# From: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

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



library(gamlss)
fit1 <- gamlss::fitDist(lm_df$zeta_52, type = "realline")
fit1 <- gamlss::fitDist(lm_df$ext_los, type = "binom")
fit1$fits

t <- gamlss::gamlss(
  formula = ext_los ~ asa_rating_c + multiple_procedures +
    team_size + zeta_prime_52 +
    re(random = ~1|primarysurgeonid) + re(random = ~1|cpt),
  family = BI(), data = lm_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
summary(t)
Rsq(t)
plot(t)
wp(t)
str(t)
t$mu.coefSmo[[2]]$coefficients$random
intervals(getSmo(t))
histDist(zeta_52, data = lm_df, family = SEP4, nbins = 100)
