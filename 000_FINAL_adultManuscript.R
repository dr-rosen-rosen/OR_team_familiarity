#########################################
#########################################
########## FINAL Analysis for ADULT article
#########################################
#########################################

library(ggplot2)
library(ggthemes)
library(patchwork)
library(sjPlot)
library(sjmisc)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(tidyverse)
library(here)
library(gtsummary)

df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases_02-19-2024.csv')
skimr::skim(df_cases)
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/familiarity_metrics_02-19-2024_V2.csv') %>%
  filter(!if_all(c(avg_dyad_exp_52,bottleneck_score_52,team_fam_disp_52, avg_dyad_exp_12,
                  bottleneck_score_12, team_fam_disp_12,avg_dyad_exp_4, bottleneck_score_4, team_fam_disp_4,
                  zeta_52,zeta_prime_52,zeta_4,zeta_prime_4,zeta_12,zeta_prime_12), is.na))
skimr::skim(df_fam)
source(here('predictorLists.R'), echo = TRUE)
# get team sizes wide to be joined with later metric set by log_id
# team_size_wide <- df_fam %>%
#   select(log_id,team_size,coreTeam) %>%
#   mutate(
#     coreTeam_rc = if_else(coreTeam == TRUE,'core_team','all_team')
#   ) %>%
#   select(-coreTeam) %>%
#   unique() %>% # removing duplicated rows
#   dplyr::group_by(log_id, coreTeam_rc) %>% # for some reasons there are two cases with multiple entries, this is just gets rid of those two cases
#   dplyr::mutate(n = dplyr::n()) %>% #, .groups = "drop") %>%
#   ungroup() %>%
#   dplyr::filter(n == 1L) %>%
#   select(-n) %>%
#   pivot_wider(
#     id_cols = log_id,
#     names_from = coreTeam_rc,
#     values_from = team_size,
#     names_glue = "{.value}.{coreTeam_rc}"
#   ) 


df_fam <- df_fam %>%
    mutate(
        # stts_rc = if_else(stts == TRUE,'case_spec','all_cases'),
        coreTeam_rc = if_else(coreTeam == TRUE,'core_team','all_team')
    ) %>%
  # select(-stts,-coreTeam,-team_size) %>%
  select(-stts,-coreTeam) %>%
  pivot_wider(
      id_cols = log_id,
      names_from = c(coreTeam_rc),
      values_from = c(#avg_dyad_exp_52,bottleneck_score_52,team_fam_disp_52, avg_dyad_exp_12,
                      #bottleneck_score_12, team_fam_disp_12,avg_dyad_exp_4, bottleneck_score_4, team_fam_disp_4,
                      zeta_52,zeta_prime_52,zeta_4,zeta_prime_4,zeta_12,zeta_prime_12, 
                      team_size),
      # names_glue = "{.value}.{stts_rc}.{coreTeam_rc}"
      names_glue = "{.value}.all_cases.{coreTeam_rc}"
  ) %>%
  rename(team_size.all_team = team_size.all_cases.all_team,team_size.core_team = team_size.all_cases.core_team)
  #left_join(team_size_wide, by = "log_id")

df_cmb <- df_cases %>%
  right_join(df_fam)

skimr::skim(df_cmb)

# Define peds service cases
Peds_OR_Service <- map(unique(df_cmb$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk

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
  # filter(surgery_date < lubridate::ymd('2020-01-01')) %>%
  mutate(cpt = factor(cpt)) %>%
  mutate(cpt_grouping = factor(cpt_grouping)) %>%
  mutate(multiple_procedures = ifelse(number_of_procedures > 1, 1, 0)) %>%
  mutate(multiple_procedures = factor(multiple_procedures)) %>%
  filter(asa_rating_c %in% asa_levels) %>%
  mutate(asa_rating_c = factor(asa_rating_c))  %>%
  # filter(facility != "HOWARD COUNTY GENERAL HOSPITAL") #%>% # there were very few cases
  filter(!facility %in% c("HOWARD COUNTY GENERAL HOSPITAL","JOHNS HOPKINS HOWARD COUNTY MEDICAL CENTER",
                       "Greenspring Ambulatory Surgery Center","Greenspring PIII Ambulatory Surgery Center",
                       "Howard County Ambulatory Surgery Center","JHM CLINICAL","Knoll Ambulatory Surgery Center",
                       "Suburban Ambulatory Surgery Center","White Marsh Ambulatory Surgery Center"))

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

skimr::skim(fam_by_perf_df)
# table(is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.core_team),is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.all_team))
lm_df <- fam_by_perf_df %>%
  dplyr::select(
    log_id,room_time, los, primarysurgeonid, asa_rating_c,
    facility, cpt, multiple_procedures,
    team_size.core_team,team_size.all_team,
    surgery_date, sched_case_start_dttm,

    # One year dyad
    # avg_dyad_exp_52.all_cases.core_team,avg_dyad_exp_52.all_cases.all_team,
    # bottleneck_score_52.all_cases.core_team, bottleneck_score_52.all_cases.all_team,
    # team_fam_disp_52.all_cases.core_team, team_fam_disp_52.all_cases.all_team,
    # bottleneck_score_52.case_spec.all_team, bottleneck_score_52.case_spec.core_team,
    # avg_dyad_exp_52.case_spec.all_team, avg_dyad_exp_52.case_spec.core_team,
    # team_fam_disp_52.case_spec.all_team, team_fam_disp_52.case_spec.core_team,

    # One quarter dyad
    # avg_dyad_exp_12.all_cases.core_team, avg_dyad_exp_12.all_cases.all_team,
    # bottleneck_score_12.all_cases.core_team, bottleneck_score_12.all_cases.all_team,
    # team_fam_disp_12.all_cases.core_team, team_fam_disp_12.all_cases.all_team,
    # avg_dyad_exp_12.case_spec.all_team, avg_dyad_exp_12.case_spec.core_team,
    # bottleneck_score_12.case_spec.all_team, bottleneck_score_12.case_spec.core_team,
    # team_fam_disp_12.case_spec.all_team, team_fam_disp_12.case_spec.core_team,

    # One month dyad
    # avg_dyad_exp_4.all_cases.core_team, avg_dyad_exp_4.all_cases.all_team,
    # bottleneck_score_4.all_cases.core_team, bottleneck_score_4.all_cases.all_team,
    # team_fam_disp_4.all_cases.core_team, team_fam_disp_4.all_cases.all_team,
    # avg_dyad_exp_4.case_spec.all_team, avg_dyad_exp_4.case_spec.core_team,
    # bottleneck_score_4.case_spec.all_team, bottleneck_score_4.case_spec.core_team,
    # team_fam_disp_4.case_spec.all_team, team_fam_disp_4.case_spec.core_team,

    # One year borgatti
    # zeta_52.all_cases.core_team, zeta_52.all_cases.all_team,
    zeta_prime_52.all_cases.core_team, zeta_prime_52.all_cases.all_team,
    # zeta_52.case_spec.all_team, zeta_52.case_spec.core_team,
    # zeta_prime_52.case_spec.all_team, zeta_prime_52.case_spec.core_team,

    # One quarter borgatti
    # zeta_12.all_cases.core_team, zeta_12.all_cases.all_team,
    zeta_prime_12.all_cases.core_team, zeta_prime_12.all_cases.all_team,
    # zeta_12.case_spec.all_team, zeta_12.case_spec.core_team,
    # zeta_prime_12.case_spec.all_team, zeta_prime_12.case_spec.core_team,

    # One month borgatti
    # zeta_4.all_cases.core_team, zeta_4.all_cases.all_team,
    zeta_prime_4.all_cases.core_team, zeta_prime_4.all_cases.all_team#,
    # zeta_4.case_spec.all_team, zeta_4.case_spec.core_team,
    # zeta_prime_4.case_spec.all_team, zeta_prime_4.case_spec.core_team,

    # zeta_1.all_cases.core_team,zeta_1.all_cases.all_team,
    # zeta_prime_1.all_cases.core_team,zeta_prime_1.all_cases.all_team#,
    # zeta_1.case_spec.all_team,zeta_1.case_spec.core_team,
    # zeta_prime_1.case_spec.all_team,zeta_prime_1.case_spec.core_team
  )  %>%
  filter(complete.cases(.)) %>%
  filter(room_time >= 1) %>% # there are a few negative numbers... not sure why?
  left_join(fam_by_perf_df[,c('log_id','cpt_grouping')], by = 'log_id') %>% # since there are some NA's int he CPT rangese
  label_cpt_groups(.) %>%
  group_by(cpt) %>%
  mutate(
    ext_rt = ifelse(room_time < quantile(room_time,probs = c(.75), na.rm = TRUE)[['75%']], 0, 1),
    ext_rt = as.factor(ext_rt),
    ext_los = ifelse(los < quantile(los,probs = c(.75), na.rm = TRUE)[['75%']], 0, 1),
    ext_los = as.factor(ext_los)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    weekend = chron::is.weekend(surgery_date),
    start_hour = lubridate::hour(sched_case_start_dttm),
    after_hours = ifelse((lubridate::hour(sched_case_start_dttm) > 19) | (lubridate::hour(sched_case_start_dttm) < 7),1,0),
    after_hours = as.factor(after_hours)
    ) %>% ungroup() %>%
  mutate(
    primarysurgeonid = as.factor(primarysurgeonid), 
    primarysurgeonid = droplevels(primarysurgeonid),
    cpt = as.factor(cpt),
    cpt = droplevels(cpt),
    asa_rating_c = as.factor(asa_rating_c),
    asa_rating_c = droplevels(asa_rating_c),
    multiple_procedures = as.factor(multiple_procedures)) %>%
  mutate(
    facility = dplyr::recode(facility, 
                             "THE JOHNS HOPKINS HOSPITAL" = "Hospital_A", 
                             "JOHNS HOPKINS BAYVIEW MEDICAL CENTER" = "Hospital_B", 
                             "SIBLEY MEMORIAL HOSPITAL" = "Hospital_C", 
                             "SUBURBAN HOSPITAL" = "Hospital_D"),
    facility = factor(facility, levels = c("Hospital_A", "Hospital_B", "Hospital_C", "Hospital_D"))
  )
skimr::skim(lm_df)


#########################################
#########################################
########## Descriptive Tables
#########################################
#########################################

sect_properties <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape",
                                 width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = officer::page_mar()
)
# lm_df %>%
#   select(
#     cpt_grouping, facility) %>%
#   tbl_summary(
#     by = facility
#   ) %>%
#   add_n() %>% # add column with total number of non-missing observations
#   # add_p() %>% # test for a difference between groups
#   modify_header(label = "**Variable**") %>% # update the column header
#   bold_labels() %>%
#   as_flex_table() %>%
#   flextable::set_table_properties(layout = 'autofit') %>%
#   flextable::save_as_docx(., path = here('output','tables','cpt_facility.docx'), pr_section = sect_properties)

lm_df %>%
  select(
    cpt_grouping, asa_rating_c, multiple_procedures, after_hours,
    team_size.core_team, zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team, zeta_prime_4.all_cases.core_team, 
    ext_los, ext_rt, facility) %>%
  tbl_summary(
    by = facility,
    type = list(
      c(cpt_grouping,asa_rating_c,multiple_procedures,after_hours) ~ "categorical",
      c(team_size.core_team, zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team,zeta_prime_4.all_cases.core_team) ~ "continuous2"),
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", 
                                     "{min}, {max}")
  ) %>%
  # add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = 'autofit') %>%
  flextable::save_as_docx(., path = here('output','tables','predictors_outcomes.docx'), pr_section = sect_properties)

#########################################
#########################################
########## GLMER on dichotomous outcomes
#########################################
#########################################


lm_df <- lm_df %>%
  #mutate(across(starts_with(c('team_fam_disp')), ~ log(.))) %>%
  mutate(across(starts_with(c('zeta','bottleneck','team_fam_disp','avg_dyad')), ~ scale(.,center = TRUE)))
  # group_by(primarysurgeonid) %>%
  # group_by(cpt) %>%
  # mutate(across(starts_with(c('zeta')), ~ scale(., center = TRUE))) %>%
  # ungroup()


z.0 <- lm(ext_rt ~ 1, data = lm_df)
summary(z.0)

z.1 <- glmer(ext_rt ~ (1|primarysurgeonid) + (1|cpt), 
             data = lm_df,
             #data = lm_df[which(lm_df$facility == 'Hospital_D'),], 
             family = 'binomial', 
             control = glmerControl(optimizer ="Nelder_Mead"))
summary(z.1)
anova(z.1,z.0)

z.1.a <- glmer(ext_rt ~ (1|primarysurgeonid) + (1|cpt) + (1|facility), 
               data = lm_df,
               #data = lm_df[which(lm_df$facility == 'Hospital_D'),], 
               family = 'binomial', 
               control = glmerControl(optimizer ="Nelder_Mead"))
summary(z.1.a)
anova(z.1, z.1.a)

z.2 <- update(z.1, .~. + facility + asa_rating_c + multiple_procedures +  after_hours)
summary(z.2)
anova(z.1,z.2)

z.2.a <- update(z.1.a, .~. + asa_rating_c + multiple_procedures +  after_hours)
summary(z.2.a)
anova(z.1.a,z.2.a)

summary(z.2.a)
anova(z.2,z.2.a)

# teaFamPreds <- c(oneYearBorg,oneYearDyad)
# teaFamPreds <- teaFamPreds[!str_detect(teaFamPreds, pattern = 'all_team')]
teaFamPreds <- c('zeta_prime_52.all_cases.core_team')#,'avg_dyad_exp_52.all_cases.core_team')
# m.oneYear <- update(z.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))#, "+ multiple_procedures*zeta_prime_52.all_cases.core_team"))
m.oneYear.a <- update(z.2.a, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))#, "+ multiple_procedures*zeta_prime_52.all_cases.core_team"))
# sjPlot::tab_model(m.oneYear,m.oneYear.a)
# summary(m.oneYear)
# anova(z.2,m.oneYear)
# performance::model_performance(m.oneYear)
# performance::check_model(m.oneYear, panel = FALSE)
sjPlot::tab_model(z.2.a, m.oneYear.a)
lme4::allFit(m.oneYear.a)

# m.oneYear.slopeVar <- update(m.oneYear, .~. - (1|primarysurgeonid) - (1|cpt) + (zeta_prime_52.all_cases.core_team|primarysurgeonid) + (zeta_prime_52.all_cases.core_team|cpt))
# sjPlot::tab_model(z.2, m.oneYear,m.oneYear.slopeVar)
# anova(m.oneYear,m.oneYear.slopeVar)
# sjPlot::plot_model(m.oneYear.slopeVar, type = 'pred', terms = c('cpt','zeta_prime_52.all_cases.core_team'), pred.type = 're')
# m.oneYear.int <- update(m.oneYear, '. ~ . + zeta_prime_52.all_cases.core_team*facility + asa_rating_c*zeta_prime_52.all_cases.core_team + multiple_procedures*zeta_prime_52.all_cases.core_team + asa_rating_c*multiple_procedures*zeta_prime_52.all_cases.core_team')
# sjPlot::plot_model(m.oneYear.int, type = 'pred', terms = c('asa_rating_c','zeta_prime_52.all_cases.core_team','multiple_procedures'))
# sjPlot::plot_model(m.oneYear.int, type = 'int')
# sjPlot::tab_model(z.2, m.oneYear,m.oneYear.int)
sjPlot::plot_model(m.oneYear.a, type = 'pred',terms = c('zeta_prime_52.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['52 weeks']),
    y = 'Probability of extended room time',
    title = 'Predicted probabilities of extended room time')

# z.t <- glmer(ext_rt ~ 
#                asa_rating_c + multiple_procedures +  team_size.core_team +
#                zeta_prime_52.all_cases.core_team + 
#                (1|primarysurgeonid) + (1|cpt), data = lm_df, family = 'binomial', 
#              control = glmerControl(optimizer ="Nelder_Mead"))
# summary(z.t)
# sjPlot::tab_model(m.oneYear,z.t)


teaFamPreds <- c('zeta_prime_12.all_cases.core_team')#,'avg_dyad_exp_12.all_cases.core_team')
m.oneQuarter <- update(z.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(m.oneQuarter)
# performance::model_performance(m.oneQuarter)
# performance::check_model(m.oneQuarter, panel = FALSE)
# sjPlot::tab_model(z.2, m.oneQuarter)
# lme4::allFit(m.oneQuarter)
m.oneQuarter.int <- update(m.oneQuarter, '. ~ . + team_size.core_team*zeta_prime_12.all_cases.core_team + asa_rating_c*zeta_prime_12.all_cases.core_team + multiple_procedures*zeta_prime_12.all_cases.core_team + asa_rating_c*multiple_procedures*zeta_prime_12.all_cases.core_team')
sjPlot::plot_model(m.oneQuarter.int, type = 'pred', terms = c('asa_rating_c','zeta_prime_12.all_cases.core_team','multiple_procedures'))
sjPlot::plot_model(m.oneQuarter.int, type = 'int')
sjPlot::tab_model(z.2, m.oneYear,m.oneYear.int)

sjPlot::plot_model(m.oneQuarter, type = 'pred',terms = c('zeta_prime_12.all_cases.core_team [all]')) + ggthemes::theme_tufte() + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['12 weeks']),
    y = 'Probability of extended room time',
    title = 'Predicted probabilities of extended room time')

teaFamPreds <- c('zeta_prime_4.all_cases.core_team')#,'avg_dyad_exp_4.all_cases.core_team')
m.oneMonth <- update(z.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(m.oneMonth)
# performance::model_performance(m.oneMonth)
# performance::check_model(m.oneMonth, panel = FALSE)
# sjPlot::tab_model(z.2, m.oneMonth)
# lme4::allFit(m.oneMonth)
m.oneMonth.int <- update(m.oneMonth, '. ~ . + team_size.core_team*zeta_prime_4.all_cases.core_team + asa_rating_c*zeta_prime_4.all_cases.core_team + multiple_procedures*zeta_prime_4.all_cases.core_team + asa_rating_c*multiple_procedures*zeta_prime_4.all_cases.core_team')
sjPlot::plot_model(m.oneMonth.int, type = 'pred', terms = c('asa_rating_c','zeta_prime_4.all_cases.core_team','multiple_procedures'))
sjPlot::plot_model(m.oneMonth.int, type = 'int')
sjPlot::tab_model(m.oneMonth,m.oneMonth.int)

sjPlot::plot_model(m.oneMonth, type = 'pred',terms = c('zeta_prime_4.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['4 weeks']),
    y = 'Probability of extended room time',
    title = 'Predicted probabilities of extended room time')


# teaFamPreds <- c('zeta_prime_1.all_cases.core_team')
# m.oneWeek <- update(z.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
# summary(m.oneWeek)
# performance::model_performance(m.oneWeek)
# performance::check_model(m.oneWeek, panel = FALSE)
# sjPlot::tab_model(z.2, m.oneWeek)
# lme4::allFit(m.oneWeek)

anova(z.1,z.2)
anova(z.2, m.oneMonth)
anova(z.2,m.oneQuarter)
anova(z.2,m.oneYear)
anova(m.oneYear,m.oneMonth)
sjPlot::tab_model(z.1,z.2, m.oneMonth, m.oneQuarter,m.oneYear,
                  #digits = 5,
                  # title = 'Table 2. Multi-level models for Extended Room Time',
                  title = 'Table 3. Multi-level models for Extended Length of Stay',
                  dv.labels = c(
                    'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                    ),
                  show.aic = TRUE,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  file = here('output','tables','exLOS.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
                  )

# teaFamPreds <- c('zeta_prime_4.all_cases.core_team','zeta_prime_12.all_cases.core_team','zeta_prime_52.all_cases.core_team')
# m.allZetas <- update(z.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
# summary(m.allZetas)
# performance::model_performance(m.allZetas)
# performance::check_model(m.allZetas, panel = FALSE)
# sjPlot::tab_model(z.2, m.allZetas)
# lme4::allFit(m.allZetas)









blmeco::dispersion_glmer(z.3)
overdisp_fun(z.3)
aods3::gof(z.3)

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


#########################################
#########################################
########## MULTI-LEVEL LASSO
#########################################
#########################################

test_HLM_LASSO <- glmmLasso::glmmLasso(room_time ~ #asa_rating_c + multiple_procedures + 
                       #team_size.core_team + team_size.all_team +
                       # One year dyad
                       avg_dyad_exp_52.all_cases.core_team + avg_dyad_exp_52.all_cases.all_team + 
                       bottleneck_score_52.all_cases.core_team +  bottleneck_score_52.all_cases.all_team + 
                       team_fam_disp_52.all_cases.core_team +  team_fam_disp_52.all_cases.all_team + 
                       bottleneck_score_52.case_spec.all_team +  bottleneck_score_52.case_spec.core_team + 
                       avg_dyad_exp_52.case_spec.all_team +  avg_dyad_exp_52.case_spec.core_team + 
                       team_fam_disp_52.case_spec.all_team +  team_fam_disp_52.case_spec.core_team + 
                       
                       # One quarter dyad
                       avg_dyad_exp_12.all_cases.core_team +  avg_dyad_exp_12.all_cases.all_team + 
                       bottleneck_score_12.all_cases.core_team +  bottleneck_score_12.all_cases.all_team + 
                       team_fam_disp_12.all_cases.core_team +  team_fam_disp_12.all_cases.all_team + 
                       avg_dyad_exp_12.case_spec.all_team +  avg_dyad_exp_12.case_spec.core_team + 
                       bottleneck_score_12.case_spec.all_team +  bottleneck_score_12.case_spec.core_team + 
                       team_fam_disp_12.case_spec.all_team +  team_fam_disp_12.case_spec.core_team + 
                       
                       # One month dyad
                       avg_dyad_exp_4.all_cases.core_team +  avg_dyad_exp_4.all_cases.all_team + 
                       bottleneck_score_4.all_cases.core_team +  bottleneck_score_4.all_cases.all_team + 
                       team_fam_disp_4.all_cases.core_team +  team_fam_disp_4.all_cases.all_team + 
                       avg_dyad_exp_4.case_spec.all_team +  avg_dyad_exp_4.case_spec.core_team + 
                       bottleneck_score_4.case_spec.all_team +  bottleneck_score_4.case_spec.core_team + 
                       team_fam_disp_4.case_spec.all_team +  team_fam_disp_4.case_spec.core_team + 
                       
                       # One year borgatti
                       zeta_52.all_cases.core_team +  zeta_52.all_cases.all_team + 
                       zeta_prime_52.all_cases.core_team +  zeta_prime_52.all_cases.all_team + 
                       zeta_52.case_spec.all_team +  zeta_52.case_spec.core_team + 
                       zeta_prime_52.case_spec.all_team +  zeta_prime_52.case_spec.core_team + 
                       
                       # One quarter borgatti
                       zeta_12.all_cases.core_team +  zeta_12.all_cases.all_team + 
                       zeta_prime_12.all_cases.core_team +  zeta_prime_12.all_cases.all_team + 
                       zeta_12.case_spec.all_team +  zeta_12.case_spec.core_team + 
                       zeta_prime_12.case_spec.all_team +  zeta_prime_12.case_spec.core_team + 
                       
                       # One month borgatti
                       zeta_4.all_cases.core_team +  zeta_4.all_cases.all_team + 
                       zeta_prime_4.all_cases.core_team +  zeta_prime_4.all_cases.all_team + 
                       zeta_4.case_spec.all_team +  zeta_4.case_spec.core_team + 
                       zeta_prime_4.case_spec.all_team +  zeta_prime_4.case_spec.core_team,
                     rnd = list(primarysurgeonid=~1, cpt = ~1),
                     data = lm_df,
                     family = gaussian(link = 'identity'),
                     lambda=100
                     #switch.NR = FALSE,
                     #final.re = FALSE#,
                     #control = list()
                     )
summary(test_HLM_LASSO)

#### glinternet

preds <- grep('^(zeta|avg_dyad|team_fam|bottleneck)',names(lm_df),value = TRUE)
X <- lm_df[preds]
y <- log(lm_df$room_time)
set.seed(1001)
numLevels <- rep(1,length(preds)) # 1 is default for continuous variables; need to adjust if to include things like 
cv_fit <- glinternet::glinternet.cv(X,y,numLevels)
plot(cv_fit)
i_1Std <- which(cv_fit$lambdaHat1Std == cv_fit$lambda)
coefs <- coef(cv_fit$glinternetFit)[[i_1Std]]
names(coefs)

## Elastic net
preds <- c(preds,'asa_rating_c','multiple_procedures')
X <- model.matrix(log(room_time) ~ .,lm_df[c('room_time',preds)])
# X <- data.matrix(lm_df[preds],rownames.force = TRUE)
y <- log(lm_df$room_time)
cv.fit <- glmnet::cv.glmnet(X,y)
plot(cv.fit)
print(cv.fit)
summary(coef(cv.fit, s = 'lambda.1se'))
reduced_coef_list <- coef(cv.fit, s = 'lambda.1se') %>% as.matrix %>% as.data.frame() %>% filter(is.numeric(s1)) %>% filter(abs(s1) > .001) %>% rownames()
reduced_coef_list <- reduced_coef_list[!grepl('^asa|multiple|Intercept',reduced_coef_list)]
reduced_coef_form <- paste(reduced_coef_list,collapse = ' + ')
