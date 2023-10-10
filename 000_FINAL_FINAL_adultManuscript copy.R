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

df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases.csv')
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/combinedTeamCompMetrics.csv') %>%
  filter(!if_all(c(avg_dyad_exp_52,bottleneck_score_52,team_fam_disp_52, avg_dyad_exp_12,
                  bottleneck_score_12, team_fam_disp_12,avg_dyad_exp_4, bottleneck_score_4, team_fam_disp_4,
                  zeta_52,zeta_prime_52,zeta_4,zeta_prime_4,zeta_1,zeta_prime_1,zeta_12,zeta_prime_12), is.na))
source(here('predictorLists.R'), echo = TRUE)
# get team sizes wide to be joined with later metric set by log_id
team_size_wide <- df_fam %>%
  select(log_id,team_size,coreTeam) %>%
  mutate(
    coreTeam_rc = if_else(coreTeam == TRUE,'core_team','all_team')
  ) %>%
  select(-coreTeam) %>%
  unique() %>% # removing duplicated rows
  dplyr::group_by(log_id, coreTeam_rc) %>% # for some reasons there are two cases with multiple entries, this is just gets rid of those two cases
  dplyr::mutate(n = dplyr::n()) %>% #, .groups = "drop") %>%
  ungroup() %>%
  dplyr::filter(n == 1L) %>%
  select(-n) %>%
  pivot_wider(
    id_cols = log_id,
    names_from = coreTeam_rc,
    values_from = team_size,
    names_glue = "{.value}.{coreTeam_rc}"
  ) 


df_fam <- df_fam %>%
    mutate(
        stts_rc = if_else(stts == TRUE,'case_spec','all_cases'),
        coreTeam_rc = if_else(coreTeam == TRUE,'core_team','all_team')
    ) %>%
  select(-stts,-coreTeam,-team_size) %>%
  pivot_wider(
      names_from = c(stts_rc,coreTeam_rc),
      values_from = c(avg_dyad_exp_52,bottleneck_score_52,team_fam_disp_52, avg_dyad_exp_12,
                      bottleneck_score_12, team_fam_disp_12,avg_dyad_exp_4, bottleneck_score_4, team_fam_disp_4,
                      zeta_52,zeta_prime_52,zeta_4,zeta_prime_4,zeta_1,zeta_prime_1,zeta_12,zeta_prime_12),
      names_glue = "{.value}.{stts_rc}.{coreTeam_rc}"
  ) %>%
  left_join(team_size_wide, by = "log_id")

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
  filter(surgery_date < lubridate::ymd('2020-01-01')) %>%
  mutate(
    team_size.periph_team = team_size.all_team - team_size.core_team,
    team_size.prop_periph = team_size.periph_team / team_size.all_team) %>%
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

skimr::skim(fam_by_perf_df)
# table(is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.core_team),is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.all_team))
lm_df <- fam_by_perf_df %>%
  dplyr::select(
    log_id,room_time, los, primarysurgeonid, asa_rating_c, age,
    facility, cpt, multiple_procedures,
    team_size.core_team,team_size.all_team,team_size.prop_periph, team_size.periph_team,
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
    weekend = as.factor(as.numeric(chron::is.weekend(surgery_date))),
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

#### raincloud plot
# https://mjskay.github.io/ggdist/articles/dotsinterval.html

library(ggdist)
# lm_df %>%
#   pivot_longer(
#     cols = contains('.all_cases.core_team'),
#     names_to = 'var',
#     values_to = 'value'
#   ) %>% 
#   select(var,value) %>%
#   slice(1:50000) %>%
#   ggplot(aes(y = var, x = value, fill = var)) +
#   stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
#   stat_dotsinterval(side = "bottom", scale = 0.4, slab_size = NA) +
#   scale_fill_brewer(palette = "Set2") +
#   ggthemes::theme_tufte()
  
  # ggplot(aes(y = zeta_prime_52.all_cases.core_team, x = facility, fill = facility)) +
  # stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
  # stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) +
  # scale_fill_brewer(palette = "Set2")

lm_df %>%
  ggplot(aes(x=zeta_prime_4.all_cases.core_team, y = los, fill = facility)) + 
  geom_point() +
  # ggside::geom_xsidedensity(alpha = .3, position = "stack") +
  # ggside::geom_ysidedensity(alpha = .3, position = "stack") +
  ggside::geom_xsideboxplot(aes(y = facility), orientation = "y") + ggside::scale_xsidey_discrete() +
  # ggside::geom_ysideboxplot(aes(y = facility), orientation = "x") + #ggside::scale_ysidex_discrete() +
  ggthemes::theme_tufte()

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
    cpt_grouping, age, asa_rating_c, multiple_procedures, after_hours, weekend,
    team_size.core_team, team_size.all_team,
    zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team, zeta_prime_4.all_cases.core_team,
    zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team,
    ext_los, ext_rt, facility) %>%
  tbl_summary(
    by = facility,
    type = list(
      c(cpt_grouping,age,asa_rating_c,multiple_procedures,weekend,after_hours) ~ "categorical",
      c(age,team_size.core_team, team_size.all_team, 
        zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team,zeta_prime_4.all_cases.core_team,
        zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team) ~ "continuous2"),
    statistic = list(c(team_size.core_team, team_size.all_team, 
                       zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team,zeta_prime_4.all_cases.core_team,
                       zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team) ~ "{mean} ({sd})")
    # statistic = all_continuous() ~ c("{median} ({p25}, {p75})", 
    #                                  "{min}, {max}"
                                     # )
  ) %>%
  add_overall() %>%
  # add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = 'autofit') %>%
  flextable::save_as_docx(., path = here('output','tables','predictors_outcomesV2.docx'), pr_section = sect_properties)

#########################################
#########################################
########## GLMER on dichotomous outcomes
#########################################
#########################################
lm_df <- lm_df %>%
  mutate(across(starts_with(c('zeta','team_fam_disp','avg_dyad','age','team_size')), ~ scale(.,center = TRUE)))

########################## EXT_RT


rt.0 <- lm(ext_rt ~ 1, data = lm_df)
summary(rt.0)

rt.1 <- glmer(ext_rt ~ (1|primarysurgeonid) + (1|cpt) + (1|facility), 
               data = lm_df,
               family = 'binomial', 
               control = glmerControl(optimizer ="Nelder_Mead"))
summary(rt.1)
anova(rt.1,rt.0)

rt.2 <- update(rt.1, .~. + age + asa_rating_c + multiple_procedures +  weekend + after_hours)
summary(rt.2)
anova(rt.1,rt.2)

#### RT one yaer

teaFamPreds <- c('zeta_prime_52.all_cases.core_team')
rt.oneYear <- update(rt.2, paste('. ~ . + team_size.core_team + ', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneYear)
anova(rt.2,rt.oneYear)
performance::model_performance(rt.oneYear)
performance::check_model(rt.oneYear, panel = FALSE)
sjPlot::tab_model(rt.2, rt.oneYear)
lme4::allFit(rt.oneYear)

#### RT one quarter

teaFamPreds <- c('zeta_prime_12.all_cases.core_team')#,'avg_dyad_exp_12.all_cases.core_team')
rt.oneQuarter <- update(rt.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneQuarter)
performance::model_performance(rt.oneQuarter)
performance::check_model(rt.oneQuarter, panel = FALSE)
sjPlot::tab_model(z.2, rt.oneYear, rt.oneQuarter)
lme4::allFit(rt.oneQuarter)



#### RT one month

teaFamPreds <- c('zeta_prime_4.all_cases.core_team')#,'avg_dyad_exp_4.all_cases.core_team')
rt.oneMonth <- update(rt.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneMonth)
performance::model_performance(rt.oneMonth)
performance::check_model(rt.oneMonth, panel = FALSE)
sjPlot::tab_model(rt.2, rt.oneYear, rt.oneQuarter, rt.oneMonth)
lme4::allFit(rt.oneMonth)


sjPlot::tab_model(rt.2, rt.oneMonth, rt.oneQuarter, rt.oneYear,
                  #digits = 5,
                  # title = 'Table 2. Multi-level models for Extended Room Time',
                  title = 'Table 2. Multi-level models for Extended Room Time',
                  dv.labels = c(
                    # 'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                    ),
                  show.aic = TRUE,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  file = here('output','tables','exRT_coreTeam.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
                  )

sjPlot::plot_models(
  rt.oneMonth,rt.oneQuarter,rt.oneYear, 
  rm.terms = c('asa_rating_c','multiple_procedures','after_hours','team_size.core_team'),
  # terms = c('zeta_prime_4.all_cases.core_team','zeta_prime_12.all_cases.core_team','zeta_prime_52.all_cases.core_team'),
  grid = FALSE) + ggthemes::theme_tufte()

anova(rt.1,rt.2)
anova(rt.2,rt.oneMonth)
anova(rt.2,rt.oneQuarter)
anova(rt.2,rt.oneYear)

########################## EXT_LOS



los.0 <- lm(ext_los ~ 1, data = lm_df)
summary(los.0)

los.1 <- glmer(ext_los ~ (1|primarysurgeonid) + (1|cpt) + (1|facility), 
              data = lm_df,
              family = 'binomial', 
              control = glmerControl(optimizer ="Nelder_Mead"))
summary(los.1)
anova(los.1,los.0)

los.2 <- update(los.1, .~. + age + asa_rating_c + multiple_procedures +  weekend + after_hours)
summary(los.2)
anova(los.1,los.2)

#### LOS One year

teaFamPreds <- c('zeta_prime_52.all_cases.core_team')
los.oneYear <- update(los.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneYear)
anova(los.2,los.oneYear)
performance::model_performance(los.oneYear)
performance::check_model(los.oneYear, panel = FALSE)
sjPlot::tab_model(los.2, los.oneYear)
lme4::allFit(los.oneYear)



#### LOS one quarter

teaFamPreds <- c('zeta_prime_12.all_cases.core_team')#,'avg_dyad_exp_12.all_cases.core_team')
los.oneQuarter <- update(los.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneQuarter)
performance::model_performance(los.oneQuarter)
performance::check_model(los.oneQuarter, panel = FALSE)
sjPlot::tab_model(z.2, los.oneYear, los.oneQuarter)
lme4::allFit(los.oneQuarter)



#### LOS one month

teaFamPreds <- c('zeta_prime_4.all_cases.core_team')#,'avg_dyad_exp_4.all_cases.core_team')
los.oneMonth <- update(los.2, paste('. ~ . + team_size.core_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneMonth)
performance::model_performance(los.oneMonth)
performance::check_model(los.oneMonth, panel = FALSE)
sjPlot::tab_model(los.2, los.oneYear, los.oneQuarter, los.oneMonth)
lme4::allFit(los.oneMonth)


sjPlot::tab_model(los.2, los.oneMonth, los.oneQuarter, los.oneYear,
                  #digits = 5,
                  # title = 'Table 2. Multi-level models for Extended Room Time',
                  title = 'Table 3. Multi-level models for Extended Length of Stay',
                  dv.labels = c(
                    # 'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                  ),
                  show.aic = TRUE,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  file = here('output','tables','exLOS_coreTeam.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
)

anova(los.1,los.2)
anova(los.2,los.oneMonth)
anova(los.2,los.oneQuarter)
anova(los.2,los.oneYear)



sjPlot::plot_models(los.oneMonth,los.oneQuarter,los.oneYear, grid = FALSE) + ggthemes::theme_tufte()

sjPlot::plot_models(los.oneMonth,rt.oneMonth,
                    rm.terms = c('asa_rating_c'))

rt_52 <- sjPlot::plot_model(rt.oneYear, type = 'pred',terms = c('zeta_prime_52.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['52 weeks']),
    y = 'Probability of extended room time'#,
    # title = 'Predicted probabilities of extended room time'
    )

rt_12 <- sjPlot::plot_model(rt.oneQuarter, type = 'pred',terms = c('zeta_prime_12.all_cases.core_team [all]')) + ggthemes::theme_tufte() + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['12 weeks']),
    y = 'Probability of extended room time'#,
    # title = 'Predicted probabilities of extended room time'
    )

rt_4 <- sjPlot::plot_model(rt.oneMonth, type = 'pred',terms = c('zeta_prime_4.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['4 weeks']),
    y = 'Probability of extended room time'#,
    # title = 'Predicted probabilities of extended room time'
    )



los_52 <- sjPlot::plot_model(los.oneYear, type = 'pred',terms = c('zeta_prime_52.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['52 weeks']),
    y = 'Probability of extended lenght of stay'#,
    # pred.type = "re",
    # ci.lvl = NA,
    # show.data = TRUE,
    # title = 'Predicted probabilities of extended lenght of stay'
    )

los_12 <- sjPlot::plot_model(los.oneQuarter, type = 'pred',terms = c('zeta_prime_12.all_cases.core_team [all]')) + ggthemes::theme_tufte() + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['12 weeks']),
    y = 'Probability of extended lenght of stay'#,
    # title = 'Predicted probabilities of extended lenght of stay'
    )

los_4 <- sjPlot::plot_model(los.oneMonth, type = 'pred',terms = c('zeta_prime_4.all_cases.core_team [all]')) + 
  ggthemes::theme_tufte() +
  labs(
    x = expression(paste(zeta,"'")['4 weeks']),
    y = 'Probability of extended lenght of stay'#,
    # title = 'Predicted probabilities of extended lenght of stay'
    )

(rt_4 + rt_12 + rt_52) / (los_4 + los_12 + los_52) +
  plot_annotation(tag_levels = 'A')

########################## 
########################## 
########################## 
########################## 
########################## FUll team anlaysis
########################## 
########################## 
########################## 
########################## 
########################## 


########################## EXT_RT full team



#### RT one yaer

teaFamPreds <- c('zeta_prime_52.all_cases.all_team')
rt.oneYear.ft <- update(rt.2, paste('. ~ . + team_size.all_team + ', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneYear.ft)
anova(rt.2,rt.oneYear.ft)
performance::model_performance(rt.oneYear.ft)
performance::check_model(rt.oneYear.ft, panel = FALSE)
sjPlot::tab_model(rt.2, rt.oneYear.ft)
lme4::allFit(rt.oneYear.ft)

#### RT one quarter

teaFamPreds <- c('zeta_prime_12.all_cases.all_team')
rt.oneQuarter.ft <- update(rt.2, paste('. ~ . + team_size.all_team +', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneQuarter.ft)
performance::model_performance(rt.oneQuarter.ft)
performance::check_model(rt.oneQuarter.ft, panel = FALSE)
sjPlot::tab_model(rt.2, rt.oneYear.ft, rt.oneQuarter.ft)
lme4::allFit(rt.oneQuarter.ft)



#### RT one month

teaFamPreds <- c('zeta_prime_4.all_cases.all_team')
rt.oneMonth.ft <- update(rt.2, paste('. ~ . + team_size.all_team +', paste(teaFamPreds,collapse = ' + ')))
summary(rt.oneMonth.ft)
performance::model_performance(rt.oneMonth.ft)
performance::check_model(rt.oneMonth.ft, panel = FALSE)
sjPlot::tab_model(rt.2, rt.oneYear.ft, rt.oneQuarter.ft, rt.oneMonth.ft)
lme4::allFit(rt.oneMonth.ft)


sjPlot::tab_model(rt.2, rt.oneMonth.ft, rt.oneQuarter.ft, rt.oneYear.ft,
                  #digits = 5,
                  # title = 'Table 2. Multi-level models for Extended Room Time',
                  title = 'Table 4. Multi-level models for Extended Room Time - with all team members',
                  dv.labels = c(
                    # 'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                  ),
                  show.aic = TRUE,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  file = here('output','tables','exRT_allTeam.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
)

sjPlot::plot_models(
  rt.oneMonth,rt.oneQuarter,rt.oneYear, 
  rm.terms = c('asa_rating_c','multiple_procedures','after_hours','team_size.core_team'),
  # terms = c('zeta_prime_4.all_cases.core_team','zeta_prime_12.all_cases.core_team','zeta_prime_52.all_cases.core_team'),
  grid = FALSE) + ggthemes::theme_tufte()

anova(rt.1,rt.2)
anova(rt.2,rt.oneMonth.ft)
anova(rt.2,rt.oneQuarter.ft)
anova(rt.2,rt.oneYear.ft)

########################## EXT_LOS

#### LOS One year

teaFamPreds <- c('zeta_prime_52.all_cases.all_team')
los.oneYear.ft <- update(los.2, paste('. ~ . + team_size.all_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneYear.ft)
anova(los.2,los.oneYear.ft)
performance::model_performance(los.oneYear.ft)
performance::check_model(los.oneYear, panel = FALSE)
sjPlot::tab_model(los.2, los.oneYear.ft)
lme4::allFit(los.oneYear.ft)



#### LOS one quarter

teaFamPreds <- c('zeta_prime_12.all_cases.all_team')#,'avg_dyad_exp_12.all_cases.core_team')
los.oneQuarter.ft <- update(los.2, paste('. ~ . + team_size.all_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneQuarter.ft)
performance::model_performance(los.oneQuarter.ft)
performance::check_model(los.oneQuarter, panel = FALSE)
sjPlot::tab_model(z.2, los.oneYear.ft, los.oneQuarter.ft)
lme4::allFit(los.oneQuarter.ft)



#### RT one month

teaFamPreds <- c('zeta_prime_4.all_cases.all_team')#,'avg_dyad_exp_4.all_cases.core_team')
los.oneMonth.ft <- update(los.2, paste('. ~ . + team_size.all_team +', paste(teaFamPreds,collapse = ' + ')))
summary(los.oneMonth.ft)
performance::model_performance(los.oneMonth.ft)
performance::check_model(los.oneMonth.ft, panel = FALSE)
sjPlot::tab_model(los.2, los.oneYear.ft, los.oneQuarter.ft, los.oneMonth.ft)
lme4::allFit(los.oneMonth.ft)


sjPlot::tab_model(los.2, los.oneMonth.ft, los.oneQuarter.ft, los.oneYear.ft,
                  #digits = 5,
                  # title = 'Table 2. Multi-level models for Extended Room Time',
                  title = 'Table 5. Multi-level models for Extended Length of Stay for all team members',
                  dv.labels = c(
                    # 'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                  ),
                  show.aic = TRUE,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  file = here('output','tables','exLOS_allTeam.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
)

anova(los.1,los.2)
anova(los.2,los.oneMonth.ft)
anova(los.2,los.oneQuarter.ft)
anova(los.2,los.oneYear.ft)



#####################################################
# Final Tables
#####################################################
#####################################################

sjPlot::tab_model(
  rt.2, rt.oneMonth, rt.oneQuarter, rt.oneYear,rt.oneMonth.ft, rt.oneQuarter.ft, rt.oneYear.ft,
                  #digits = 5,
                  title = 'Table 2. Multi-level models for Extended Length of Stay for all team members',
                  dv.labels = c(
                    # 'M1: Surgeon & Procedure Nesting',
                    'M2: M1 + control variables',
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression(" M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year']),
                    expression("M3: M2 + \U03B6'"['1 month']),
                    expression("M4: M2 + \U03B6'"['1 quarter']),
                    expression("M5: M2 + \U03B6'"['1 year'])
                  ),
                  show.p = FALSE,
                  show.aic = TRUE#,
                  # show.loglik = TRUE,
                  # file = here('output','tables','exRT.html')
                  # file = here('output','tables','extRT_combined.html')
                  # show.reflvl = TRUE,
                  # show.df = TRUE,
                  # show.stat = TRUE,
)

sjPlot::tab_model(
  los.2, los.oneMonth, los.oneQuarter, los.oneYear,los.oneMonth.ft, los.oneQuarter.ft, los.oneYear.ft,
  #digits = 5,
  title = 'Table 3. Summary of Multi-level Models for Extended Length of Stay',
  dv.labels = c(
    # 'M1: Surgeon & Procedure Nesting',
    'M2: M1 + control variables',
    expression("M3: M2 + \U03B6'"['1 month']),
    expression(" M4: M2 + \U03B6'"['1 quarter']),
    expression("M5: M2 + \U03B6'"['1 year']),
    expression("M3: M2 + \U03B6'"['1 month']),
    expression("M4: M2 + \U03B6'"['1 quarter']),
    expression("M5: M2 + \U03B6'"['1 year'])
  ),
  show.p = FALSE,
  show.aic = TRUE#,
  # show.loglik = TRUE,
  # file = here('output','tables','extLOS_combined.html')
  # show.reflvl = TRUE,
  # show.df = TRUE,
  # show.stat = TRUE,
)


