# Analysis for OR shared work experience in adult surgery cases.
# Author: Jaxon Wu (jwu149)
# Date: 03/05/2024

# Loading libraries
library(dplyr)
library(here)
library(tidyverse)
library(lme4)
library(sjPlot)

# Load config
# config <- config::get()

# Load raw data
df_cases <- read_csv(here::here("Jaxons_work","Data","cases_02-19-2024.csv")) 
df_fam <- read_csv(here::here("Jaxons_work","Data","familiarity_metrics_03-05-2024.csv"))

# Clean df_cases
## Get start times (the hour was stripped off in reading in before)
start_times <- data.table::fread(here::here("Jaxons_work","Data","All OR Cases as of 2023_1115.csv"),
                                 select = c("LOG_ID", "SCHED_CASE_START_DTTM")) %>%
        bind_rows(data.table::fread(here::here("Jaxons_work","Data","All OR Cases.csv"),
                                    select = c("LOG_ID", "SCHED_CASE_START_DTTM"))) %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(sched_case_start_dttm = lubridate::mdy_hm(sched_case_start_dttm,
                                                         tz = "UTC"))
## Join start times with df_cases 
df_cases <- df_cases %>%
        select(-sched_case_start_dttm) %>%
        left_join(start_times, by = "log_id") 

# Cleanand join data
df_fam <- df_fam[,-1]

# Remove rows with any missing data
df_fam <- df_fam %>%
        filter(!if_all(
          c(
            avg_dyad_exp_52, bottleneck_score_52, team_fam_disp_52,
            avg_dyad_exp_12, bottleneck_score_12, team_fam_disp_12,
            avg_dyad_exp_4, bottleneck_score_4, team_fam_disp_4,
                         zeta_52, zeta_prime_52, zeta_4,
                         zeta_prime_4, #zeta_1, zeta_prime_1, 
                         zeta_12,zeta_prime_12),
                       is.na))

# Get predictor list; see 'predictorLists.R'
source(here('predictorLists.R'), echo = TRUE)

# Make team sizes wide to be joined with later metric set by log_id
team_size_wide <- df_fam %>%
        select(log_id, team_size, coreTeam) %>%
        mutate(coreTeam_rc = if_else(coreTeam == TRUE,
                                     'core_team',
                                     'all_team'),
               .keep = "unused") %>%
        unique() %>% 
        pivot_wider(id_cols = log_id,
                    names_from = coreTeam_rc,
                    values_from = team_size,
                    names_glue = "{.value}.{coreTeam_rc}") 

# Make stts wide and joining with team size wide
df_fam <- df_fam %>%
        mutate(stts_rc = if_else(stts == TRUE,'case_spec','all_cases'),
               coreTeam_rc = if_else(coreTeam == TRUE,'core_team','all_team'),
               .keep = "unused") %>%
        select(-team_size) %>%
        pivot_wider(names_from = c(stts_rc, coreTeam_rc),
                    values_from = c(avg_dyad_exp_52, avg_dyad_exp_12, avg_dyad_exp_4,
                                    bottleneck_score_52, bottleneck_score_12, bottleneck_score_4,
                                    team_fam_disp_52, team_fam_disp_12, team_fam_disp_4,
                                    zeta_52, zeta_12, zeta_4, # zeta_1, zeta_prime_1
                                    zeta_prime_52, zeta_prime_12, zeta_prime_4),
                    names_glue = "{.value}.{stts_rc}.{coreTeam_rc}") %>%
        left_join(team_size_wide, by = "log_id")  

# Join `df_cases` and `df_fam` based on IDs in `df_fam`
df_cmb <- df_cases %>%
        right_join(df_fam)

# Subset pediatric cases
Peds_OR_Service <- map(unique(df_cmb$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] 

# Define ASA levels
asa_levels <- c('1','2','3','4')

# # Examine patient classes in not Pediatrics
# df_cmb %>%
#         filter(!(or_service %in% Peds_OR_Service)) %>%
#         group_by(or_patientclass) %>%
#         summarise(count = n()) %>%
#         mutate(prop = count / sum(count) * 100) %>%
#         arrange(desc(count))
# 
# # Examine case classes in Pediatrics
# df_cmb %>%
#         filter(or_service %in% Peds_OR_Service) %>%
#         group_by(caseclass) %>%
#         summarise(count = n()) %>%
#         mutate(prop = count / sum(count) * 100) %>%
#         arrange(desc(count))

# Filter for adult cases
fam_by_perf_df <- df_cmb %>%
        mutate(room_time = as.numeric(out_or_dttm - in_or_dttm, units = "mins"),
               disdate = as.POSIXct(disdate, format = "%m/%d/%y"), 
               surgery_date = as.POSIXct(surgery_date, format = "%m/%d/%y"),
               los = as.numeric(disdate - surgery_date, units = 'days'),
               team_size.periph_team = team_size.all_team - team_size.core_team,
               team_size.prop_periph = team_size.periph_team / team_size.all_team,
               cpt = factor(cpt),
               cpt_grouping = factor(cpt_grouping),
               asa_rating_c = factor(asa_rating_c),
               multiple_procedures = as.factor(ifelse(number_of_procedures > 1, 1, 0))) %>%
  # mutate(facility = recode(facility, 
  #        'HOWARD COUNTY GENERAL HOSPITAL' = 'HC', 
  #        'JOHNS HOPKINS HOWARD COUNTY MEDICAL CENTER' = 'HC',
  #        .default = facility)) |>
        filter(!(or_service %in% Peds_OR_Service),
               asa_rating_c %in% asa_levels,
               age >= 18,
               or_patientclass %in% c('Inpatient','Surgery Admit','Extended Surgical Recovery'), 
               caseclass == 'Elective', 
               surgery_date < lubridate::mdy('12/31/23'),
               # surgery_date < lubridate::ymd('2020-01-01'), # For pre covid
               facility %in% c("JOHNS HOPKINS BAYVIEW MEDICAL CENTER",
                               "SIBLEY MEMORIAL HOSPITAL",
                               "SUBURBAN HOSPITAL",
                               "THE JOHNS HOPKINS HOSPITAL"))

# fam_by_perf_df <- fam_by_perf_df %>%
#         group_by(cpt) %>% # TODO: see what differences there are when regrouping these
#         filter(n() > 30) %>%
#         ungroup()
# 
# # Drop low frequency surgeons
# fam_by_perf_df <- fam_by_perf_df %>%
#          group_by(primarysurgeonid) %>%
#          filter(n() > 30) %>%
#          ungroup()

# Check for any irregular missing data
# table(is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.core_team),
#       is.na(fam_by_perf_df$avg_dyad_exp_52.all_cases.all_team))

# Load function
label_cpt_groups <- function(df) {
        df <- df %>%
                mutate(cpt_grouping = recode_factor(cpt_grouping, 
                                                    "1" = "integumentary & musculoskeletal",
                                                    "2" = "respiratory/hemic/lymphatic",
                                                    "3" = "cardiovascular",
                                                    "4" = "vascular",
                                                    "5" = "upper digestive tract / abdominal",
                                                    "6" = "other digestive tract / abdominal",
                                                    "7" = "hernia repair",
                                                    "8" = "endocrine",
                                                    "9" = "urinary",
                                                    "10" = "nervous system"))
        return(df)
}

# Select relevant variables
lm_df <- fam_by_perf_df %>%
        select(log_id, room_time, los, primarysurgeonid, asa_rating_c, age,
               facility, cpt, cpt_grouping, multiple_procedures,
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
                zeta_prime_4.all_cases.core_team, zeta_prime_4.all_cases.all_team,
                # zeta_4.case_spec.all_team, zeta_4.case_spec.core_team,
                # zeta_prime_4.case_spec.all_team, zeta_prime_4.case_spec.core_team,
                
               # zeta_1.all_cases.core_team,# zeta_1.all_cases.all_team,
               # zeta_prime_1.all_cases.core_team,# zeta_prime_1.all_cases.all_team,
               # zeta_1.case_spec.all_team,zeta_1.case_spec.core_team,
               # zeta_prime_1.case_spec.all_team,zeta_prime_1.case_spec.core_team
               )  %>%
        filter(complete.cases(.)) %>% 
        label_cpt_groups(.) %>%
        group_by(cpt) %>%
        mutate(ext_rt = ifelse(room_time < quantile(room_time, probs = c(.75), na.rm = TRUE)[["75%"]], 0, 1),
               ext_rt = as.factor(ext_rt),
               ext_los = ifelse(los < quantile(los, probs = c(.75), na.rm = TRUE)[["75%"]], 0, 1),
               ext_los = as.factor(ext_los)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(weekend = as.factor(as.numeric(chron::is.weekend(surgery_date))),
               start_hour = lubridate::hour(sched_case_start_dttm),
               after_hours = ifelse(start_hour < 7 | start_hour > 19, 1, 0),
               after_hours = as.factor(after_hours)) %>% 
        ungroup() %>%
        mutate(primarysurgeonid = as.factor(primarysurgeonid), 
               primarysurgeonid = droplevels(primarysurgeonid),
               cpt = as.factor(cpt),
               cpt = droplevels(cpt),
               asa_rating_c = as.factor(asa_rating_c),
               asa_rating_c = droplevels(asa_rating_c),
               multiple_procedures = as.factor(multiple_procedures)) 
lm_df$ext_rt <- relevel(lm_df$ext_rt, "0")
lm_df$ext_los <- relevel(lm_df$ext_los, "0")

# Drop weekend, after hours, and NULL asa scores
lm_df <- lm_df %>%
        filter(asa_rating_c != "NULL")
               # weekend != 1, 
               # after_hours != 1) 

# Load data
load(here::here("Jaxons_work","Data","lm_df.rdat"))

# Standardizing dataset
lm_df <- lm_df %>%
        mutate(across(starts_with(c('zeta','team_fam_disp','avg_dyad','age','team_size')), ~ scale(.,center = TRUE, scale = TRUE))) 

# Extended room time
rt_base <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                 + age + asa_rating_c + multiple_procedures 
                 + weekend + after_hours, 
                      data = lm_df, 
                      family = "binomial",
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
## Core team models
rt_core_zeta_prime_4 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                      + age + asa_rating_c + multiple_procedures 
                      + team_size.core_team + weekend + after_hours 
                      + zeta_prime_4.all_cases.core_team, 
                      data = lm_df, 
                      family = "binomial",
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
rt_core_zeta_prime_12 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                      + age + asa_rating_c + multiple_procedures 
                      + team_size.core_team + weekend + after_hours 
                      + zeta_prime_12.all_cases.core_team, 
                      data = lm_df, 
                      family = "binomial",
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
rt_core_zeta_prime_52 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                      + age + asa_rating_c + multiple_procedures 
                      + team_size.core_team + weekend + after_hours 
                      + zeta_prime_52.all_cases.core_team, 
                      data = lm_df, 
                      family = "binomial",
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sjPlot::tab_model(rt_core_zeta_prime_52)

## All team models
rt_all_zeta_prime_4 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                         + age + asa_rating_c + multiple_procedures 
                         + team_size.core_team + weekend + after_hours 
                         + zeta_prime_4.all_cases.all_team, 
                         data = lm_df, 
                         family = "binomial",
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
rt_all_zeta_prime_12 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                          + age + asa_rating_c + multiple_procedures 
                          + team_size.core_team + weekend + after_hours 
                          + zeta_prime_12.all_cases.all_team, 
                          data = lm_df, 
                          family = "binomial",
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
rt_all_zeta_prime_52 <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                          + age + asa_rating_c + multiple_procedures 
                          + team_size.core_team + weekend + after_hours 
                          + zeta_prime_52.all_cases.all_team, 
                          data = lm_df, 
                          family = "binomial",
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sjPlot::tab_model(rt_all_zeta_prime_52)

tab_model(rt_base, 
          rt_core_zeta_prime_4, rt_core_zeta_prime_12, rt_core_zeta_prime_52,
          rt_all_zeta_prime_4, rt_all_zeta_prime_12, rt_all_zeta_prime_52,
          show.obs = T, show.r2 = T, show.icc = T)

# Extended length of stay
los_base <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                 + age + asa_rating_c + multiple_procedures 
                 + weekend + after_hours, 
                 data = lm_df, 
                 family = "binomial",
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
## Core team models
los_core_zeta_prime_4 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                              + age + asa_rating_c + multiple_procedures 
                              + team_size.core_team + weekend + after_hours 
                              + zeta_prime_4.all_cases.core_team, 
                              data = lm_df, 
                              family = "binomial",
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
los_core_zeta_prime_12 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                               + age + asa_rating_c + multiple_procedures 
                               + team_size.core_team + weekend + after_hours 
                               + zeta_prime_12.all_cases.core_team, 
                               data = lm_df, 
                               family = "binomial",
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
los_core_zeta_prime_52 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                               + age + asa_rating_c + multiple_procedures 
                               + team_size.core_team + weekend + after_hours 
                               + zeta_prime_52.all_cases.core_team, 
                               data = lm_df, 
                               family = "binomial",
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sjPlot::tab_model(los_core_zeta_prime_52)

## All team models
los_all_zeta_prime_4 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                             + age + asa_rating_c + multiple_procedures 
                             + team_size.core_team + weekend + after_hours 
                             + zeta_prime_4.all_cases.all_team, 
                             data = lm_df, 
                             family = "binomial",
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
los_all_zeta_prime_12 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                              + age + asa_rating_c + multiple_procedures 
                              + team_size.core_team + weekend + after_hours 
                              + zeta_prime_12.all_cases.all_team, 
                              data = lm_df, 
                              family = "binomial",
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
los_all_zeta_prime_52 <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility)
                              + age + asa_rating_c + multiple_procedures 
                              + team_size.core_team + weekend + after_hours 
                              + zeta_prime_52.all_cases.all_team, 
                              data = lm_df, 
                              family = "binomial",
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sjPlot::tab_model(los_all_zeta_prime_2)

tab_model(los_base, 
          los_core_zeta_prime_4, los_core_zeta_prime_12, los_core_zeta_prime_52,
          los_all_zeta_prime_4, los_all_zeta_prime_12, los_all_zeta_prime_52,
          show.obs = T, show.r2 = T, show.icc = T)

# Finding L-ratio
## Extended length of stay base model 
los_intercept <- glmer(ext_los ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility), 
                       data = lm_df, 
                       family = "binomial",
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(los_intercept, los_base, test = "LRT")
## Extended length of stay core team models
anova(los_base, los_core_zeta_prime_4, test = "LRT")
anova(los_base, los_core_zeta_prime_12, test = "LRT")
anova(los_base, los_core_zeta_prime_52, test = "LRT")
## Extended length of stay full team models
anova(los_base, los_all_zeta_prime_4, test = "LRT")
anova(los_base, los_all_zeta_prime_12, test = "LRT")
anova(los_base, los_all_zeta_prime_52, test = "LRT")
## Extended room time
rt_intercept <- glmer(ext_rt ~ (1 | primarysurgeonid) + (1 | cpt) + (1 | facility), 
                       data = lm_df, 
                       family = "binomial",
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(rt_intercept, rt_base, test = "LRT")
## Extended room time core team models
anova(rt_base, rt_core_zeta_prime_4, test = "LRT")
anova(rt_base, rt_core_zeta_prime_12, test = "LRT")
anova(rt_base, rt_core_zeta_prime_52, test = "LRT")
## Extended room time full team models
anova(rt_base, rt_all_zeta_prime_4, test = "LRT")
anova(rt_base, rt_all_zeta_prime_12, test = "LRT")
anova(rt_base, rt_all_zeta_prime_52, test = "LRT")

# Create final tables
## Extended Room Time
sjPlot::tab_model(
        rt_base, 
        rt_core_zeta_prime_4, rt_core_zeta_prime_12, rt_core_zeta_prime_52,
        rt_all_zeta_prime_4, rt_all_zeta_prime_12, rt_all_zeta_prime_52,
        #digits = 5,
        title = 'Table 2. Multi-level models for Extended Room Time for all team members',
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

## Extended Length of Stay
sjPlot::tab_model(
        los_base, 
        los_core_zeta_prime_4, los_core_zeta_prime_12, los_core_zeta_prime_52,
        los_all_zeta_prime_4, los_all_zeta_prime_12, los_all_zeta_prime_52,
        #digits = 5,
        title = 'Table 3. Multi-level models for Extended Length of Stay for all team members',
        dv.labels = c(
                # 'M1: Surgeon & Procedure Nesting',
                'M2: M1 + control variables',
                expression("M3: M2 + \U03B6'"['1 month']),
                expression(" M4: M2 + \U03B6'"['1 qualoser']),
                expression("M5: M2 + \U03B6'"['1 year']),
                expression("M3: M2 + \U03B6'"['1 month']),
                expression("M4: M2 + \U03B6'"['1 qualoser']),
                expression("M5: M2 + \U03B6'"['1 year'])
        ),
        show.p = FALSE,
        show.aic = TRUE#,
        # show.loglik = TRUE,
        # file = here('output','tables','exlos.html')
        # file = here('output','tables','extlos_combined.html')
        # show.reflvl = TRUE,
        # show.df = TRUE,
        # show.stat = TRUE,
)
