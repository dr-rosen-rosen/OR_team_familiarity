# Generate descriptive tables for OR Team Familiarity data.
# Author: Jaxon Wu (jwu149)
# Date: 04/05/2024

# Load libraries
library(dplyr)
library(flextable)
library(gtsummary)
library(here)
library(officer)

# Load data
load("lm_df.rdat")

# Set margins for output
sect_properties <- prop_section(
        page_size = page_size(orient = "landscape",
                              width = 8.3,
                              height = 11.7),
        type = "continuous",
        page_margins = page_mar()
)

# Create descriptive table
lm_df %>%
        select(cpt_grouping, age, asa_rating_c, facility,
               multiple_procedures, after_hours, weekend,
               # Team measures
               team_size.core_team, team_size.all_team,
               # Familiarity metrics
               zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team, zeta_prime_4.all_cases.core_team,
               zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team,
               # Outcomes
               ext_los, ext_rt) %>%
        tbl_summary(by = facility,
                    type = list(c(cpt_grouping, age, asa_rating_c,
                                  multiple_procedures, after_hours, weekend) 
                                ~ "categorical",
                                c(age, team_size.core_team, team_size.all_team,
                                  zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team,zeta_prime_4.all_cases.core_team,
                                  zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team) 
                                ~ "continuous2"),
                    statistic = list(c(team_size.core_team, team_size.all_team,
                                       zeta_prime_52.all_cases.core_team, zeta_prime_12.all_cases.core_team,zeta_prime_4.all_cases.core_team,
                                       zeta_prime_52.all_cases.all_team, zeta_prime_12.all_cases.all_team, zeta_prime_4.all_cases.all_team)
                                     ~ "{mean} ({sd})")) %>%
        add_overall() %>%
        modify_header(label = "**Variable**") %>% 
        bold_labels() %>%
        as_flex_table() %>%
        flextable::set_table_properties(layout = 'autofit') %>%
        flextable::save_as_docx(., path = here('predictors_outcomesV2.docx'), pr_section = sect_properties)




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
        set_table_properties(layout = 'autofit') %>%
        save_as_docx(., path = here('predictors_outcomesV2.docx'), 
                     pr_section = sect_properties)
