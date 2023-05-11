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

df_cmb <- df_cases %>%
  full_join(df_fam)



Peds_OR_Service <- map(unique(df_cmb$or_service), keep, str_detect, 'Ped')
Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0] # just clears out junk

asa_levels <- c('1','2','3','4') #c('2','3')
fam_by_perf_df <- df_cmb %>%

  ####### ADULT filters
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(or_patientclass %in% c('Inpatient','Surgery Admit','Extended Surgical Recovery')) %>%
  filter(caseclass == 'Elective') %>%
  filter(age >= 18) %>%
  filter(facility != "THE JOHNS HOPKINS ALL CHILDREN'S HOSPITAL") %>%
  
  ######## PEDS filters
  # filter((or_service %in% Peds_OR_Service)) %>%
  # filter(caseclass == 'Elective') %>%
  
  ######## Everybody filters
  mutate(room_time = as.numeric((out_or_dttm - in_or_dttm), units = "mins")) %>%
  mutate(los = as.numeric((disdate - surgery_date), units = 'days')) %>%
  #filter(between(room_time, 15,1000)) %>%
  #filter(between(team_size,4,15)) %>%
  filter(surgery_date < lubridate::ymd('2020-01-01')) %>%
  #filter(facility == 'THE JOHNS HOPKINS HOSPITAL') %>%
  # mutate(los = ifelse(los >= 30, 30, los)) %>%
  mutate(cpt = factor(cpt)) %>%
  mutate(cpt_grouping = factor(cpt_grouping)) %>%
  mutate(multiple_procedures = ifelse(number_of_procedures > 1, 1, 0)) %>%
  mutate(multiple_procedures = factor(multiple_procedures)) %>%
  #mutate(multiple_procedures = ordered(multiple_procedures, levels = c(0,1))) %>%
  filter(asa_rating_c %in% asa_levels) %>%
  mutate(asa_rating_c = factor(asa_rating_c)) 
  #mutate(asa_rating_c = ordered(asa_rating_c,levels = asa_levels))
  
# fam_by_perf_df <- fam_by_perf_df %>% # there were only ~120 cases
#   filter(facility != "HOWARD COUNTY GENERAL HOSPITAL")

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
  select(log_id,room_time, los, primarysurgeonid, asa_rating_c, zeta_52, 
         team_size, zeta_prime_52, facility, cpt, multiple_procedures, facility) %>%
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
  flextable::save_as_docx(., path = 'cpt_facility.docx', pr_section = sect_properties)

lm_df %>%
  select(
    asa_rating_c, multiple_procedures, team_size, zeta_52, ext_los, ext_rt, facility) %>%
  tbl_summary(
    by = facility
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = 'autofit') %>%
  flextable::save_as_docx(., path = 'predictors_outcomes.docx', pr_section = sect_properties)

# Scale variables 
lm_df <- lm_df %>%
  mutate(across(starts_with(c('zeta','team_size')), ~ scale(.,center = TRUE))) 

x.0 <- lm(as.formula('log(room_time) ~ 1'), data = lm_df) #log(room_time)
# Add cpt code as random var
f <- as.formula(log(room_time) ~ 1 + (1|cpt))
x.1 <- lmer(f, data = lm_df, REML = TRUE)
anova(x.1,x.0)
summary(x.1)
lattice::qqmath(x.1)

# x.2 <- lmer(f, data = lm_df, REML = TRUE)
x.2 <- update(x.1, . ~ . + (1|primarysurgeonid))
anova(x.1,x.2)
summary(x.2)
lattice::qqmath(x.2)

# Test for facility??
x.2a <- update(x.2, . ~ . + (1|facility))
summary(x.2a)
anova(x.2, x.2a)

# Add ASA
x.3 <- update(x.2, . ~ . + asa_rating_c + multiple_procedures)
anova(x.2,x.3)
summary(x.3)
lattice::qqmath(x.3)

# Add familiarity
x.4 <- update(x.3, . ~ . + zeta_52) #zeta_1 + zeta_4 + zeta_12 + 
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
              + multiple_procedures:zeta_52
                # + team_size:asa_rating_c 
                # + team_size:multiple_procedures
              # + asa_rating_c:multiple_procedures
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
              #+ team_size:asa_rating_c:multiple_procedures
)
anova(x.6, x.7)
summary(x.7)
lattice::qqmath(x.7)

tab_model(
  x.0,x.2, x.3, x.5,x.6, x.7,
  show.ci = FALSE#,
  # dv.labels = c('M0: Null model','M1: Nesting','M2: Risk','M3: Team composition'),
  # Peds labels
  # pred.labels = c(
  #   "Intercept", "ASA [2]","ASA [3]", "ASA [4]",
  #   "Mult. procs.","Zeta","Team size",
  #   "ASA [2] * Zeta","ASA [3] * Zeta", "Mult. procs. * Zeta"),
  # Adult labels
  # pred.labels = c(
  #   "Intercept", "ASA [2]","ASA [3]","ASA [4]","Mult. procs.","Familiarity (zeta)","Team size")#,
    #"ASA [2] * Zeta","ASA [3] * Zeta", "ASA [4] * Zeta", "Mult. procs.) * Zeta"),
  #file = here('Model_table_adult.html')
)
plot_model(x.7, type = "pred", terms = c('zeta_52','asa_rating_c','multiple_procedures'), mdrt.values = "meansd") + theme_tufte()

p <- sjPlot::plot_model(
  x.5, 
  type = 'pred', 
  show.p = TRUE, 
  ci.lvl = NULL,
  terms = c('zeta_52','asa_rating_c','multiple_procedures')
  ) + 
  theme_tufte()# +
  # theme(legend.position = 'bottom') +
  # scale_color_hue(labels = c("No", "Yes")) +
  # labs(
  #   title = 'Predicted values of room time', 
  #   y = 'Room time (minutes)', 
  #   x = 'Familiarity (Zeta)',
  #   color = 'Multiple Procedures'
  #   )
show(p)

p0 <- sjPlot::plot_model(
  x.6,
  type = 'eff'
)#+ theme_tufte()
show(p0$zeta_52 + theme_tufte())


p_peds <- p_peds + labs(title = 'Pediatric')
p_adult <- p_adult + labs(title = 'Adult')
p_adult + p_peds + 
  plot_annotation(title = 'Predicted values of room time') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')



?plot_model


#########################################
#########################################
########## GLMER on dichotomous outcomes
#########################################
#########################################


z.0 <- lm(ext_rt ~ 1, data = lm_df)#, family = poisson)
summary(z.0)

z.1 <- glmer(ext_rt ~ (1|primarysurgeonid) + (1|cpt) + (1|facility), data = lm_df, family = binomial)
summary(z.1)
anova(z.1,z.0)
z.2 <- update(z.1, .~. + asa_rating_c + multiple_procedures)
summary(z.2)
anova(z.1,z.2)
z.3 <- update(z.2, .~. + team_size + zeta_52)
summary(z.3)
anova(z.2,z.3)
# z.4 <- update(z.3, .~. + 
#                 zeta_52*multiple_procedures + 
#                 team_size*multiple_procedures +
#                 zeta_52*asa_rating_c +
#                 zeta_52*multiple_procedures*asa_rating_c
#                 )
# summary(z.4)
lme4::allFit(z.3)
blmeco::dispersion_glmer(z.3)
overdisp_fun(z.3)
aods3::gof(z.3)
tab_model(z.0, z.1, z.2,z.3,
          show.aic = TRUE#,
          # dv.labels = c('M0: Null model','M1: Nesting','M2: Risk','M3: Team composition'),
          # Peds labels
          # pred.labels = c(
          #   "Intercept", "ASA [2]","ASA [3]", "ASA [4]",
          #   "Mult. procs.","Zeta","Team size"),
          #file = here('output','tables','MLM_rt.html')
          )
p_rt <- sjPlot::plot_model(z.3,
                   type = 'eff', 
                   terms = 'zeta_52'
                   ) + 
  labs(x = 'Shared work experiences (zeta)',
       y = 'Probability of an extended room time',
       title = 'Predicted probabilities of an extended room time across shared work experiences') +
  theme_tufte() +
  geom_vline(xintercept = mean(lm_df$zeta_52),  
             color = "red", size=1) +
  geom_vline(xintercept = quantile(lm_df$zeta_52, probs = c(.75), na.rm = TRUE)[['75%']],  
             color = "black", size=.75, linetype="dotted") +
  geom_vline(xintercept = quantile(lm_df$zeta_52, probs = c(.25), na.rm = TRUE)[['25%']],
             color = "black", size=.75, linetype="dotted") +
  geom_vline(xintercept = mean(lm_df$zeta_52) - sd(lm_df$zeta_52),
             color = "red", size=.75, linetype="dashed")  +
  geom_vline(xintercept = mean(lm_df$zeta_52) + sd(lm_df$zeta_52),
             color = "red", size=.75, linetype="dashed") 

p_los / p_rt
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




#########################################
#########################################
########## Functions for prepping data for analysis
#########################################
#########################################

#prep_data_for_quartile_charts <- function(df) {
outcome <- sym('room_time')

grouping <- sym('cpt') #'primarysurgeonid
familiarity <- sym('zeta_52')
fam.quart <- lm_df %>% group_by({{grouping}}) %>%
  mutate(Fam.quart = ntile({{familiarity}},4)) %>%
  group_by({{grouping}},Fam.quart) %>%
  summarize(quart_mean = mean({{outcome}})) %>%
  pivot_wider(
    names_from = Fam.quart, 
    values_from = quart_mean, 
    names_prefix = "Outcome.by.fam.quart.") %>%
  mutate(Outcome.by.fam.quart.diff = Outcome.by.fam.quart.4 - Outcome.by.fam.quart.1)

Surg_ID_descr <- lm_df %>% group_by({{grouping}}) %>%
  summarize(vol = n(),
            rt.avg = mean({{outcome}}),
            fam.avg = mean({{familiarity}}),
            rt.IQR = IQR({{outcome}}),
            fam.IQR = IQR({{familiarity}}))

summ_data <- dplyr::inner_join(fam.quart, Surg_ID_descr, by = as.character(grouping))

  #  return(summ_data)}

summ_data <- prep_data_for_quartile_charts(lm_df) 
summ_data %>%
  ggplot(
    aes(
      x=RT.by.fam.quart.diff,
      y=case_vol)) +
  geom_point(size = 1L, colour = "steelblue") +
  geom_smooth() +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$RT.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  geom_hline(yintercept = mean(summ_data$case_vol), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Mean room time for top and bottom quartile of familiarity scores by surgeon case volume', 
       x = 'Difference betweeen mean room times for top and bottom quartile of familiarity scores (minutes)',
       y = 'Average surgeon case volume')


annotations <- data.frame(
  xpos = c(-275),
  ypos =  c(Inf),
  annotateText = c(paste("Across surgeons there is an \naverage difference of",round(mean(summ_data$Outcome.by.fam.quart.diff),1),"minutes \nbetweeen the top and bottom \nquartiles of familiarity")),
  hjustvar = c(0),
  vjustvar = c(1.5)) #<- adjust

summ_data %>%
  ggplot() +
  aes(x = Outcome.by.fam.quart.diff) +
  geom_histogram(bins = 25, fill = "steelblue") +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$Outcome.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Distribution of mean room time for top and bottom quartile of \nfamiliarity scores by surgeon', 
       x = 'Difference betweeen mean room times for top and bottom quartile of familiarity scores (minutes)',
       y = 'Count (surgeons)') +
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))# +


