library(purrr)
library(stringr)
library(tidyverse)

library(ggplot2)
library(sjPlot)
library(sjmisc)
fam_by_perf_df <- merge(fam_metrics, d
                        f_perf, by = 'log_id')


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
    aes(x = room_time, y = zeta_prime_52) +
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

fam_by_perf_df <- merge(fam_metrics, df_perf, by = 'log_id')
fam_by_perf_df <- fam_by_perf_df %>%
  filter(!(or_service %in% Peds_OR_Service)) %>%
  filter(room_time > 15 & room_time < 1000) %>%
  filter(los < 40) %>%
  filter(or_patientclass %in% c('Elective','Surgery Admit','Extended Surgical Recovery')) %>%
  filter(caseclass == 'Elective') %>%
  #filter(between(team_size,4,15)) %>%
  filter(asa_rating_c != '5') %>%
  filter(asa_rating_c != '6') %>%
  filter(asa_rating_c != 'NULL') 

#fam_by_perf_df$room_time <- log(fam_by_perf_df$room_time)

lm_df <- fam_by_perf_df %>%
  select(room_time, los,primarysurgeonid, asa_rating_c, zeta_prime_52) 
lm_df <- lm_df %>%
  na.omit()
lm_df$asa_rating_c <- ordered(lm_df$asa_rating_c, levels = c('1','2','3','4'))

lm_df <- lm_df %>%
  filter(asa_rating_c %in% c('2','3')) %>%
  droplevels()
lm_df <- lm_df %>%
  group_by(primarysurgeonid) %>% filter(n() >= 30) %>%
  ungroup()

lm_df %>%
  ggplot() +
  aes(x = room_time, y = zeta_prime_52) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth() +
  facet_wrap(~ asa_rating_c) +#
  theme_minimal() +
  labs(title = 'Room time by familiarity for ASA 1-4')
lm_df %>%
  ggplot() +
  #aes(x = room_time) +
  aes(x = zeta_prime_52) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  facet_wrap(~ asa_rating_c) +
  theme_minimal() + 
  #labs(title = 'Distribution of room times by ASA 1-4')
  labs(title = 'Distribution of familiarity by ASA 1-4')
x.0 <- lm(los ~ zeta_prime_52, data = lm_df) #log(room_time)
summary(x.0)
plot(x.0)

x.1 <- lmer(los ~ 1 + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.2 <- lmer(los ~ asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.3 <- lmer(los ~ zeta_prime_52 + asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
x.4 <- lmer(los ~ zeta_prime_52*asa_rating_c + (1|primarysurgeonid), data = lm_df, REML = TRUE)
#x.5 <- lmer(room_time ~ zeta_prime_52*asa_rating_c + (zeta_prime_52|primarysurgeonid), data = lm_df, REML = TRUE)
# x.4.a <- lm(log(room_time) ~ zeta_prime_52*asa_rating_c, data = lm_df)
# summary(x.4.a)



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

summary(x.5)
anova(x.4,x.5)

sjPlot::plot_model(x.4, type = 'int',show.p = TRUE, ci.lvl = NA) + theme_classic() + theme(legend.position=c(.8, .8)) #+ legend_style(inside = TRUE, pos = 'top right')# + theme_classic()
tab_model(x.1, x.2, x.3, x.4)

#########################
########## Differences in room_time by ``quantiles
#########################

RT.by.fam.quart <- lm_df %>% group_by(primarysurgeonid) %>%
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

summ_data %>% ggplot(
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

summ_data %>% ggplot(
  aes(
    x=RT.by.fam.quart.diff,
    y=rt.avg)) +
  geom_point(size = 1L, colour = "steelblue") +
  geom_smooth() +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$RT.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  geom_hline(yintercept = mean(summ_data$rt.avg), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Mean room time for top and bottom quartile of familiarity scores by average room time', 
       x = 'Difference betweeen mean room times for top and bottom quartile of familiarity scores (minutes)',
       y = 'Average surgeon room time')

annotations <- data.frame(
  xpos = c(-275),
  ypos =  c(Inf),
  annotateText = c(paste("Across surgeons there is an \naverage difference of",round(mean(summ_data$RT.by.fam.quart.diff),1),"minutes \nbetweeen the top and bottom \nquartiles of familiarity")),
  hjustvar = c(0),
  vjustvar = c(1.5)) #<- adjust

summ_data %>%
  ggplot() +
  aes(x = RT.by.fam.quart.diff) +
  geom_histogram(bins = 25, fill = "steelblue") +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$RT.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Distribution of mean room time for top and bottom quartile of \nfamiliarity scores by surgeon', 
       x = 'Difference betweeen mean room times for top and bottom quartile of familiarity scores (minutes)',
       y = 'Count (surgeons)') +
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))# +
  #scale_color_manual(name = "Differences", values = c(none = "red", mean = "black"))


#########################
########## Differences in los by ``quantiles
#########################

los.by.fam.quart <- lm_df %>% group_by(primarysurgeonid) %>%
  mutate(Fam.quart = ntile(zeta_prime_52,4)) %>%
  group_by(primarysurgeonid,Fam.quart) %>%
  summarize(los_quart_mean = mean(los)) %>%
  pivot_wider(
    names_from = Fam.quart, 
    values_from = los_quart_mean, 
    names_prefix = "los.by.fam.quart.") %>%
  mutate(los.by.fam.quart.diff = los.by.fam.quart.4 - los.by.fam.quart.1)

Surg_ID_descr <- lm_df %>% group_by(primarysurgeonid) %>%
  summarize(case_vol = n(),
            los.avg = mean(los),
            fam.avg = mean(zeta_prime_52),
            los.IQR = IQR(los),
            fam.IQR = IQR(zeta_prime_52))

summ_data <- dplyr::inner_join(los.by.fam.quart, Surg_ID_descr, by = 'primarysurgeonid')

summ_data %>% ggplot(
  aes(
    x=los.by.fam.quart.diff,
    y=case_vol)) +
  geom_point(size = 1L, colour = "steelblue") +
  geom_smooth() +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$los.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  geom_hline(yintercept = mean(summ_data$case_vol), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Mean los for top and bottom quartile of familiarity scores by surgeon case volume', 
       x = 'Difference betweeen mean los for top and bottom quartile of familiarity scores (days)',
       y = 'Average surgeon case volume')

summ_data %>% ggplot(
  aes(
    x=los.by.fam.quart.diff,
    y=los.avg)) +
  geom_point(size = 1L, colour = "steelblue") +
  geom_smooth() +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$los.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  geom_hline(yintercept = mean(summ_data$los.avg), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Mean los for top and bottom quartile of familiarity scores by los', 
       x = 'Difference betweeen mean los for top and bottom quartile of familiarity scores (days)',
       y = 'Average surgeon los')

annotations <- data.frame(
  xpos = c(-7),
  ypos =  c(Inf),
  annotateText = c(paste("Across surgeons there is an \naverage difference of",round(mean(summ_data$los.by.fam.quart.diff),1),"days \nbetweeen the top and bottom \nquartiles of familiarity")),
  hjustvar = c(0),
  vjustvar = c(1.5)) #<- adjust

summ_data %>%
  ggplot() +
  aes(x = los.by.fam.quart.diff) +
  geom_histogram(bins = 25, fill = "steelblue") +
  theme_minimal() + 
  geom_vline(xintercept = 0,  
             color = "red", size=1.5) +
  geom_vline(xintercept = mean(summ_data$los.by.fam.quart.diff), color = 'black',
             size=1, linetype="dotted") +
  labs(title = 'Distribution of mean los for top and bottom quartile of familiarity \nscores by surgeon', 
       x = 'Difference betweeen mean los for top and bottom quartile of familiarity scores (days)',
       y = 'Count (surgeons)') +
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))# +
#scale_color_manual(name = "Differences", values = c(none = "red", mean = "black"))


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