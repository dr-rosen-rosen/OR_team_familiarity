#########################################
#########################################
########## Work arounds for modeling data on MAC & Running metrics on PC
#########################################
#########################################

df_cases <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/cases.csv')
df_fam <- readr::read_csv('/Users/mrosen44/OneDrive - Johns Hopkins University/OR_Team_Data/all_cases_w50per_rt.csv')

df_cmb <- df_cases %>%
  full_join(df_fam)

library(ggplot2)
library(dplyr)

p <- df_cmb %>%
  filter(facility == 'THE JOHNS HOPKINS HOSPITAL') %>%
  #mutate()
  ggplot(aes(y=zeta_prime_1, x=surgery_date)) +
  geom_point() + 
  xlab("")
p