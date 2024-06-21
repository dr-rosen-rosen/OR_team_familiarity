##########################################
#####################
##################### Familiarity over time
#####################
##########################################

x <- lm_df |> 
  #select(-contains("prime_52"), -contains("prime_12")) |>
  group_by(
    #facility, 
    week = lubridate::floor_date(surgery_date, "week")) |>
  summarise(across(starts_with('zeta'), ~ mean(.x))) |>
  pivot_longer(
    cols = starts_with('zeta'),
    names_to = 'var',
    values_to = 'familiarity') |>
  mutate(
    team = case_when(
      str_detect(var,'all_team', negate = F) ~ 'full_team',
      str_detect(var, 'core', negate = F) ~ 'core_team'),
    time = factor(case_when(
      str_detect(var,'_4', negate = F) ~ 'One week',
      str_detect(var,'_12', negate = F) ~ 'One month',
      str_detect(var, '_52',negate = F) ~ 'One year'), levels = c('One week','One month','One year'))
    ) |>
  ggplot(aes(x = week, y = familiarity, color = team)) +
  geom_line() + 
  geom_vline(xintercept=as.POSIXct(as.Date("2020-03-15")), linetype="dashed", 
               color = "black", size=.5) + 
  geom_smooth(method = "loess") +
  facet_wrap(~time) + 
  ggthemes::theme_clean()

###################### ITS from: https://rpubs.com/chrissyhroberts/1006858

y <- lm_df |>
  #select(-contains("prime_52"), -contains("prime_12")) |>
  group_by(
    #facility, 
    week = lubridate::floor_date(surgery_date, "week")) |>
  summarise(across(starts_with('zeta'), ~ mean(.x))) |>
  ungroup() |>
  #select(week,zeta_prime_4.all_cases.all_team) |>
  mutate(
    time = row_number(),
    intervention = if_else(week >= as.Date("2020-03-15"), 1, 0),
    post.int.time = c(rep(0,142),1:191)
  )

its.1 <- lm(zeta_prime_52.all_cases.core_team ~ time + intervention + post.int.time, data = y)
summary(its.1)
