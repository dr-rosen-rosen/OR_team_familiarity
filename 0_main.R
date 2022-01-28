################################################################
################################################################
######  Main file for OR team composition analyses
################################################################
################################################################################

library(here)
library(config)


debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = 'default')
config <- config::get()


source(here('1_funcs.R'), echo = TRUE)
# add get_peds_cases
# calculate LOP, OR Rm time, LOS, on time start / end

#source(here('2_load.R'), echo = TRUE)
# add col specifications on read in

df_cases <- get_and_clean_cases(
  data_dir = config$data_dir,
  cases_file = config$cases_file,
  cpt_file = config$cpt_file)

df_providers <- get_and_clean_providers(
  data_dir = config$data_dir,
  providers_file = config$providers_file,
  remove_dupes = TRUE # this addresses the issue of people being on a single case multiple times
  )

push_cases_providers_to_db(
  df_cases = NA,
  df_providers = df_providers
)

fam_df <- prep_data_for_fam_metrics(
  df_cases = df_cases, #%>% filter(log_id %in% unique(big_zetas$log_id)),
  df_providers = df_providers,
  shared_work_experience_window_weeks = config$shared_work_experience_window_weeks)

prep_DB_for_fam_metrics(
  df_cases = df_cases,
  df_providers = df_providers,
  table_suffix = config$table_suffix,
  shared_work_experience_window_weeks = config$shared_work_experience_window_weeks
)

source('2_make_fam_metrics_db.R')

### Use if processing stops before all cases done
fam_df <- get_unprocessed(
  con = DBI::dbConnect(RPostgres::Postgres(),
                        dbname   = 'OR_DB',
                        host     = 'localhost',
                        port     = 5432,
                        user     = 'postgres',
                        password = 'LetMeIn21'),
  metrics = 'borgatti',
  df = fam_df,
  table_suffix = config$table_suffix,
  shared_work_experience_window_weeks = config$shared_work_experience_window_weeks)

fam_by_perf_df <- get_perf_fam_metrics(
  df_cases = df_cases,
  table_suffix = config$table_suffix
)


#################################

x <- run_audit(
  con = DBI::dbConnect(RPostgres::Postgres(),
                       dbname   = 'OR_DB',
                       host     = 'localhost',
                       port     = 5432,
                       user     = 'postgres',
                       password = 'LetMeIn21'),
  tname = 'team_comp_metrics_v2_fifty_perc_rt' #'team_comp_metrics'#
)
write_csv(x,'audit.csv')

#### Dump contents of the cases and perf metrics so I can work on models while this runs on PC w/ PG DB
tcon <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname   = 'OR_DB',
                     host     = 'localhost',
                     port     = 5432,
                     user     = 'postgres',
                     password = 'LetMeIn21')
all_cases <- tcon %>% tbl('team_comp_metrics_v2_all_staff') %>% collect()
all_cases %>% write_csv(.,'all_cases_all_staff.csv')
all_cases <- tcon %>% tbl('cases') %>% collect()
all_cases %>% write_csv(.,'cases.csv')

big_zetas <- tcon %>% tbl('team_comp_metrics_v2_fifty_perc_rt') %>%
  filter(zeta_prime_1 > 1) %>% collect()
