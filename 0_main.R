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
  providers_file = config$providers_file)

# push_cases_providers_to_db(
#   df_cases = df_cases,
#   df_providers = NA
# )

fam_df <- prep_data_for_fam_metrics(
  df_cases = df_cases,
  df_providers = df_providers,
  shared_work_experience_window_weeks = config$shared_work_experience_window_weeks)

prep_DB_for_fam_metrics(
  df_cases = df_cases,
  df_providers = df_providers,
  table_suffix = config$table_suffix,
  shared_work_experience_window_weeks = config$shared_work_experience_window_weeks
)

source('2_make_fam_metrics_db.R')

### Use if processing stops before all casees done
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
