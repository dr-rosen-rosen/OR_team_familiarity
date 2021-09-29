
# This is for parallelizing with DB functions

# spin up cluster

per_room_time_threshold <- config$per_room_time_threshold
shared_work_experience_window_weeks <- config$shared_work_experience_window_weeks
table_suffix <- config$table_suffix

cl <- makeCluster(10, outfile="")
clusterExport(cl=cl, varlist = c(
  'get_team_members_db','get_team_members_db_sfly','get_perf_hx_db','get_perf_hx_db_sfly','get_team_size',
  'get_zeta','get_zeta_safely', 'per_room_time_threshold', 'shared_work_experience_window_weeks', 'table_suffix')
)
clusterEvalQ(cl, {
  library(magrittr)
  library(dplyr)
  library(RSQLite)
  library(dbplyr)
  library(DBI)
  #con <- DBI::dbConnect(RSQLite::SQLite(), 'OR_data.db')
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname   = 'OR_DB',
                        host     = 'localhost',
                        port     = 5432,
                        user     = 'postgres',
                        password = 'LetMeIn21')
  NULL
})
registerDoParallel(cl)
getDoParWorkers()

system.time({
  borgattizer_par_db(
    df = fam_df
  )
})
parallel::stopCluster(cl)



################## Single test cases
# case <- 271624
# prov <- df_providers %>%
#   filter(log_id == case) %>%
#   filter(staffrole %in% c('Primary','Assisting') | time_duration_mins > (cs$room_time * .5)) %>%
#   dplyr::distinct(staff_id)
# cs <- df_cases_trim %>%
#   filter(log_id == case)
# difftime(cs$out_or_dttm - cs$in_or_dttm, units = 'mins')
# test_cases <- c(1674676,339554,2873672,3043625,291864)
# test_fam_df <- data.frame(
#   LOG_ID = test_cases,
#   team_size = rep(NA,length(test_cases)),
#   zeta = rep(NA,length(test_cases)),
#   zeta_prime = rep(NA,length(test_cases))
# )
# test_fam_df <- merge(test_fam_df, df_cases[c('LOG_ID','SURGERY_DATE','in_or_dttm','out_or_dttm')], by = 'LOG_ID')
#
#
# test_fam_df <- borgattizer_par(test_fam_df)
#
# parallel::stopCluster(cl)
