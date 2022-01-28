
# This is for parallelizing with DB functions

# spin up cluster

per_room_time_threshold <- config$per_room_time_threshold
shared_work_experience_window_weeks <- config$shared_work_experience_window_weeks
table_suffix <- config$table_suffix
STTS <- config$STTS

cl <- makeCluster(10, outfile="")
clusterExport(cl=cl, varlist = c(
  'get_team_members_db','get_team_members_db_sfly','get_perf_hx_db','get_perf_hx_db_sfly','get_team_size',
  'get_zeta','get_zeta_safely', 'per_room_time_threshold', 'shared_work_experience_window_weeks', 
  'STTS','table_suffix')
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

borgattizer_par_db(
  df = fam_df
)
parallel::stopCluster(cl)

