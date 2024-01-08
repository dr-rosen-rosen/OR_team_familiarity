
# This is for parallelizing with DB functions

# spin up cluster

per_room_time_threshold <- config$per_room_time_threshold
shared_work_experience_window_weeks <- config$shared_work_experience_window_weeks
dyad_table_suffix <- config$dyad_table_suffix
borg_table_suffix <- config$borg_table_suffix
STTS <- config$STTS

cl <- makeCluster(4, outfile="")
clusterExport(cl=cl, varlist = c(
  'get_team_members_db','get_team_members_db_sfly','get_perf_hx_db','get_perf_hx_db_sfly','get_team_size',
  'get_zeta','get_zeta_safely',
  'get_dyad_safely','get_dyad_based_fam',
  'per_room_time_threshold', 'shared_work_experience_window_weeks',
  'STTS',#'table_suffix',
  'dyad_table_suffix', 'borg_table_suffix','config')
)
clusterEvalQ(cl, {
  library(magrittr)
  library(dplyr)
  library(RSQLite)
  library(dbplyr)
  library(DBI)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname   = config$db_name,
                        host     = 'localhost',
                        port     = config$port,
                        user     = config$db_user,
                        password = config$db_pw)
  NULL
})
registerDoParallel(cl)
getDoParWorkers()

cmbd_dyad_borg_par_db( #cmbd_dyad_borg_par_db(#dyad_izer_par_db( #borgattizer_par_db(
  df = fam_df
)

get_team_consistency_db(
  df = fam_df
)
parallel::stopCluster(cl)
