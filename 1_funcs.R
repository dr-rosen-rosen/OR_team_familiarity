library(parallel)
library(foreach)
library(iterators)
library(doParallel)
library(dplyr)
library(dbplyr)
library(DBI)
library(RSQLite)
library(tidyverse)
#########################################################################
###############
############## Functions reading in, cleaning, and storing data files
###################
###########################################################################

get_and_clean_cases <- function(data_dir, cases_file){
  min_sec_ts <- c("AN_START_DATETIME","IN_OR_DTTM","IntubationTime","AnesReady",
                  "Procedure_Start_DateTime", "Incision","Closure","Emergence",
                  "CaseFinish","ExtubationTime","OUT_OR_DTTM","AN_STOP_DATETIME",
                  "SCHED_CASE_START_DTTM","ACTUAL_CASE_START_DTTM")
  df_cases <- readr::read_csv(here(data_dir, cases_file)) %>%
    mutate_if(is.character, ~iconv(.,'UTF-8','UTF-8',sub = '')) %>% # gets rid of odd characters
    mutate(across(c("AdmDate","DisDate","SURGERY_DATE"), lubridate::mdy)) %>%
    mutate(across(all_of(min_sec_ts), lubridate::mdy_hm)) %>%
    mutate(LOG_ID = as.integer(LOG_ID)) %>%
    rename_with(tolower)
  return(df_cases)
}

get_and_clean_providers <- function(data_dir, providers_file) {
  df_providers <- readr::read_csv(here('OR_Team_Data','All OR Providers.csv')) %>%
    mutate_if(is.character, ~iconv(.,'UTF-8','UTF-8',sub = '')) %>% # gets rid of odd characters
    mutate(LOG_ID = as.integer(LOG_ID)) %>%
    mutate(staff_id = as.integer(staff_id)) %>%
    mutate(TIME_DURATION_MINS = as.numeric(TIME_DURATION_MINS)) %>%
    rename_with(tolower)
}

prep_data_for_fam_metrics <- function(df_cases, df_providers, shared_work_experience_window_weeks) {
  ### Do any prep for data before calculating metrics here...
  # Creates list of all Peds Service listings
  #Peds_OR_Service <- map(unique(df_cases$OR_Service), keep, str_detect, 'Ped')
  #Peds_OR_Service <- Peds_OR_Service[lapply(Peds_OR_Service, length) > 0]
  #Peds_OR_Service <- str_subset(Peds_OR_Service, 'OPH Pediatric Strabismus', negate = TRUE) # All of these patients appear to be quite old

  # trims off cases that should not be processed (within window for calculating shared work experience)
  landmark <- (lubridate::ymd(min(df_cases$surgery_date)) + lubridate::weeks(shared_work_experience_window_weeks))
  print(landmark)
  df_cases_trim <- df_cases %>%
    filter(
      (facility == 'THE JOHNS HOPKINS HOSPITAL') &
        #(adtpatientclass == 'Inpatient') &
        (surgery_date >= landmark)
      )
  df_cases_trim$room_time <- as.numeric((df_cases_trim$out_or_dttm - df_cases_trim$in_or_dttm), units = "mins")
  print(nrow(df_cases_trim))
  all_cases <- intersect(unique(df_cases_trim$log_id),unique(df_providers$log_id))
  fam_df <- data.frame(
    log_id = all_cases
  )
  print(nrow(fam_df))
  # need to have serial connection to db open first. Then close it, and go back to register DBs
  #df_cases_trim <- get_unprocessed(con = con, metrics = 'borgatti',df = df_cases_trim, table_suffix = table_suffix, shared_work_experience_window_weeks = shared_work_experience_window_weeks)
  print(names(fam_df))
  print(names(df_cases_trim))
  fam_df <- base::merge(fam_df, df_cases_trim[c('log_id','surgery_date','room_time')], by = 'log_id')
  fam_df$log_id <- as.integer(fam_df$log_id)
  fam_df <- fam_df[complete.cases(fam_df),]
  return(fam_df)
}

prep_DB_for_fam_metrics <- function(df_cases, df_providers, table_suffix, shared_work_experience_window_weeks) {
  # set up DB commection
  con <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname   = 'OR_DB',
                       host     = 'localhost',
                       port     = 5432,
                       user     = 'postgres',
                       password = 'LetMeIn21')
  # Test if Table exists
  t_name <- paste0('team_comp_metrics',table_suffix)
  if (!DBI::dbExistsTable(con,t_name)) {
    # If no, create it
    print('No table... do something!')
    all_cases <- intersect(unique(df_cases$log_id),unique(df_providers$log_id))
    fam_metrics_df <- data.frame( # adds basic columsn for a week, a month and a year.
      log_id = all_cases,
      team_size = rep(NaN,length(all_cases)),
      zeta_52 = rep(NaN,length(all_cases)),
      zeta_prime_52 = rep(NaN,length(all_cases)),
      zeta_4 = rep(NaN,length(all_cases)),
      zeta_prime_4 = rep(NaN,length(all_cases)),
      zeta_1 = rep(NaN,length(all_cases)),
      zeta_prime_1 = rep(NaN,length(all_cases))) %>%
      base::merge(., df_cases[c('log_id','surgery_date')], by = 'log_id') %>%
      mutate(log_id = as.integer(log_id))
    DBI::dbWriteTable(con,paste0("team_comp_metrics",table_suffix),fam_metrics_df, overwrite = TRUE)
  } else { print('Table exists... ')}
  tbls <- dbListTables(con)
  tbl <- paste0('zeta_',shared_work_experience_window_weeks)
  if (!(tbl %in% tbls) ) { #Test if columns exists for zeta / zeta_prime with shared_work_experience_window_weeks
    stmt <- paste0("ALTER TABLE ",t_name," ADD COLUMN ",tbl," NUMERIC")
    print(stmt)
    wrt <- dbSendQuery(con,stmt)
    dbClearResult(wrt)
  }
  tbl <- paste0('zeta_prime_',shared_work_experience_window_weeks)
  if (!(tbl %in% tbls) ) { #Test if columns exists for zeta / zeta_prime with shared_work_experience_window_weeks
    stmt <- paste0("ALTER TABLE ",t_name," ADD COLUMN ",tbl," NUMERIC")
    print(stmt)
    wrt <- dbSendQuery(con,stmt)
    dbClearResult(wrt)
  }
  NULL
}
#########################################################################
###############
############## Functions run in borgattizer_par_db using DB
###################
###########################################################################

get_team_members_db <- function(case_ID,thresh) {
  print(thresh)
  t <- dplyr::tbl(con,'providers')
  team_members <- t %>%
    filter(log_id == case_ID) %>%
    dplyr::collect()
  team_members <- team_members %>%
    filter((time_duration_mins > thresh) | (!is.na(staffrole))) # Need to document the staffrole condition
  team_members <- unique(team_members$staff_id)
  return(team_members)
}

get_team_members_db_sfly <- purrr::possibly(.f = get_team_members_db, otherwise = NULL)

get_perf_hx_db <- function(r, team_members, con) {
  t <- dplyr::tbl(con,'providers')
  s_date <- as.Date(r$surgery_date)
  yr_window_lft <- s_date - lubridate::weeks(shared_work_experience_window_weeks)
  x <- t %>%
    filter(staff_id %in% team_members) %>%
    filter(surgery_date > yr_window_lft, surgery_date < s_date) %>%
    select(staff_id,log_id) %>%
    dplyr::collect() %>%
    table() %>%
    as.data.frame.matrix()
  return(x)
}

get_perf_hx_db_sfly <- purrr::possibly(.f = get_perf_hx_db, otherwise = NULL)

borgattizer_par_db <- function(df) {
  foreach::foreach(
    r = iterators::iter(df, by = 'row'),
    .combine = rbind, .noexport = 'con') %dopar% {
    # get team member IDs
    LOG_ID <- unique(r$log_id)
    team_members <- get_team_members_db_sfly(
      case_ID = LOG_ID,
      thresh = per_room_time_threshold * r$room_time
      )
    # set team size
    t_size <- get_team_size(team_members)
    if (t_size >= 2) {
      # pull past performance data adn calculate metrics
      print(paste("Getting past perf hx:",LOG_ID))
      past_perf_hx_mx <- get_perf_hx_db_sfly(r = r, team_members = team_members, con = con)
      if (!is.null(past_perf_hx_mx)) {
        zeta <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = FALSE)
        zeta_prime <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = TRUE)

        # if nothing bad happened, save the results to db
        if (t_size && zeta && zeta_prime && LOG_ID) {
          write_stmt <- paste(
                          paste0("UPDATE team_comp_metrics",table_suffix),
                          "SET team_size =",paste0(t_size,','),
                          paste0("zeta_",shared_work_experience_window_weeks," = ",zeta,','),
                          paste0("zeta_prime_",shared_work_experience_window_weeks," = ",zeta_prime),
                          "WHERE LOG_ID =",paste0(LOG_ID,';'))
          wrt <- dbSendQuery(con,write_stmt)
          dbClearResult(wrt)
        } else  {print(paste('Failed write:',LOG_ID))}
      } else {print(paste("Failed past perf hx:",LOG_ID))}
    } else {print(paste("Failed - fewer than 2 team members:",LOG_ID))}
    NULL
  }
}

#########################################################################
###############
############## Potpouri helpers
###################
###########################################################################

get_unprocessed <- function(con, metrics, df, table_suffix, shared_work_experience_window_weeks) {
  t <- dplyr::tbl(con,paste0('team_comp_metrics',table_suffix))
  if (metrics == 'borgatti'){
    # this querries the DB and returns a new df with only rows w/o data in db
    # made for when the metric generation process dies before finishing and needs
    # to be restarted with just the unfinished cases
    processed <- t %>%
      filter_at(vars(ends_with(as.character(shared_work_experience_window_weeks))), all_vars(!is.na(.))) %>%
      select(log_id) %>%
      dplyr::collect() %>%
      as.list() %>%
      unlist() %>%
      as.numeric()
    unprocessed <- df %>%
    filter(!(log_id %in% processed))
  } else {unprocessed = NULL}
  return(unprocessed)
}


#########################################################################
###############
############## Functions for performance measures
###################
###########################################################################

get_perf_fam_metrics <- function(df_cases, table_suffix){

  # Define performance measures... could store and pull this from DB instead of csv's
  df_cases$room_time <- as.numeric((df_cases$out_or_dttm - df_cases$in_or_dttm), units = "mins")
  df_cases$proc_time <- as.numeric((df_cases$casefinish - df_cases$procedure_start_datetime), units = "mins")
  df_cases$operative_time <- as.numeric((df_cases$closure - df_cases$incision), units = "mins")
  df_cases$los <- as.numeric((df_cases$disdate - df_cases$surgery_date), units = 'days')
  df_cases$slack_time <- as.numeric((df_cases$room_time - df_cases$proc_time), units = 'mins')
  df_cases$ontime <- as.numeric((df_cases$actual_case_start_dttm - df_cases$sched_case_start_dttm), units = 'mins')

  df_perf <- df_cases %>%
    filter(surgery_date < as.Date('2020/1/1')) %>%
    select(log_id,asa_rating_c,caseclass,age,or_patientclass,or_service,adtpatientclass,proclevel,location,facility, primary_proc_display_name, primary_procedure_id,
           room_time, proc_time, operative_time, los, slack_time, ontime,primarysurgeonid)

  con <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname   = 'OR_DB',
                         host     = 'localhost',
                         port     = 5432,
                         user     = 'postgres',
                         password = 'LetMeIn21')

   # pull all rows with data
  t <- dplyr::tbl(con, paste0('team_comp_metrics',table_suffix)) #team_comp_metrics_fifty_perc_rt #team_comp_metrics
  fam_metrics <- t %>%
     filter(team_size > 0) %>%
     dplyr::collect()
  fam_by_perf_df <- merge(fam_metrics, df_perf, by = 'log_id')
  reutrn(fam_by_perf_df)
}

#prep_data_for_analysis(con,)

#########################################################################
###############
############## Functions for charts
###################
###########################################################################

#get_descriptive_plots()


#########################################################################
###############
############## Functions run in borgattizer_par using in memory tibbles
###################
###########################################################################

get_team_members <- function(case_ID) {
  team_members <- df_providers %>%
    filter(log_id == case_ID) %>%
    distinct(staff_id) %>%
    pull()
  team_members
}

get_team_members_sfly <- purrr::possibly(.f = get_team_members, otherwise = NULL)

get_perf_hx <- function(r, team_members) {
  x <- df_providers %>%
    filter((staff_id %in% team_members) & (surgery_date > (r$surgery_date - lubridate::years(1))) & (surgery_date < r$surgery_date)) %>%
    select(staff_id,log_id) %>%
    table() %>%
    as.data.frame.matrix()
  return(x)
}

get_perf_hx_sfly <- purrr::possibly(.f = get_perf_hx, otherwise = NULL)

get_team_size <- purrr::possibly(.f = length, otherwise = NULL)

get_zeta <- function(past_perf_hx_mx, prime) {
  if (prime == FALSE) {
    z = 1 - (ncol(past_perf_hx_mx) / (past_perf_hx_mx %>% summarize_if(is.numeric, sum, na.rm=TRUE) %>% sum()))
  } else {
    prime_offset <- past_perf_hx_mx %>% rowSums() %>% max()
    z = 1 - ((ncol(past_perf_hx_mx) - prime_offset) / ((past_perf_hx_mx %>% summarize_if(is.numeric, sum, na.rm=TRUE) %>% sum()) - 1))
  }
  z
}
get_zeta_safely <- purrr::possibly(.f = get_zeta, otherwise = NULL)


borgattizer_par <- function(df) {
  foreach::foreach(r = iterators::iter(df, by = 'row'), .combine = rbind) %dopar% {
    # get team member IDs
    LOG_ID <- unique(r$log_id)
    team_members <- get_team_members_sfly(LOG_ID)
    # set team size
    t_size <- get_team_size(team_members)
    # pull past performance data
    past_perf_hx_mx <- get_perf_hx_sfly(r = r, team_members = team_members)
    data.frame(
      log_id=LOG_ID,
      team_size=t_size,
      zeta=zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = FALSE),
      zeta_prime=zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = TRUE)
    )
  }
}
