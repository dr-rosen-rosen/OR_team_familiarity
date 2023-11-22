library(parallel)
library(foreach)
library(iterators)
library(doParallel)
library(dplyr)
library(dbplyr)
library(DBI)
library(RSQLite)
library(tidyverse)
library(glue)
#########################################################################
###############
############## Functions reading in, cleaning, and storing data files
###################
###########################################################################

  
# reading, cleaning, and preparing data files
get_and_clean_cases <- function(data_dir, cases_file, cpt_file){
  # Reads in, formats and cleans surgical case data file, including:
  # formating date times, log id's
  min_sec_ts <- c("AN_START_DATETIME",
                  #"IN_OR_DTTM",
                  "IntubationTime","AnesReady",
                  "Procedure_Start_DateTime", "Incision","Closure","Emergence",
                  "CaseFinish","ExtubationTime","OUT_OR_DTTM","AN_STOP_DATETIME",
                  "SCHED_CASE_START_DTTM"
                  #,"ACTUAL_CASE_START_DTTM"
  )
  df_cases <- readxl::read_excel(here::here(data_dir,cases_file)) %>%
  #df_cases <- readr::read_csv(here(data_dir, cases_file)) %>%
    mutate_if(is.character, ~iconv(.,'UTF-8','UTF-8',sub = '')) %>% # gets rid of odd characters
    #mutate(across(c("AdmDate","DisDate","SURGERY_DATE"), lubridate::mdy)) %>%
    #mutate(across(all_of(min_sec_ts), lubridate::mdy_hm)) %>%
    mutate(across(all_of(min_sec_ts), ~ openxlsx::convertToDateTime(.,origin = "1900-01-01"))) %>%
    mutate(across(c("AdmDate","DisDate"), ~ openxlsx::convertToDate(.,origin = "1900-01-01"))) %>%
    mutate(LOG_ID = as.integer(LOG_ID)) %>%
    rename_with(tolower) %>%
    mutate(surgery_date = as.Date(surgery_date))
  if (!is.na(cpt_file)) {
    # reads in and cleans, and categorizes CPT codes if CPT file provided
    df_cpt <- readxl::read_excel(here::here(data_dir,cpt_file)) %>%
    #df_cpt <- readr::read_csv(here(config$data_dir,config$cpt_file)) %>%
      rename_with(tolower) %>%
      drop_na() %>%
      filter(!grepl("\\D", cpt)) %>% # keeps only strings with all numbers
      mutate(cpt = as.integer(cpt)) %>%
      mutate(log_id = as.integer(log_id)) %>%
      rowwise() %>%
      mutate(cpt_grouping = CPT_grouper(cpt)) %>%
      ungroup() %>%
      drop_na()
    # joins CPT with rest of case information
    df_cases <- dplyr::full_join(df_cases, df_cpt, by = 'log_id')
  } else { # handles updated file with cpt in the original data
    df_cases <- df_cases %>%
      rename(cpt = cpt_default) %>%
      filter(!grepl("\\D",cpt)) %>%
      mutate(
        cpt = as.integer(cpt),
        log_id = as.integer(log_id),
        hsp_account_id = as.integer(hsp_account_id)
      ) %>%
      rowwise() %>%
      mutate(
        cpt_grouping = CPT_grouper(cpt)
      ) %>% ungroup()
  }
  return(df_cases)
}

get_and_clean_providers <- function(data_dir, providers_file, remove_dupes) {
  # reads in and cleans provider file info
  df_providers <- readr::read_csv(here(data_dir,providers_file)) %>%
    mutate_if(is.character, ~iconv(.,'UTF-8','UTF-8',sub = '')) %>% # gets rid of odd characters
    mutate(LOG_ID = as.integer(LOG_ID)) %>%
    mutate(staff_id = as.integer(staff_id)) %>%
    mutate(TIME_DURATION_MINS = as.numeric(TIME_DURATION_MINS)) %>%
    rename_with(tolower)
  if(remove_dupes == TRUE) {
    df_providers <- df_providers %>%
      group_by(log_id, staff_id, surgery_date) %>%
      summarize(
        time_duration_mins = sum(time_duration_mins),
        staffrole = first(na.omit(staffrole))) %>%
      ungroup()
  }
  return(df_providers)
}

CPT_grouper <- function(code) {
  #groupings taken from ACS: https://doi.org/10.1016/j.jamcollsurg.2010.07.021
  if (between(code,10000,29999)) {group_label <- 1}#'integumentary & musculoskeletal'
  else if (between(code,30000,32999)) {group_label <- 2}#''respiratory/hemic/lymphatic'
  else if (between(code,38000,39999)) {group_label <- 2}#''respiratory/hemic/lymphatic'
  else if (between(code,33001,34900)) {group_label <- 3}#''cardiovascular'
  else if (between(code,35001,37799)) {group_label <- 4}#''vascular'
  else if (between(code,40000,43499)) {group_label <- 5}#''upper digestive tract / abdominal'
  else if (between(code,43500,49429)) {group_label <- 6}#''other digestive tract / abdominal'
  else if (between(code,49650,49999)) {group_label <- 6}#''other digestive tract / abdominal'
  else if (between(code,49491,49611)) {group_label <- 7}#''hernia repair'
  else if (between(code,60000,60999)) {group_label <- 8}#''endocrine'
  else if (between(code,50000,59999)) {group_label <- 9}#''urinary'
  else if (between(code,61000,99999)) {group_label <- 10}#''nervous system'
  else {group_label <- NA}
  return(group_label)
}

prep_data_for_fam_metrics <- function(df_cases, df_providers, shared_work_experience_window_weeks, drop_n_of_1,threshold) {
  ### Do any prep for data before calculating metrics here...

  # trims off cases that should not be processed (within window for calculating shared work experience)
  landmark <- (lubridate::ymd(min(df_cases$surgery_date, na.rm = TRUE)) + lubridate::weeks(shared_work_experience_window_weeks))
  print(landmark)
  df_cases_trim <- df_cases %>%
    filter(
        (surgery_date >= landmark)
      )
  df_cases_trim$room_time <- as.numeric((df_cases_trim$out_or_dttm - df_cases_trim$in_or_dttm), units = "mins")
  print(nrow(df_cases_trim))
  all_cases <- intersect(unique(df_cases_trim$log_id),unique(df_providers$log_id))
  fam_df <- data.frame(
    log_id = all_cases
  )

  fam_df <- base::merge(fam_df, df_cases_trim[c('log_id','surgery_date','room_time','cpt')], by = 'log_id')
  fam_df$log_id <- as.integer(fam_df$log_id)
  fam_df <- fam_df[complete.cases(fam_df),]
  if (drop_n_of_1) { # This drops cases that would not run anyway with too few teams to calculate
    rt_df <- get_staff_time_in_room_metrics(
      providers = df_providers,
      cases = df_cases %>% mutate(room_time = as.numeric((out_or_dttm - in_or_dttm), units = "mins")),
      threshold = threshold)
    fam_df <- fam_df %>%
      left_join(rt_df, by = 'log_id') %>%
      filter(tot_tm_50_perc_rt_nona > 1)
  }
  return(fam_df)
}

# managing database
push_cases_providers_to_db <- function(con, df_cases,df_providers) {
  if (!is.null(df_cases)) {
    dbWriteTable(con, "cases", df_cases, overwrite = TRUE)
  }
  if (!is.null(df_providers)) {
    dbWriteTable(con,"providers",df_providers, overwrite = TRUE)
  }
  NULL
}

prep_DB_for_fam_metrics <- function(df_cases, df_providers, table_suffix, 
                                    shared_work_experience_window_weeks,con) {
  # Test if Table exists
  t_name <- paste0('team_comp_metrics',table_suffix)
  if (!grepl('dyad',table_suffix)){ # sets up a borgatti version
    if (!DBI::dbExistsTable(con,t_name)) {
      # If no, create it
      print('No BORGATTI table... do something!')
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
    #tbls <- dbListTables(con)
    c_names <- colnames(tbl(con,t_name))
    c <- paste0('zeta_',shared_work_experience_window_weeks)
    if (!(c %in% c_names) ) { #Test if columns exists for zeta / zeta_prime with shared_work_experience_window_weeks
      stmt <- paste0("ALTER TABLE ",t_name," ADD COLUMN ",c," NUMERIC")
      print(stmt)
      wrt <- dbSendQuery(con,stmt)
      dbClearResult(wrt)
    }
    c <- paste0('zeta_prime_',shared_work_experience_window_weeks)
    if (!(c %in% c_names) ) { #Test if columns exists for zeta / zeta_prime with shared_work_experience_window_weeks
      stmt <- paste0("ALTER TABLE ",t_name," ADD COLUMN ",c," NUMERIC")
      print(stmt)
      wrt <- dbSendQuery(con,stmt)
      dbClearResult(wrt)
    }
  } else { # sets up a dyad table
    if (!DBI::dbExistsTable(con,t_name)) {
      # If no, create it
      print('No DYAD table... do something!')
      all_cases <- intersect(unique(df_cases$log_id),unique(df_providers$log_id))
      fam_metrics_df <- data.frame( # adds basic columns
        log_id = all_cases,
        team_size = rep(NaN,length(all_cases)),
        avg_dyad_exp_52 = rep(NaN,length(all_cases)),
        bottleneck_score_52 = rep(NaN,length(all_cases)),
        team_fam_disp_52 = rep(NaN,length(all_cases))) %>%
        base::merge(., df_cases[c('log_id','surgery_date')], by = 'log_id') %>%
        mutate(log_id = as.integer(log_id))
      DBI::dbWriteTable(con,paste0("team_comp_metrics",table_suffix),fam_metrics_df, overwrite = TRUE)
    } else { print('Table exists... ')}
    c_names <- colnames(tbl(con,t_name))
    v_names <- paste0(c('avg_dyad_exp_','bottleneck_score_','team_fam_disp_'),shared_work_experience_window_weeks)
    for (v in v_names) {
      if (!(v %in% c_names) ) { #Test if columns exists
        stmt <- paste0("ALTER TABLE ",t_name," ADD COLUMN ",v," NUMERIC")
        print(stmt)
        wrt <- dbSendQuery(con,stmt)
        dbClearResult(wrt)
      }
    }
  } # End dyad table set up
  NULL
}

#########################################################################
###############
############## Functions run in borgattizer_par_db using DB
###################
###########################################################################

get_team_members_db <- function(case_ID,thresh) {
  # for a given case ID, get all of the team members who worked that case
  # if a threshold is provided, only return team members who where in the room
  # longer than that proportion of the overall case time
  print(glue::glue("Case ID in get teams is {case_ID} and threshold is {thresh}"))
  t <- dplyr::tbl(con,'providers')
  team_members <- t %>%
    filter(log_id == case_ID) %>%
    dplyr::collect()
  x <- length(unique(team_members$staff_id))
  print(glue::glue("Team size from db: {x}"))
  print(team_members)
  # If there is a cutoff, it will adjust here, otherwise use all team members
  print(!is.na(thresh))
  if (!is.na(thresh)) {
    team_members <- team_members %>%
      filter(staffrole == 'Primary' | !is.na(time_duration_mins)) %>% # drops rows with NA for time duration
      filter((time_duration_mins > thresh) | (staffrole == 'Primary')) # Need to document the staffrole condition
    }
  # this should always be unique from the providers db; but just in case
  x <- length(unique(team_members$staff_id))
  team_members <- unique(team_members$staff_id)
  print(glue::glue("Team size after adjustment: {x}"))
  return(team_members)
}

get_team_members_db_sfly <- purrr::possibly(.f = get_team_members_db, otherwise = NULL)

get_perf_hx_db <- function(r, team_members, con) {
  t <- dplyr::tbl(con,'providers')
  s_date <- as.Date(r$surgery_date)
  print(s_date)
  yr_window_lft <- s_date - lubridate::weeks(shared_work_experience_window_weeks)
  print(yr_window_lft)
  x <- t %>%
    filter(staff_id %in% team_members) %>%
    filter(surgery_date > yr_window_lft, surgery_date < s_date) %>%
    select(staff_id,log_id) %>%
    dplyr::collect() %>%
    table() %>%
    as.data.frame.matrix()
  print(nrow(x))
  if (STTS == TRUE){ # Trims the dataset to only include cases with the same cpt code

    # filter to only those with the same cpt code as current procedure
    CPT <- unique(r$cpt)
    all_cases <- unique(as.integer(colnames(x)))
    t <- dplyr::tbl(con,'cases')
    same_cpt_cases <- t %>%
      filter(log_id %in% all_cases) %>%
      filter(cpt == CPT) %>%
      select(log_id) %>%
      dplyr::collect()
    # drop all columns (cases) from perf history that are not same procedure type
    same_cpt_cases <- unique(as.character(same_cpt_cases$log_id))
    x <- x %>%
      select(same_cpt_cases)
  }
  return(x)
}

get_perf_hx_db_sfly <- purrr::possibly(.f = get_perf_hx_db, otherwise = NULL)

get_team_size <- purrr::possibly(.f = length, otherwise = NULL)

get_zeta <- function(past_perf_hx_mx, prime) {
  zeta_num <- ncol(past_perf_hx_mx) # the columns represent all cases in which a team member worked
  zeta_denom <- past_perf_hx_mx %>% summarize_if(is.numeric, sum, na.rm=TRUE) %>% sum()
  if (prime == FALSE) {
    z <- 1 - (zeta_num / zeta_denom)
  } else {
    prime_offset <- past_perf_hx_mx %>% rowSums() %>% max()
    z <- 1 - ((zeta_num - prime_offset) / (zeta_denom - prime_offset))
  }
  return(z)
}
get_zeta_safely <- purrr::possibly(.f = get_zeta, otherwise = NULL)

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
      past_perf_hx_mx <- get_perf_hx_db_sfly(r = r, team_members = team_members, con = con)
      if (!is.null(past_perf_hx_mx)) {
        zeta <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = FALSE)
        zeta_prime <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = TRUE)

        # if nothing bad happened, save the results to db
        if (!is.null(t_size) && !is.null(zeta) && !is.null(zeta_prime) && !is.null(LOG_ID)) {
          write_stmt <- paste(
                          paste0("UPDATE team_comp_metrics",borg_table_suffix),
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
############## Functions run in dyad based measures using DB
###################
###########################################################################
get_dyad_based_fam <- function(past_perf_hx_mx){
  get_dyad_shared_w_exp <- function(dyad){
    dyad_case_count <- past_perf_hx_mx[dyad,] %>%
    select_if(colSums(.) == 2) %>%
    ncol(.)
    print(dyad_case_count)
    return(dyad_case_count)
  }
  t_size <- nrow(past_perf_hx_mx)
  dyad_shared_work_exp <- utils::combn(seq(1:nrow(past_perf_hx_mx)), 2, get_dyad_shared_w_exp)
  print('dyad_shared_work_exp:')
  print(dyad_shared_work_exp)
  avg_dyad_exp <- mean(dyad_shared_work_exp) # used in Luciano, Parker & others
  avg_team_fam <- sum(dyad_shared_work_exp) / ((t_size*(t_size-1)) / 2) # version from Avgerinos
  print(min(dyad_shared_work_exp))
  print(avg_dyad_exp)
  bottleneck_score <- min(dyad_shared_work_exp) / avg_dyad_exp
  print(bottleneck_score)
  team_fam_disp <- sd(dyad_shared_work_exp)
  return(
    list(
      avg_dyad_exp = avg_dyad_exp,
      avg_team_fam = avg_team_fam,
      bottleneck_score = bottleneck_score,
      team_fam_disp = team_fam_disp
    )
  )
}

get_dyad_safely <- purrr::possibly(.f = get_dyad_based_fam, otherwise = NULL)

dyad_izer_par_db <- function(df){
  foreach::foreach(
    r = iterators::iter(df, by = 'row'),
    .combine = rbind, .noexport = 'con') %do% {
    # get team member IDs
    LOG_ID <- unique(r$log_id)
    print(LOG_ID)
    # team_members <- get_team_members_db(
    #   case_ID = LOG_ID,
    #   thresh = per_room_time_threshold * r$room_time
    #   )
    thresh <- per_room_time_threshold * r$room_time
    case_ID <- LOG_ID
    print(glue::glue("Case ID in get teams is {case_ID} and threshold is {thresh}"))
    
    t <- dplyr::tbl(con,'providers')
    team_members <- t %>%
      filter(log_id == case_ID) %>%
      dplyr::collect()
    x <- length(unique(team_members$staff_id))
    print(glue::glue("Team size from db: {x}"))
    print(team_members)
    # If there is a cutoff, it will adjust here, otherwise use all team members
    print(!is.na(thresh))
    if (!is.na(thresh)) {
      team_members <- team_members %>%
        filter((time_duration_mins > thresh) | (staffrole == 'Primary')) # Need to document the staffrole condition
    }
    team_members <- team_members %>%
      filter((time_duration_mins > thresh) | (staffrole == 'Primary'))
    # this should always be unique from the providers db; but just in case
    team_members <- unique(team_members$staff_id)
    x <- length(unique(team_members$staff_id))
    print(glue::glue("Team size after adjustment: {x}"))
    
    print(team_members)
    # set team size
    t_size <- get_team_size(team_members)
    print(t_size)
    if (t_size >= 2) {
      # pull past performance data adn calculate metrics
      past_perf_hx_mx <- get_perf_hx_db_sfly(r = r, team_members = team_members, con = con)
      if (!is.null(past_perf_hx_mx)) {
        dyad_metrics <- get_dyad_safely(past_perf_hx_mx = past_perf_hx_mx)
        print(dyad_metrics)
        # if nothing bad happened, save the results to db
        if (!is.null(t_size) && !is.na(dyad_metrics$avg_dyad_exp) && !is.na(dyad_metrics$avg_team_fam) && !is.null(LOG_ID)) {
          write_stmt <- paste(
                          paste0("UPDATE team_comp_metrics",dyad_table_suffix),
                          "SET team_size =",paste0(t_size,','),
                          paste0("avg_dyad_exp_",shared_work_experience_window_weeks," = ",dyad_metrics$avg_dyad_exp)
                        )
          ## adds in the bottleneck and dispersion score if there were more than 2 people on the 'team'
          if ((t_size > 2) && !is.na(dyad_metrics$bottleneck_score) && !is.na(dyad_metrics$team_fam_disp)) {
            write_stmt <- paste0(write_stmt,', ',
                              paste0("bottleneck_score_",shared_work_experience_window_weeks," = ",dyad_metrics$bottleneck_score,','),
                              paste0(" team_fam_disp_",shared_work_experience_window_weeks," = ",dyad_metrics$team_fam_disp))
          }
          write_stmt <- paste(write_stmt, "WHERE LOG_ID =",paste0(LOG_ID,';'))
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
############## Functions run in dyad and zeta measures at the same time using DB
###################

cmbd_dyad_borg_par_db <- function(df){
  foreach::foreach(
    r = iterators::iter(df, by = 'row'),
    .combine = rbind, .noexport = 'con') %dopar% {
    # get team member IDs
    LOG_ID <- unique(r$log_id)
    team_members <- get_team_members_db_sfly(
      case_ID = LOG_ID,
      thresh = per_room_time_threshold * r$room_time
      )
    print(team_members)
    # set team size
    t_size <- get_team_size(team_members)
    print(t_size)
    if (t_size >= 2) {
      # pull past performance data adn calculate metrics
      past_perf_hx_mx <- get_perf_hx_db_sfly(r = r, team_members = team_members, con = con)
      if (!is.null(past_perf_hx_mx)) {
        # get zeta metrics
        zeta <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = FALSE)
        zeta_prime <- get_zeta_safely(past_perf_hx_mx = past_perf_hx_mx, prime = TRUE)
        # get dyad-based metrics
        dyad_metrics <- get_dyad_safely(past_perf_hx_mx = past_perf_hx_mx)

        # if nothing bad happened, save the dyad results to db
        if (!is.null(t_size) && !is.na(dyad_metrics$avg_dyad_exp) && !is.na(dyad_metrics$avg_team_fam) && !is.null(LOG_ID)) {
          write_stmt <- paste(
                          paste0("UPDATE team_comp_metrics",dyad_table_suffix),
                          "SET team_size =",paste0(t_size,','),
                          paste0("avg_dyad_exp_",shared_work_experience_window_weeks," = ",dyad_metrics$avg_dyad_exp)
                        )
          ## adds in the bottleneck and dispersion score if there were more than 2 people on the 'team'
          if ((t_size > 2) && !is.na(dyad_metrics$bottleneck_score) && !is.na(dyad_metrics$team_fam_disp)) {
            write_stmt <- paste0(write_stmt,', ',
                              paste0("bottleneck_score_",shared_work_experience_window_weeks," = ",dyad_metrics$bottleneck_score,','),
                              paste0(" team_fam_disp_",shared_work_experience_window_weeks," = ",dyad_metrics$team_fam_disp))
          }
          write_stmt <- paste(write_stmt, "WHERE LOG_ID =",paste0(LOG_ID,';'))
          wrt <- dbSendQuery(con,write_stmt)
          dbClearResult(wrt)
        } else  {print(paste('Failed write dyad measures:',LOG_ID))}
        # if nothing bad happened, save the zeta results to db
        if (!is.null(t_size) && !is.null(zeta) && !is.null(zeta_prime) && !is.null(LOG_ID)) {
          write_stmt <- paste(
                          paste0("UPDATE team_comp_metrics",borg_table_suffix),
                          "SET team_size =",paste0(t_size,','),
                          paste0("zeta_",shared_work_experience_window_weeks," = ",zeta,','),
                          paste0("zeta_prime_",shared_work_experience_window_weeks," = ",zeta_prime),
                          "WHERE LOG_ID =",paste0(LOG_ID,';'))
          wrt <- dbSendQuery(con,write_stmt)
          dbClearResult(wrt)
        } else  {print(paste('Failed write borgatti measures:',LOG_ID))}
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

get_unprocessed <- function(con, df, table_suffix, shared_work_experience_window_weeks) {
  t <- dplyr::tbl(con,paste0('team_comp_metrics',table_suffix))
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
  return(unprocessed)
}

run_audit <- function(con, tname){
  t <- dplyr::tbl(con,tname)
  df <- t %>%
    dplyr::collect()
  locs <- dplyr::tbl(con,'cases') %>%
    dplyr::select(log_id,facility) %>%
    collect()
  df <- df %>%
    full_join(locs, by = 'log_id') %>%
    group_by(facility) %>%
    summarise_all(funs(sum(!is.na(.))))
  return(df)
}

get_staff_time_in_room_metrics <- function(providers,cases,threshold){
  
  rt_df <- cases %>%
    select(log_id,room_time)
  
  rm_tm_team_df <- providers %>%
    left_join(rt_df, by = 'log_id') %>%
    group_by(log_id) %>%
    summarize(
      tot_team_members = n(),
      tot_tm_50_perc_rt = length(staff_id[time_duration_mins > (threshold*room_time) | staffrole == 'Primary']),
      max_rm_time = max(time_duration_mins),
      min_rm_time = min(time_duration_mins),
      num_duration_na = length(staff_id[is.na(time_duration_mins)])
      ) %>% ungroup()
  
  rm_tm_NONA <- providers %>%
    left_join(rt_df, by = 'log_id') %>%
    filter(staffrole == 'Primary' | !is.na(time_duration_mins)) %>%
    group_by(log_id) %>%
      summarize(
        tot_team_members_nona = n(),
        tot_tm_50_perc_rt_nona = length(staff_id[time_duration_mins > (.5*room_time) | staffrole == 'Primary']),
        max_rm_time_nona = max(time_duration_mins),
        min_rm_time_nona = min(time_duration_mins),
        num_duration_na_nona = length(staff_id[is.na(time_duration_mins)])
      ) %>% ungroup()
    
    rm_tm_team_df <- rm_tm_team_df %>%
      full_join(rm_tm_NONA, by = 'log_id')
  return(rm_tm_team_df)
}

#########################################################################
###############
############## Functions for performance measures
###################
###########################################################################

get_perf_fam_metrics <- function(df_cases, table_suffix, con){

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

   # pull all rows with data
  t <- dplyr::tbl(con, paste0('team_comp_metrics',table_suffix)) #team_comp_metrics_fifty_perc_rt #team_comp_metrics
  fam_metrics <- t %>%
     filter(team_size > 0) %>%
     dplyr::collect()
  fam_by_perf_df <- merge(fam_metrics, df_perf, by = 'log_id')
  reutrn(fam_by_perf_df)
}

pullAllTeamCompMetrics <- function(con) {
  # get all table names
  tableNames <- DBI::dbListTables(con)
  # only keep names with 'team_comp_metrics
  tableNames <- grep('team_comp_metrics',tableNames, value = TRUE)
  # create empty df for storing data
  borgTables <- grep('borg',tableNames, fixed = TRUE, value = TRUE)
  dyadTables <- grep('dyad',tableNames,fixed = TRUE, value = TRUE)

  for (tname in borgTables) {
    print(tname)
    #pull all data
    t <- dplyr::tbl(con,tname)
    df <- t %>%
      dplyr::collect()
#    cols <- names(df)
#    cols <- cols[!(cols %in% c('log_id','surgery_date'))]
#    print(cols)
    # df <- df %>% drop_na(all_of(cols))

    df$stts <- dplyr::if_else(grepl('stts',tname,fixed=TRUE),TRUE,FALSE) # set true or false for all cases or stts
    df$coreTeam <- dplyr::if_else(grepl('fifty',tname,fixed=TRUE),TRUE,FALSE) # set true or false for >50% rt or any staff
    if (!exists("borg_data")) {
      borg_data <- data.frame(df)}
    else {
      borg_data <- dplyr::full_join(borg_data,df)#, by = c('log_id','team_size','surgery_date','stts','coreTeam'))
      }
  }
  for (tname in dyadTables) {
    print(tname)
    #pull all data
    t <- dplyr::tbl(con,tname)
    df <- t %>%
      dplyr::collect()
#    cols <- names(df)
#    cols <- cols[!(cols %in% c('log_id','surgery_date'))]
#    df <- df %>% drop_na(all_of(cols))

    df$stts <- dplyr::if_else(grepl('stts',tname,fixed=TRUE),TRUE,FALSE) # set true or false for all cases or stts
    df$coreTeam <- dplyr::if_else(grepl('fifty',tname,fixed=TRUE),TRUE,FALSE) # set true or false for >50% rt or any staff
    if (!exists("dyad_data")) {
      dyad_data <- data.frame(df)}
    else {
      dyad_data <- dplyr::full_join(dyad_data,df)#, by = c('log_id','team_size','surgery_date','stts','coreTeam'))
      }
  }
  all_data <- dplyr::full_join(dyad_data,borg_data, by = c('log_id','team_size','surgery_date','stts','coreTeam'))
  return(all_data)
}

#########################################################################
###############
############## Functions for charts
###################
###########################################################################

#get_descriptive_plots()
