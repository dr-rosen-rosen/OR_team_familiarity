library(tidyverse)
library(here)
library(DBI)
library(config)
config <-config::get('default')

df <- readr::read_csv(here(config$data_dir,config$cpt_file))


CPT_grouper <- function(code) {
  #groupings taken from ACS: https://doi.org/10.1016/j.jamcollsurg.2010.07.021
  if (between(code,10000,29999)) {group_label <- 1}#'integumentary & musculoskeletal'}
  else if (between(code,30000,32999)) {group_label <- 2}#''respiratory/hemic/lymphatic'}
  else if (between(code,38000,39999)) {group_label <- 2}#''respiratory/hemic/lymphatic'}
  else if (between(code,33001,34900)) {group_label <- 3}#''cardiovascular'}
  else if (between(code,35001,37799)) {group_label <- 4}#''vascular'}
  else if (between(code,40000,43499)) {group_label <- 5}#''upper digestive tract / abdominal'}
  else if (between(code,43500,49429)) {group_label <- 6}#''other digestive tract / abdominal'}
  else if (between(code,49650,49999)) {group_label <- 6}#''other digestive tract / abdominal'}
  else if (between(code,49491,49611)) {group_label <- 7}#''hernia repair'}
  else if (between(code,60000,60999)) {group_label <- 8}#''endocrine'}
  else if (between(code,50000,59999)) {group_label <- 9}#''urinary'}
  else if (between(code,61000,99999)) {group_label <- 10}#''nervous system'}
  else {group_label <- NA}
  return(group_label)
  }

cpt_grouped <- df %>%
  drop_na() %>%
  filter(!grepl("\\D", CPT)) %>% # keeps only strings with all numbers
  mutate(CPT = as.integer(CPT)) %>%
  mutate(log_id = as.integer(log_id)) %>%
  rowwise() %>%
  mutate(groupings = CPT_grouper(CPT)) %>%
  drop_na()
cpt_group_freq <- cpt_grouped %>%
  group_by(groupings) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

##### Update CPT codes and code groupings in cases table

update_cpt_db <- function(cpt_grouped){
  con <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname   = 'OR_DB',
                       host     = 'localhost',
                       port     = 5432,
                       user     = 'postgres',
                       password = 'LetMeIn21')
  for (row in 1:nrow(cpt_grouped)){
    write_stmt <- paste(
                    "UPDATE cases",
                    paste0("SET cpt =",as.character(cpt_grouped[row,'CPT']),','),
                    paste0("cpt_group =",as.character(cpt_grouped[row,'groupings'])),
                    paste0("WHERE log_id =",as.character(cpt_grouped[row,'log_id']),';'))
    #print(write_stmt)
    wrt <- DBI::dbSendStatement(con,write_stmt)
    dbClearResult(wrt)
  }
  NULL
}

update_cpt_db(cpt_grouped = cpt_grouped)
