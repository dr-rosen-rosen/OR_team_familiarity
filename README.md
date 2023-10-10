# OR_team_familiarity
Analysis of OR team familiarity and outcomes

**Set up steps**
1. Install Postgres and create a db named 'OR_DB'; save all db info in config file
2. Set up all other variables in config file (for reading in case and provider data)
3. Run the 'get_and_clean_cases' and 'get_and_clean_providers' to read in and process the csv files.
4. Pass resulting dataframes to 'push_cases_providers_to_db' to load into db

**Generating familiarity metrics**
0. We have flexibility in how we generate measures,but in general we can only generate one set at a time. These options are set in the config file (under 'analysis run' block). We can vary *timewindow* over which familiarity is calculated, *STTS* (False = all cases, True = only the same cpt code is considered), and *proportion of time a person needs to be in the case before being considered present* (we either us no cut off for full team, or .5 for 'core team'). We can generate either borgatti or 'dyad' based measures based on these parameters (separate scripts, but there is one that will run both). So, before starting, make sure the config file is set appropriately, and then do following.
1. Run the prep_data_for_fam_metrics script (this will use timewindow set in config to trim list of cases for which familiarity measures are appropriate... i.e., cut those in the leading timewindow).
2. Run the prep_DB_for_fam_metrics script to make sure there is a table in the db to hold the results for the metrics being calculated.
3. On the '2_make_fam_metrics_db.R' script, make sure the correct function is included (borgatti, dyad, or combined) and the others commented out.
4. source / run '2_make_fam_metrics_db.R'

For debugging the scripts (borgatti or dyad), it is best to set '%dopar%' to '%do%' in the main foreach loop. This makes it run sequentially and not in parallel, which is obviously much slower, but error messages are tough to capture when running in parallel. When the code is running well, you can switch back to dopar and it will go much faster (but still take a long time)