

# get connection
library(odbc)
library(dbplyr)
library(dplyr)
library(RPostgres)
library(DBI)


con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname   = 'OR_DB',
                      host     = 'localhost',
                      port     = 5432,
                      user     = 'postgres',
                      password = 'LetMeIn21')

# pull all rows with data
t <- dplyr::tbl(con, 'team_comp_metrics_fifty_perc_rt') #team_comp_metrics_fifty_perc_rt #team_comp_metrics
fam_metrics <- t %>%
  filter(team_size > 0) %>%
  dplyr::collect()



library(ggplot2)

f.1.1 <- ggplot(fam_metrics) +
  aes(x = zeta_prime_52) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

f.1.2 <- ggplot(fam_metrics) +
  aes(x = zeta_prime_1) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

f.1.3 <- ggplot(fam_metrics) +
  aes(x = team_size, y = zeta_1) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth() +
  theme_minimal()

f.1.4 <- ggplot(fam_metrics) +
  aes(x = team_size, y = zeta_prime_52) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth() +
  theme_minimal()


f.2.1 <- ggplot(fam_metrics) +
 aes(x = zeta_4) +
 geom_histogram(bins = 100, fill = "#0c4c8a") +
 theme_minimal()

f.2.2 <- ggplot(fam_metrics) +
  aes(x = zeta_prime_4) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

f.2.3 <- ggplot(fam_metrics) +
  aes(x = team_size, y = zeta_4) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth()  +
  theme_minimal()

f.2.4 <- ggplot(fam_metrics) +
 aes(x = team_size, y = zeta_prime_4) +
 geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth()  +
 theme_minimal()

f.3.1 <- ggplot(fam_metrics) +
  aes(x = zeta_52) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

f.3.2 <- ggplot(fam_metrics) +
  aes(x = zeta_prime_52) +
  geom_histogram(bins = 100, fill = "#0c4c8a") +
  theme_minimal()

f.3.3 <- ggplot(fam_metrics) +
  aes(x = team_size, y = zeta_52) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth()  +
  theme_minimal()

f.3.4 <- ggplot(fam_metrics) +
  aes(x = team_size, y = zeta_prime_52) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth()  +
  theme_minimal()

library(patchwork)
test_fig <- (
(f.1.1 | f.1.2 | f.1.3 | f.1.4) /
(f.2.1 | f.2.2 | f.2.3 | f.2.4) /
(f.3.1 | f.3.2 | f.3.3 | f.3.4)
)
show(test_fig)