# datasets.R
# contains database connection + queries

dbcon <- function(conf='~/.rconfig') {
  source(conf)
  drv <- dbDriver('PostgreSQL')
  dbConnect(drv, dbname = nfldb.database, user=nfldb.username, password=nfldb.password)
}

dataset.dst <- function(connxn, seas=2009) {
  q = paste0(
    "SELECT *, row_number() OVER (PARTITION BY season_year, week ORDER BY fantasy_points DESC) as weekly_rank ",
    "FROM vw_dst WHERE season_year >= ", seas)
  dbGetQuery(connxn, q)
}

dataset.td <- function(connxn, seas=2009) {
  q = paste0(
    "SELECT season_year, week, team, opp, phase, passing_yds, passing_tds, rushing_yds, rushing_tds ",
    "FROM offense_tds ",
    "WHERE season_year >= ", seas)
  dbGetQuery(connxn, q)
}

dataset.team.offense <- function(connxn, seas=2009) {
  q = paste0(
    "SELECT *, (pass_td + rush_td) as td, ",
    "row_number() OVER (PARTITION BY season_year ORDER BY play_count_tip DESC) as plays_rank, ",
    "row_number() OVER (PARTITION BY season_year ORDER BY (pass_td + rush_td) DESC) as td_rank ",
    "FROM teamstats_offense_yearly WHERE season_year >= ", seas,
    " ORDER BY season_year, pass_att DESC")
  dbGetQuery(connxn, q)
}

dataset.team.plays <- function(connxn, seas=2009) {
  q = paste0(
    "SELECT season_year, week, game_date, team_code, opp, ",
    "row_number() OVER (PARTITION BY team_code ORDER BY season_year, week) AS gamenum, ",
    "s, consensus_spread, consensus_game_ou, consensus_implied_total, is_win, plays ",
    "FROM vw_teamplays ",
    "WHERE gsis_id <> '2014122809' AND season_year >= ", seas,
    " ORDER BY gsis_id, team_code")
  dbGetQuery(connxn, q)
}

dataset.team.targets <- function(con, seas=2009) {
  q = paste0("SELECT season_year, team, pass_cmp, pass_att, pass_yds, pass_int, pass_td,
  row_number() OVER (PARTITION BY season_year ORDER BY pass_att DESC) as pass_att_rank
  FROM teamstats_offense_yearly
  WHERE season_year >= ", seas,
             "ORDER BY season_year, pass_att DESC")
  dbGetQuery(con, q)
}
