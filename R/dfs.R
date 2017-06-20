# dfs.R
# analyze past seasons/weeks of DFS results
# use different script for predictions of upcoming weeks
# this is retrospective only

dataset.dfs <- function(connxn, seas=2014) {
  # Dataframe of dfs weekly results
  #
  # Args:
  #   connxn: postgresql connection
  #   seas: DEFAULT is 2014, will get 2014-present
  #
  # Returns:
  #   dataframe

  q = paste0(
    "SELECT season_year, week, player_name, player_pos as position, ",
    "team_code, is_home, is_win, opp_imptot, ",
    "dk_salary as salary, dk_points as fppg ",
    "FROM vw_dfs WHERE season_year >= ", seas)
  dbGetQuery(connxn, q)
}

dfSample <- function(df, sampct=.4) {
  # Samples weekly players, ensures some from each position
  #
  # Args:
  #   df: dataframe
  #   sampct: numeric between 0-1
  #
  # Returns:
  #   dataframe

  # first take a sample of the dataframe
  # then add players if position missing
  smpl <- df[sample(nrow(df), round(nrow(df)*sampct, 0)),]
  if (nrow(smpl %>% filter(position == 'DST')) == 0) {
    pos <- df %>% filter(position == 'DST')
    smpl <- rbind(smpl, pos)
  }
  if (nrow(smpl %>% filter(position == 'QB')) == 0) {
    pos <- df %>% filter(position == 'QB')
    smpl <- rbind(smpl, pos)
  }
  if (nrow(smpl %>% filter(position == 'TE')) == 0) {
    pos <- df %>% filter(position == 'TE')
    smpl <- rbind(smpl, pos)
  }
  if (nrow(smpl %>% filter(position == 'RB')) == 0) {
    pos <- df %>% filter(position == 'RB')
    smpl <- rbind(smpl, pos[sample(nrow(pos), round(nrow(pos)/2,0)),])
  }
  if (nrow(smpl %>% filter(position == 'WR')) == 0) {
    pos <- df %>% filter(position == 'WR')
    smpl <- rbind(smpl, pos[sample(nrow(pos), round(nrow(pos)/2,0)),])
  }
  smpl
}

ds.dfs <- function(connxn) {
  # Connects to database and gets dataframe of weekly results
  #
  # Args:
  #   None
  #
  # Returns:
  #   dataframe

  dataset.dfs(connxn)
}

posweek <- function(d, sy, wk) {
  # Filters dataframe by year, week, and positional threshold points
  #
  # Args:
  #   d: dataframe
  #   sy: int such as 2016
  #   wk: int between 1-17
  #
  # Returns:
  #   filtered dataframe

  results = vector("list", length = 5)
  d = d %>%
    filter(season_year==sy, week==wk)
  results[[1]] = d %>%
    filter(position=='QB', fppg >= 20)
  results[[2]] = d %>%
    filter(position=='RB', fppg >= 18)
  results[[3]] = d %>%
    filter(position=='WR', fppg >= 18)
  results[[4]] = d %>%
    filter(position=='TE', fppg >= 12)
  results[[5]] = d %>%
    filter(position=='DST', fppg >= 10)
  do.call(rbind.data.frame, results)
}

randopt <- function(d, n=5, ll=5) {
  # Finds optimal lineups with random sample of player pool
  #
  # Args:
  #   d: dataframe
  #   n: number of lineups in each iteration, default is 5
  #  ll: number of iterations, default is 5
  # first set of results will be global optimal
  # theory is that it is faster to sample player pool then
  # recalculate than it is to look for less optimal lineups
  # performance seems to really degrade if n gets too large
  results = vector("list", length = ll)
  for (i in 1:ll) {
    if (i == 1) {
      results[[i]] <-  Lineups(df, n=n)
    } else {
      results[[i]] <- randomLineups(df, n=n)
    }
  }

  do.call(rbind.data.frame, results)
}

randomLineups <- function(d, n=5, sampct=.4) {
  # Get n lineups from random sample of the dataframe
  #
  # Args:
  #   d: dataframe
  #   n: number of lineups, default is 5
  #   sampct: % to sample, between 0 and 1, default is .4
  #
  # Returns:
  #   dataframe of lineups

  Lineups(dfSample(d, sampct=sampct), n=n)
}

seasonopt <- function(d, s, n=10, min_week=1, max_week=17) {
  # Gets optimal lineups for each week of season
  #
  # Args:
  #   d: dataframe
  #   s: season
  #
  # Returns:
  #  dataframe of optimal lineups

  results <- vector("list", length = (max_week - min_week + 1))
  for (i in min_week:max_week) {
    results[[i]] <- Lineups(posweek(d, s, i), sim.id=i, n=n)
  }
  do.call(rbind.data.frame, results)
}

summopt <- function(d) {
  # Dataframe of summarized optimizer results
  #
  # Args:
  #   d: dataframe
  #
  # Returns:
  #   dataframe summarizing results of optimizer

  d %>%
  group_by(sim.id, lineup.id) %>%
  summarize(pts = sum(fppg))
}

week <- function(d, sy, wk, thresh=5) {
  # Filters dataframe by year, week, and threshold points
  #
  # Args:
  #   d: dataframe
  #   sy: int such as 2016
  #   wk: int between 1-17
  #   thresh: minimum fantasy points scored
  #
  # Returns:
  #   filtered dataframe

  d %>%
    filter(season_year==sy, week==wk, fppg >= thresh)
}
