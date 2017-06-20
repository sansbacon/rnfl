# dst.R
# functions for evaluating defense and special teams

dst.ma <- function(d, n=6) {
  # Adds moving / ema for dst stats
  d %>% group_by(team_code) %>%
    mutate(fpts_rm_c=rollapply(fantasy_points, n, mean, align='right', fill=0),
           fpts_ema_c=EMA(fantasy_points, n=n),
           ptsa_rm_c=rollapply(pts_allowed, n, mean, align='right', fill=0),
           ptsa_ema_c=EMA(pts_allowed, n=n),
           sacks_rm_c=rollapply(sacks, n, mean, align='right', fill=0),
           sacks_ema_c=EMA(sacks, n=n),
           int_rm_c=rollapply(int, n, mean, align='right', fill=0),
           int_ema_c=EMA(int, n=n),
           opp_fptsa_rm=rollapply(opp_fp_alw_last, n, mean, align='right', fill=0),
           opp_fptsa_ema=EMA(opp_fp_alw_last, n=n)
           ) %>%
    mutate(
      fpts_rm = lag(fpts_rm_c, 1),
      fpts_ema = lag(fpts_ema_c, 1),
      ptsa_rm = lag(ptsa_rm_c, 1),
      ptsa_ema = lag(ptsa_ema_c, 1),
      sacks_rm = lag(sacks_rm_c, 1),
      sacks_ema = lag(sacks_ema_c, 1),
      int_rm = lag(int_rm_c, 1),
      int_ema = lag(int_ema_c, 1),
      offdvoa = lag(weighted_off_dvoa, 1),
      defdvoa = lag(weighted_def_dvoa, 1),
      oppoffdvoa = lag(opp_weighted_off_dvoa, 1),
      oppdefdvoa = lag(opp_weighted_def_dvoa, 1)
    ) %>%
    ungroup()
}

dst.factors <- function(d) {
  d$gsis_id = NULL
  d$season_year = as.factor(d$season_year)
  d$week = as.factor(d$week)
  d$team_code = as.factor(d$team_code)
  d$opp = as.factor(d$opp)
  d$is_home = as.factor(d$is_home)
  d$is_win = as.factor(d$is_win)
  d
}

dst.scoring.parts <- function(d, groups=0) {
  # Calculates mean % for parts of DST scoring
  #
  # Args:
  #   d: dataframe
  #
  # Returns:
  #   dataframe with new columns: sackpct, intpct, fumpct, papct,
  #                               dtdpct, sttdpct

  if (groups > 0) {
    dst.scoring.std(d) %>%
      mutate(weekly_rank_grp = ifelse(weekly_rank < 5, 1,
                                      ifelse(weekly_rank < 9, 2,
                                             ifelse(weekly_rank < 13, 3,
                                                    ifelse(weekly_rank < 17, 4,
                                                           ifelse(weekly_rank < 21, 5,
                                                                  ifelse(weekly_rank < 25, 6,
                                                                         ifelse(weekly_rank < 29, 7, 8)))))))) %>%
      mutate(weekly_rank_grp = as.factor(weekly_rank_grp)) %>%
      group_by(weekly_rank_grp) %>%
      summarize(
        sackpct = round(sum(sackpts)/sum(totpts), 2),
        intpct = round(sum(intpts)/sum(totpts), 2),
        fumpct = round(sum(fumpts)/sum(totpts), 2),
        safpct = round(sum(safpts)/sum(totpts), 2),
        papct = round(sum(papts)/sum(totpts), 2),
        dtdpct = round(sum(dtdpts)/sum(totpts), 2),
        sttdpct = round(sum(sttdpts)/sum(totpts), 2))
  } else {
    dst.scoring.std(d) %>%
      mutate(weekly_rank = as.factor(weekly_rank)) %>%
      group_by(weekly_rank) %>%
      summarize(
        sackpct = round(sum(sackpts)/sum(totpts), 2),
        intpct = round(sum(intpts)/sum(totpts), 2),
        fumpct = round(sum(fumpts)/sum(totpts), 2),
        safpct = round(sum(safpts)/sum(totpts), 2),
        papct = round(sum(papts)/sum(totpts), 2),
        dtdpct = round(sum(dtdpts)/sum(totpts), 2),
        sttdpct = round(sum(sttdpts)/sum(totpts), 2))
  }
}

dst.scoring.std <- function(d) {
  # Calculates parts of and total DST scoring
  #
  # Args:
  #   dst: datafram
  #
  # Returns:
  #   dataframe with new columns: sackpts, intpts, fumpts, papts,
  #                               dtdpts, sttdpts, totpts
  d$sackpts = d$sacks
  d$intpts = d$int * 2
  d$fumpts = d$fum_rec * 2
  d$safpts = d$safeties * 2
  d$papts = ifelse(d$pts_allowed == 0, 10,
                   ifelse(d$pts_allowed < 7, 7,
                          ifelse(d$pts_allowed < 14, 4,
                                 ifelse(d$pts_allowed < 21, 1,
                                        ifelse(d$pts_allowed < 28, 0,
                                               ifelse(d$pts_allowed < 35, -1, -4))))))
  d$dtdpts = d$def_td * 6
  d$sttdpts = d$return_td * 6
  d$totpts = d$sackpts + d$intpts + d$fumpts + d$safpts + d$papts + d$dtdpts + d$sttdpts
  d
}

