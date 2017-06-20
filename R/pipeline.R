# not implemented yet
# will take data from various sources and prepare to use in optimizer
# examples include fantasypros, fantasyfootballnerd, and fantasylabs

Jitter <- function(projections, projection.formula='cash') {
  # takes dfr of mean, ceiling, floor
  # returns dfr with fppg column

  if (projection.formula == 'cash') {
    apply(projections, 1, function(x) mean(rnorm(50, x[1], (x[1] - x[3]) / 2)))
  } else {
    apply(projections, 1, function(x) mean(rnorm(50, (x[1] + x[2])/2, x[2] - x[1])))
  }
}

BoundedJitter <- function(n, fppg, ceiling, floor, floor.weight=.1, ceiling.weight=.1, sd.weight=.5) {
  list(rtnorm(n, mean=fppg, sd=(ceiling-floor)*sd.weight,
         lower=floor-(floor.weight*floor),
         upper=ceiling*(1 + ceiling.weight)))
}

FantasyLabsCSVPipeline <- function(fn, projection.formula='cash') {
  proj = read.csv(fn, stringsAsFactors = FALSE)
  wanted = c('PlayerId', 'Player_Name', 'Position', 'Team', 'Opposing_TeamFB', 'Salary', 'AvgPts', 'Ceiling', 'Floor')
  if (projection.formula == 'cash') {
    proj[,wanted]
  } else if (projection.formula == 'tournament') {
    proj[,wanted]
  } else {
    proj[,wanted]
  }
}

FantasyLabsJSONPipeline <- function() {
  wanted = c('PlayerId', 'Player_Name', 'Position', 'Team', 'Opposing_TeamFB', 'Salary', 'AvgPts', 'Ceiling', 'Floor')
  proj = d[,wanted]
  proj$is_home = !grepl('@', Opposing_TeamFB)
}
