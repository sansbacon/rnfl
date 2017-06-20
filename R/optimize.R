LineupConfig = function(configfile) {
  # NOT SURE WHAT THIS DOES
  l = ini::read.ini(configfile)
  locked.players = unlist(strsplit(l$locks$locked_players, ",\\s?"))
}

PositionMatrix <- function(data, intercept='DST') {
  # Takes a dataframe, returns matrix of positions
  #
  # Args:
  #   data: dataframe
  #   intercept: default 'DST' (no need to change)
  #
  # Returns:
  #   matrix of dummy variables

  dummy.vars = matrix(as.numeric(model.matrix(fppg ~ position, data=data)), ncol=5)
  dummy.vars[,1] = ifelse(data$position == intercept,1,0)
  dummy.vars
}

Lineup <- function(data, locked.players=NULL, excluded.players=NULL, excluded.teams=NULL,
                   qb.team=NULL, qb.depth=0, qb.stack.positions=c('WR', 'TE', 'RB'),
                   opp.team=NULL, opp.depth=0, opp.stack.positions=c('WR', 'TE', 'RB'), max.points = 500) {

  # dfr (data.frame): columns player, position, team, salary, fppg
  # locked.players (character): e.g. c('Joe Flacco', 'Vikings')
  # excluded.players (character): e.g. c('Sam Bradford', 'Ravens')
  # excluded teams (character): This is not yet implemented
  # qb.team (character): e.g. 'Bal' or 'GB'
  # depth (integer): number of players to stack with QB
  # stack_positions ()

  # column numbers:
  # 1-8: dst, qb, rb, te, wr, rb, te, wr
  # 9: locked players
  # 10: excluded players
  # 11: excluded teams
  # 12: qb teammate
  # 13: total players
  # 14: salary
  # 15: max_points

  dummy.vars = PositionMatrix(data)

  # create columns for is_locked and is_excluded
  # nlock ensures that no error where no locked players
  if (is.null(locked.players)) {
    locked.p = ifelse(data$player == 0, 1, 0)
    nlock = 0
  } else {
    locked.p = ifelse(is.element(data$player, locked.players),1,0)
    nlock = length(locked.players)
  }

  if (is.null(excluded.players)) {
    excluded.p = ifelse(data$player == 0, 1, 0)
  } else {
    excluded.p = ifelse(is.element(data$player, excluded.players),1,0)
  }

  if (is.null(excluded.teams)) {
    excluded.t = ifelse(data$team == 0, 1, 0)
  } else {
    excluded.t = ifelse(is.element(data$team, excluded.teams),1,0)
  }

  # qb stack, if no qb.team, then will not enforce requirement
  if (is.null(qb.team)) {
    qb.t = ifelse(data$team == 0, 1, 0)
  } else {
    qb.t = ifelse(data$team == qb.team & is.element(data$position, qb.stack.positions),1,0)
  }

  # opp stack, if no opp_team, then will not enforce requirement
  if (is.null(opp.team)) {
    opp.t = ifelse(data$team == 0, 1, 0)
  } else {
    opp.t = ifelse(data$team == opp.team & is.element(data$position, opp.stack.positions),1,0)
  }

  # add partial copy of matrix for maximum position (flex could be RB, TE, WR)
  # add on_team, column for each team in teams hash
  # also add 1 for solution / not solution
  # salary, fppg
  dummy.vars = cbind(dummy.vars, dummy.vars[,3:5], locked.p, excluded.p, excluded.t, qb.t, opp.t, 1, data$salary, data$fppg)
  cnames = c("dst", "qb", "rb", "te", "wr", "rb", "te", "wr", "locked.p", "excluded.p", "excluded.t", "qb.team", "opp.team", "tot", "salary", "fppg")
  colnames(dummy.vars) = cnames

  # have to transpose the matrix for the solver
  dummy.vars = t(dummy.vars)

  # now adjust the direction variable
  # replicate is used to dynamically adjust
  dirxn.positions = c("==", "==", "<=", "<=", "<=", ">=", ">=", ">=") # 1-8
  dirxn.players = c("==", "==", "==", ">=", ">=") # 9-13 (locked.p, excluded.p, excluded.t, qb_depth, opp_depth)
  dirxn.totals = c("==", "<=", "<=") # 14-16 (num_players, salary, max_points)
  dirxn = c(dirxn.positions, dirxn.players, dirxn.totals)

  rhs.positions = c(1,1,3,2,4,2,1,3)
  rhs.players = c(nlock, 0, 0, qb.depth, opp.depth)
  rhs.totals = c(9, 50000, max.points)
  rhs <- c(rhs.positions, rhs.players, rhs.totals)

  # this is the fppg column in the matrix
  obj <- dummy.vars[nrow(dummy.vars),]

  # solve and view results
  sol <- Rsymphony_solve_LP(obj=obj, mat=dummy.vars, dir=dirxn, rhs=rhs,
                           types="B", max=T)
  data$selected <- sol$solution
  data$position <- factor(data$position, levels = c("QB","RB","WR","TE","DST"))
  lu <- data[data$selected == 1, -which(names(data) %in% c("selected"))] %>% arrange(position)
  lu$qbteam <- unlist(lu %>% filter(position == 'QB') %>% select(team_code))
  lu
}

Lineups <- function(data, sim.id, n=5,
                    locked.players=NULL,
                    excluded.players=NULL,
                    excluded.teams=NULL,
                    qb.team=NULL,
                    qb.depth=0,
                    qb.stack.positions=c('WR', 'TE', 'RB'),
                    opp.team=NULL,
                    opp.depth=0,
                    opp.stack.positions=c('WR', 'TE', 'RB'),
                    max.points=500) {

  # use for multiple lineups
  results = vector("list", length = n)
  for (i in 1:n) {
    l = Lineup(data, locked.players, excluded.players, excluded.teams,
               qb.team, qb.depth, qb.stack.positions,
               opp.team, opp.depth, opp.stack.positions, max.points)
    results[[i]] = l
    max.points = sum(l$fppg) - .1
  }

  lu = do.call(rbind.data.frame, results)
  lu$lineup.id = rep(1:n, each=9)
  lu$sim.id = sim.id
  lu
}

LineupMatrix <- function(data, locked.players=NULL, excluded.players=NULL, excluded.teams=NULL, qb.team=NULL, qb.depth=0, qb.stack.positions=c('WR', 'TE', 'RB'), opp.team=NULL, opp.depth=0, opp.stack.positions=c('WR', 'TE', 'RB')) {
  # NOT SURE WHAT THIS DOES
  dummy.vars = PositionMatrix(data)
  if (is.null(locked.players)) {
    locked.p = ifelse(data$player == 0, 1, 0)
  } else {
    locked.p = ifelse(is.element(data$player, locked.players),1,0)
  }
  if (is.null(excluded.players)) {
    excluded.p = ifelse(data$player == 0, 1, 0)
  } else {
    excluded.p = ifelse(is.element(data$player, excluded.players),1,0)
  }
  if (is.null(excluded.teams)) {
    excluded.t = ifelse(data$team == 0, 1, 0)
  } else {
    excluded.t = ifelse(is.element(data$team, excluded.teams),1,0)
  }
  if (is.null(qb.team)) {
    qb.t = ifelse(data$team == 0, 1, 0)
  } else {
    qb.t = ifelse(data$team == qb.team & is.element(data$position, qb.stack.positions),1,0)
  }
  if (is.null(opp.team)) {
    opp.t = ifelse(data$team == 0, 1, 0)
  } else {
    opp.t = ifelse(data$team == opp.team & is.element(data$position, opp.stack.positions),1,0)
  }
  dummy.vars = cbind(dummy.vars, dummy.vars[,3:5], locked.p, excluded.p, excluded.t, qb.t, opp.t, 1, data$salary, data$fppg)
  cnames = c("dst", "qb", "rb", "te", "wr", "rb", "te", "wr", "locked.p", "excluded.p", "excluded.t", "qb.team", "opp.team", "tot", "salary", "fppg")
  colnames(dummy.vars) = cnames
  t(dummy.vars)
}
