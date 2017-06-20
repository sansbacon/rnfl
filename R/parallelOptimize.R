LineupMatrix <- function(data, locked.players=NULL, excluded.players=NULL, excluded.teams=NULL, qb.team=NULL, qb.depth=0, qb.stack.positions=c('WR', 'TE', 'RB'), opp.team=NULL, opp.depth=0, opp.stack.positions=c('WR', 'TE', 'RB'), intercept='DST') {
  dummy.vars = matrix(as.numeric(model.matrix(fppg ~ position, data=data)), ncol=5)
  dummy.vars[,1] = ifelse(data$position == intercept,1,0)
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

LineupsParallel <- function(data, n=5, locked.players=NULL, excluded.players=NULL, excluded.teams=NULL, qb.team=NULL, qb.depth=0, qb.stack.positions=c('WR', 'TE', 'RB'), opp.team=NULL, opp.depth=0, opp.stack.positions=c('WR', 'TE', 'RB'), max.points=500) {
  dummy.vars = LineupMatrix(data, locked.players, excluded.players, excluded.teams, qb.team,
                            qb.depth, qb.stack.positions, opp.team, opp.depth, opp.stack.positions)
  nlock = ifelse(is.null(locked.players), 0, length(locked.players))
  dirxn.positions = c("==", "==", "<=", "<=", "<=", ">=", ">=", ">=") # 1-8
  dirxn.players = c("==", "==", "==", ">=", ">=") # 9-13 (locked.p, excluded.p, excluded.t, qb_depth, opp_depth)
  dirxn.totals = c("==", "<=", "<=") # 14-16 (num_players, salary, max_points)
  dirxn = c(dirxn.positions, dirxn.players, dirxn.totals)
  rhs.positions = c(1,1,3,2,3,2,1,3)
  rhs.players = c(nlock, 0, 0, qb.depth, opp.depth)
  rhs.totals = c(9, 50000)
  rhs = c(rhs.positions, rhs.players, rhs.totals)
  obj = dummy.vars[nrow(dummy.vars),]
  m = boost.mutex('mp')
  shared<-bigmemory::big.matrix(nrow = 1, ncol = 1, type = 'double')
  shared[1] <- max.points

  results = foreach(i=1:n) %dopar% {
    mm = attach.mutex(describe(m))
    lock(mm)
    mp <- shared[1]
    unlock(mm)
    sol = Rsymphony_solve_LP(obj=obj, mat=dummy.vars, dir=dirxn, rhs=c(rhs, mp), types="B", max=T)
    data$selected = sol$solution
    l = data[data$selected == 1,]
    lock(m)
    shared[1] <- sum(l$fppg) - .1
    unlock(m)
    l
  }

  lu = do.call(rbind.data.frame, results)
  lu$lineup.id = rep(1:n, each=9)
  lu$position = factor(lu$position, levels = c("QB","RB","WR","TE","DST"))
  lu
}

library(Rsymphony)
library(dplyr)
library(bigmemory)
library(synchronicity)
library(doParallel)
registerDoParallel(4)

#LineupsParallel(proj, n=5)
