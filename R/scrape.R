# will get projections or rankings from various sources
# start with fantasypros, espn, and fantasyfootballnerd

ScrapeDKSalaries <- function() {
  # gets salaries from rotoguru by way of EWT API
  df = fromJSON('https://dou9h5z7o1.execute-api.us-east-1.amazonaws.com/prod/nfl')
  colnames(df) = c('salary', 'fppg', 'opp', 'floor', 'position', 'ceiling', 'player', 'team')
  df[, c(7, 5, 8, 3, 1, 2, 6, 4)]
}

FFNPositionProjections <- function(pos, week) {
  # you have to set FFNERD_API_KEY in .Renviron to make API calls
  base_url = 'http://www.fantasyfootballnerd.com/service/weekly-projections/json'
  api_key = Sys.getenv("FFNERD_API_KEY")
  url = paste(c(base_url, api_key, pos, week), collapse='/')
  proj = fromJSON(url)
  as.data.frame(proj$Projections)
}

ScrapeFantasyFootballNerd <- function(week) {
  # you have to set FFNERD_API_KEY in .Renviron to make API calls
  positions = c('QB', 'RB', 'WR', 'TE', 'DEF')
  do.call(rbind, lapply(positions, function(x) FFNPositionProjections(x, week)))
}

ScrapeFantasyLabs <- function(model_url='') {

  if (model_url == '') {
    df = read.csv(Sys.getenv("MODEL_URL"), stringsAsFactors = FALSE)
  } else if (grepl(model_url, 'json')) {
    df = fromJSON(model_url)
  } else if (grepl(model_url, 'csv')) {
    df = read.csv(model_url, stringsAsFactors = FALSE)
  } else {
    df = data.frame
  }
  df
}

ScrapeFantasyProsWeekly <- function() {
  cols = c('rank', 'player.team', 'opp', 'high', 'low', 'avg', 'stdev', 'position')
  qb = as.data.frame(read_html('https://www.fantasypros.com/nfl/rankings/qb.php') %>% html_nodes("#data > tbody > tr") %>% html_text() %>% lapply(function(x) unlist(strsplit(x, '\n'))) %>% do.call(rbind, .) %>% .[,1:7], stringsAsFactors = FALSE)
  qb$position = 'QB'
  rb = as.data.frame(read_html('https://www.fantasypros.com/nfl/rankings/ppr-rb.php') %>% html_nodes("#data > tbody > tr") %>% html_text() %>% lapply(function(x) unlist(strsplit(x, '\n'))) %>% do.call(rbind, .) %>% .[,1:7], stringsAsFactors = FALSE)
  rb$position = 'RB'
  wr = as.data.frame(read_html('https://www.fantasypros.com/nfl/rankings/ppr-wr.php') %>% html_nodes("#data > tbody > tr") %>% html_text() %>% lapply(function(x) unlist(strsplit(x, '\n'))) %>% do.call(rbind, .) %>% .[,1:7], stringsAsFactors = FALSE)
  wr$position = 'WR'
  te = as.data.frame(read_html('https://www.fantasypros.com/nfl/rankings/ppr-te.php') %>% html_nodes("#data > tbody > tr") %>% html_text() %>% lapply(function(x) unlist(strsplit(x, '\n'))) %>% do.call(rbind, .) %>% .[,1:7], stringsAsFactors = FALSE)
  te$position = 'TE'
  dst = as.data.frame(read_html('https://www.fantasypros.com/nfl/rankings/dst.php') %>% html_nodes("#data > tbody > tr") %>% html_text() %>% lapply(function(x) unlist(strsplit(x, '\n'))) %>% do.call(rbind, .) %>% .[,1:7], stringsAsFactors = FALSE)
  dst$position = 'DST'
  ranks = as.data.frame(rbind(qb, rb, wr, te, dst))
  colnames(ranks) = c('rank', 'player.team', 'opp', 'high', 'low', 'avg', 'stdev', 'position')
  ranks
}

