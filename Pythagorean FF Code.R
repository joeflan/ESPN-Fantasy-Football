# Load Libraries 

library(jsonlite)
library(tidyverse)
library(gt)
library(glue)

# ESPN League Information

# League must be viewable to the public in league settings
league_id <- 226171 ### Copy this from your league's url
league_year <- 2020

# League URL
league_url <- paste0("http://fantasy.espn.com/apis/v3/games/ffl/seasons/",league_year,"/segments/0/leagues/",league_id,"?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav")

# Create Teams data frame

y <- fromJSON(league_url)

teams <- jsonlite::flatten(y$teams)

teams <- 
  select(teams, 
         'team_id' = id,
         'division_id' = divisionId,
         location,
         nickname,
         'abbreviation' = abbrev,
         'wins' = record.overall.wins,
         'losses' = record.overall.losses,
         'ties' = record.overall.ties) %>% 
  mutate('team' = paste(location, nickname, sep = '\n'))

# Create Scores Data Frame

scores <- 
  tibble('week' = y$schedule$matchupPeriodId,
         'home_id' = y$schedule$home$teamId,
         'away_id' = y$schedule$away$teamId,
         'home_score' = apply(y$schedule$home$pointsByScoringPeriod, 1, sum, na.rm = T),
         'away_score' = apply(y$schedule$away$pointsByScoringPeriod, 1, sum, na.rm = T))

# Join in team names

scores <- 
  scores %>% 
  inner_join(select(teams, team_id, team), by = c('home_id' = 'team_id')) %>% 
  rename('home_team' = 'team') %>% 
  inner_join(select(teams, team_id, team), by = c('away_id' = 'team_id')) %>% 
  rename('away_team' = 'team') %>% 
  mutate('result' = 
           case_when(home_score > away_score ~ home_team,
                     away_score > home_score ~ away_team,
                     home_score == away_score ~ 'tie')) %>% 
  filter(home_score != 0)
  
# Create Current Week Standings

n_weeks <- (nrow(scores) * 2 / nrow(teams))
wins <- 
  map_dbl(teams$team, ~{sum(scores$result == .x) + 
      0.5 * sum(scores$result == 'tie' & (scores$home_team == .x | scores$away_team == .x))})
losses <-  n_weeks - wins
standings <- tibble('team' = teams$team,
                    'wins' = wins,
                    'losses' = losses)

# Figure out each team's rank in given week

week_ranks <- 
  select(scores, week, 'team' = home_team, 'score' = home_score) %>% 
  bind_rows(select(scores, week, 'team' = away_team, 'score' = away_score)) %>% 
  group_by(week) %>% 
  mutate('rank' = rank(desc(score), ties.method = 'average')) %>% 
  arrange(week)

# Expected Records

exp_standings <- 
  group_by(week_ranks, team) %>% 
  summarise('exp_wins' = sum((nrow(teams) - rank)/(nrow(teams) - 1)),
            'pts_per_week' = mean(score)) %>% 
  ungroup()


# Formula for simple Power Rankings
#     Points Scored + 
#     (Points Scored * Winning %) + 
#     (Points Scored * Winning % if played vs the median score of the week)

# Points Scored

points_summary <- week_ranks %>% 
  group_by(team) %>% 
  summarise('total score' = sum(score)) %>% 
  select(team, 'total score')

# Winning Percentage

winning_percent <- (wins / n_weeks)

# Winning % vs median score of the week

median_score <- week_ranks %>% 
  group_by(week) %>% 
  summarise('median score' = median(score)) %>% 
  select(week, 'median score')

# Did team win the week (Did they beat the median score)

week_ranks_with_median <- week_ranks %>% 
  inner_join(median_score, by = 'week') %>% 
  mutate('median win' = as.double((score > `median score`)))

#summing median wins

median_wins <- week_ranks_with_median %>%
  group_by(team) %>% 
  summarise(`beat the median` = sum(`median win`)) %>% 
  arrange(desc(`beat the median`))



# Create Power Rankings Table / Gather Data

power_rankings <- median_wins %>% 
  #left_join(points_scored, by = 'team') %>% 
  left_join(points_scored) %>%
  left_join(exp_standings) %>% 
  left_join(teams) %>% 
  left_join(median_wins) %>% 
  select_all() %>% 
  mutate('winning percentage' = (wins / n_weeks),
         'median win percentage' = (as.numeric(`beat the median`) / n_weeks)) %>% 
  select(team_id, team, wins, losses, `total score`, exp_wins, pts_per_week,
         `beat the median`, `winning percentage`, `median win percentage`)


# Add Formula

final_power_rankings <- power_rankings %>% 
  mutate('raw_score' = 
           `total score` + 
           (`total score` * as.double(`winning percentage`)) +
           (`total score` * as.double(`median win percentage`))) %>% 
  arrange(desc(`raw_score`)) %>% 
  select_all()






