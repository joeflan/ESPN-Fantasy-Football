# Trying to figure out power rankings
# Formula for Power Rankings
#     Points Scored + 
#     (Points Scored * Winning %) + 
#     (Points Scored * Winning % if played vs the median score of the week)

# Points Scored is easy

points_scored <- week_ranks %>% 
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
  left_join(points_scored, by = 'team') %>% 
  left_join(exp_standings, by = 'team') %>% 
  left_join(teams, by = 'team') %>% 
  left_join(median_wins, by = 'team') %>% 
  select_all() %>% 
  mutate('winning percentage' = (wins / n_weeks),
         'median win percentage' = (as.numeric(`beat the median.x`) / n_weeks)) %>% 
  select(team_id, team, wins, losses, `total score`, exp_wins, pts_per_week,
         `beat the median.x`, `winning percentage`, `median win percentage`)


# Add Formula

final_power_rankings <- power_rankings %>% 
  mutate('raw_score' = 
           `total score` + 
           (`total score` * as.double(`winning percentage`)) +
           (`total score` * as.double(`median win percentage`))) %>% 
  arrange(desc(`raw_score`)) %>% 
  select_all()
  
  
  
