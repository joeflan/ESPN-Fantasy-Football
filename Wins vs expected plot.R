### Make Plot

df <- inner_join(standings, exp_standings, by = 'team')
theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 16, hjust = 0.5),
                  axis.title = element_text(size = 14),
                  plot.subtitle = element_text(size = 12, hjust = 0.5),
                  legend.position = "bottom"))

ggplot(df, aes(x = exp_wins, y = wins)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2, col = 'black') +
  geom_label(aes(label = team, fill = pts_per_week), alpha = 0.5, size = 2) +
  scale_fill_viridis_c()  +
  labs(x = 'Expected Wins',
       y = 'Actual Wins',
       fill = 'Points/Week',
       title = 'Pythagorean Fantasy Football Records',
       subtitle = glue('ESPN LeagueID: {league_id}'),
       caption = 'Code: @recspecs730 | Data: ESPN Fantasy Football API') +
  scale_x_continuous(limits = c(0, n_weeks), breaks = 0:n_weeks) +
  scale_y_continuous(limits = c(0, n_weeks), breaks = 0:n_weeks) 

ggsave('pythagorean_fantasy_football.png', height = 9/1.5, width = 16/1.5)  