
options(digits = 3)

### ESPN gt Theme (@thomas_mock)

gt_theme_espn <- function(data, ...){
  data %>% 
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts()
      )
    )  %>% 
    opt_row_striping() %>% 
    tab_options(
      row.striping.background_color = "#fafafa",
      table_body.hlines.color = "#f6f7f7",
      source_notes.font.size = 12,
      table.font.size = 16,
      table.width = px(700),
      heading.align = "left",
      heading.title.font.size = 24,
      table.border.top.color = "transparent",
      table.border.top.width = px(3),
      data_row.padding = px(7),
      ...
    ) 
}

pw_table <- final_power_rankings %>% 
  mutate(Rank = rank(desc(raw_score))) %>% 
  select(Rank, team, wins, losses, `Win %` = `winning percentage`, `Total Score` = `total score`, `Expected Wins` = `exp_wins`,
         `Points Per Week` = pts_per_week, `Median Wins` = `beat the median.x`,
         `Median Win %` = `median win percentage`, `Power Rank Score` = `raw_score`) %>% 
  mutate(team = paste0(
    "<span style='font-size:16px; color:royalblue;'>",
    team,
    "</span>"),
    #" <span style='font-size:12px; color:grey;'>",
    #word(team, start = -1), "</span>"),
    team = map(team, ~gt::html(as.character(.x)))
  ) %>%
  gt() %>% 
  tab_header(title = md("**V3 Power Rankings - Week 11**")) %>% 
  gt_theme_espn() %>% 
  cols_align("left", columns = vars(team)) %>% 
  cols_align("center", columns = "Rank") %>% 
  tab_source_note(md("**Data:** ESPN Fantasy Football API<br>**Table:** @JoeFlanagan"))

pw_table
  

#gt::gtsave(pw_table, filename = "week11.png") # width = 740, height = 900, units = "px")














