library(nflfastR)
library(tidyverse)
library(ggimage)

pbp <- nflfastR::load_pbp(2019)
vikingsgames <- pbp %>%
  group_by(posteam) %>%
  filter(defteam == "MIN" & season_type == "REG") %>%
  filter(wp > .20, wp < .80, half_seconds_remaining > 120) %>%
  filter(down == 1 | down == 2) %>%
  summarize(total = n(),
            rpsplit <- sum(play_type == "run")/total)

nfl <- pbp %>%
  group_by(posteam) %>%
  filter(season_type == "REG") %>%
  filter(wp > .20, wp < .80, half_seconds_remaining > 120) %>%
  filter(down == 1 | down == 2) %>%
  summarize(total = n(),
            rpsplittotal <- sum(play_type == "run")/total)

vikingsgames <- vikingsgames[vikingsgames$total>15,]
vikingsgames <- vikingsgames[order(vikingsgames[,3]),]
data <- merge(vikingsgames, nfl, by="posteam")
data <- merge(data, teams_colors_logos, by.x="posteam", by.y = "team_abbr")

ggplot(data, aes(x=reorder(posteam, data[,5]), y=data[,5])) + 
       geom_segment(aes(y = data[,3], x = reorder(posteam, data[,5]), 
                        yend = data[,5],  xend = reorder(posteam, data[,5])), 
                        color = data$team_color2) +
       geom_point(stat='identity', color = data$team_color,
                  size=data$total.x/20)  +
       labs(title="Run-Pass Split vs NFL/Zimmer 2019", 
             subtitle="Downs 1 & 2  -  20% < Win Prob < 80%") + 
       xlab("Team") + 
       ylab("Run-Pass Percentage") + 
       coord_flip()

ggsave("zimmerrpsplit.png", width = 7, height = 4, dpi = 300)
