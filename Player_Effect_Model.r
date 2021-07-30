data <- read.csv("shot_ages.csv")


library(lme4)

player_effects_ages <- glmer(goal ~ arenaAdjustedShotDistance + shotAngleAdjusted + 
                               shotGeneratedRebound + shotRush +
                               (1|shooterName) + (1|goalieName) +
                               ns(shooterAge, df = 6) + ns(goalieAge, df = 6), 
                             data = shots_ages, family = "binomial")

# Creating data frame to visualize spline (shooter age) ---------------------
df_shooter = data.frame(shooterAge = seq(from = 18, to = 42, by=0.1), 
                        goalieAge = 30,
                        arenaAdjustedShotDistance = 30,
                        shotAngleAdjusted = 30,
                        shotGeneratedRebound = 1,
                        shotRush = 1,
                        shooterName = "Shooter A",
                        goalieName = "Goalie A")

#get predicted values from model
df_shooter$pred = predict(player_effects_ages, newdata = df_shooter, type = "response", allow.new.levels = TRUE)

#plot predicted values by age
df_shooter %>%
  ggplot(aes(x= shooterAge, y= pred))+
  geom_line()+
  labs(x = "Shooter Age",
       y = "Goal Probability") +
  theme_bw()


# Creating data frame to visualize spline (goalie age) -----------------------
df_goalie = data.frame(shooterAge = 30, 
                       goalieAge = seq(from = 18, to = 42, by=0.1),
                       arenaAdjustedShotDistance = 30,
                       shotAngleAdjusted = 30,
                       shotGeneratedRebound = 0,
                       shotRush = 0,
                       shooterName = "Shooter A",
                       goalieName = "Goalie A")

#get predicted values from model
df_goalie$pred = predict(player_effects_ages, newdata = df_goalie, type = "response", allow.new.levels = TRUE)

#plot predicted values by age
df_goalie %>%
  ggplot(aes(x= goalieAge, y= 1-pred))+
  geom_line()+
  labs(x = "Goalie Age",
       y = "Save Percentage") +
  theme_bw()




player_effects <- glmer(goal ~ arenaAdjustedShotDistance + shotAngleAdjusted + 
                               shotGeneratedRebound + shotRush +
                               (1|shooterName) + (1|goalieNameForShot), 
                             data = shots_5v5, family = "binomial")

