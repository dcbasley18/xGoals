
# Reading Data into Script ------------------------------------------------



shots_10 <- read.csv("shots_2010.csv")
shots_11 <- read.csv("shots_2011.csv")
shots_12 <- read.csv("shots_2012.csv")
shots_13 <- read.csv("shots_2013.csv")
shots_14 <- read.csv("shots_2014.csv")
shots_15 <- read.csv("shots_2015.csv")
shots_16 <- read.csv("shots_2016.csv")
shots_17 <- read.csv("shots_2017.csv")
shots_18 <- read.csv("shots_2018.csv")
shots_19 <- read.csv("shots_2019.csv")
shots_20 <- read.csv("shots_2020.csv")





# Making the Bootstrap ----------------------------------------------------

set.seed(100)


n <- 1000 # number of bootstrap samples


# Turning data in Even Strength only --------------------------------------


shots_10_5v5 <- shots_10 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

shots_11_5v5 <- shots_11 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

shots_12_5v5 <- shots_12 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

shots_13_5v5 <- shots_13 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

shots_14_5v5 <- shots_14 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_15_5v5 <- shots_15 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_16_5v5 <- shots_16 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_17_5v5 <- shots_17 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_18_5v5 <- shots_18 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_19_5v5 <- shots_19 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
shots_20_5v5 <- shots_20 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)



# Performing Bootstrap samples --------------------------------------------




n <- 1000 # number of bootstrap samples


lst_sa10 <- list()
lst_sd10 <- list()
set.seed(10)
for (i in 1:n){
  x <- shots_10_5v5[sample(1:nrow(shots_10_5v5), size=84730, replace = TRUE), ] 
  
  
  init_logit_10_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = x,
                      family = "binomial")
  lst_sa10[i] <- init_logit_10_5v5$coefficients[2]
  lst_sd10[i] <- init_logit_10_5v5$coefficients[3]
  #print(init_logit_10$coefficients[3])
  sa10_5v5_mean <- mean(init_logit_10_5v5$coefficients[2])
  shot10_5v5_d_mean <- mean(init_logit_10_5v5$coefficients[3])
 # sa10_sd <- sd(round(init_logit_10$coefficients[2],5))
  #shot10_d_sd <- sd(round(init_logit_10$coefficients[3], 5))
  
  
  
}
lst_sa10 <- data.frame(lst_sa10)
lst_sd10 <- data.frame(lst_sd10)


sd_sa10 <- sd(lst_sa10)
sd_sd10 <-sd(lst_sd10)



lst_sa11 <- list()
lst_sd11 <- list()

set.seed(11)
for (i in 1:n){
  y = shots_11_5v5[sample(1:nrow(shots_11_5v5), size=83858, replace = TRUE), ]
  init_logit_11_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = y,
                      family = "binomial") 
  
  lst_sa11[i] <- init_logit_11_5v5$coefficients[2]
  lst_sd11[i] <- init_logit_11_5v5$coefficients[3]
  sa11_5v5_mean <- mean(init_logit_11_5v5$coefficients[2])
  shot11_5v5_d_mean <- mean(init_logit_11_5v5$coefficients[3])
  #sa11_sd <- sd(round(init_logit_11$coefficients[2],5))
  #shot11_d_sd <- sd(round(init_logit_11$coefficients[3], 5))
  
}  

lst_sa11 <- data.frame(lst_sa11)
lst_sd11 <- data.frame(lst_sd11)


sd_sa11 <- sd(lst_sa11)
sd_sd11 <- sd(lst_sd11)




lst_sa12 <- list()
lst_sd12 <- list()

set.seed(12)
for (i in 1:n){
  z = shots_12_5v5[sample(1:nrow(shots_12_5v5), size=51545, replace = TRUE), ]
  init_logit_12_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = z,
                      family = "binomial") 
  lst_sa12[i] <- init_logit_12_5v5$coefficients[2]
  lst_sd12[i] <- init_logit_12_5v5$coefficients[3]
  
  sa12_5v5_mean <- mean(init_logit_12_5v5$coefficients[2])
  shot12_5v5_d_mean <- mean(init_logit_12_5v5$coefficients[3])
  #sa12_sd <- sd(round(init_logit_12$coefficients[2],5))
  #shot12_d_sd <- sd(round(init_logit_12$coefficients[3], 5))
  
}  


lst_sa12 <- data.frame(lst_sa12)
lst_sd12 <- data.frame(lst_sd12)


sd_sa12 <- sd(lst_sa12)
sd_sd12 <-sd(lst_sd12)




lst_sa13 <- list()
lst_sd13 <- list()
set.seed(13)

for (i in 1:n){
  x = shots_13_5v5[sample(1:nrow(shots_13_5v5), size=85059, replace = TRUE), ]
  init_logit_13_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = x,
                      family = "binomial") 
  lst_sa13[i] <- init_logit_13_5v5$coefficients[2]
  lst_sd13[i] <- init_logit_13_5v5$coefficients[3]
  sa13_5v5_mean <- mean(init_logit_13_5v5$coefficients[2])
  shot13_5v5_d_mean <- mean(init_logit_13_5v5$coefficients[3])
  #sa13_sd <- sd(round(init_logit_13$coefficients[2],5))
  #shot13_d_sd <- sd(round(init_logit_13$coefficients[3], 5))
  
}  

lst_sa13 <- data.frame(lst_sa13)
lst_sd13 <- data.frame(lst_sd13)


sd_sa13 <- sd(lst_sa13)
sd_sd13 <- sd(lst_sd13)




lst_sa14 <- list()
lst_sd14 <- list()

set.seed(14)
for (i in 1:n){
  y = shots_14_5v5[sample(1:nrow(shots_14_5v5), size=85300, replace = TRUE), ]
  init_logit_14_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = y,
                      family = "binomial") 
  
  lst_sa14[i] <- init_logit_14_5v5$coefficients[2]
  lst_sd14[i] <- init_logit_14_5v5$coefficients[3]
  
  sa14_5v5_mean <- mean(init_logit_14_5v5$coefficients[2])
  shot14_5v5_d_mean <- mean(init_logit_14_5v5$coefficients[3])
 # sa14_sd <- sd(round(init_logit_14$coefficients[2],5))
  #shot14_d_sd <- sd(round(init_logit_14$coefficients[3], 5))
  
}  

lst_sa14 <- data.frame(lst_sa14)
lst_sd14 <- data.frame(lst_sd14)


sd_sa14 <- sd(lst_sa14)
sd_sd14 <- sd(lst_sd14)




lst_sa15 <- list()
lst_sd15 <- list()
set.seed(15)

for (i in 1:n){
  z = shots_15_5v5[sample(1:nrow(shots_15_5v5), size=84935, replace = TRUE), ]
  init_logit_15_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = z,
                      family = "binomial") 
  lst_sa15[i] <- init_logit_15_5v5$coefficients[2]
  lst_sd15[i] <- init_logit_15_5v5$coefficients[3]
  sa15_5v5_mean <- mean(init_logit_15_5v5$coefficients[2])
  shot15_5v5_d_mean <- mean(init_logit_15_5v5$coefficients[3])
  #sa15_sd <- sd(round(init_logit_15$coefficients[2],5))
  #shot15_d_sd <- sd(round(init_logit_15$coefficients[3], 5))
  
}  

lst_sa15 <- data.frame(lst_sa15)
lst_sd15 <- data.frame(lst_sd15)


sd_sa15 <- sd(lst_sa15)
sd_sd15 <- sd(lst_sd15)



lst_sa16 <- list()
lst_sd16 <- list()

set.seed(16)
for (i in 1:n){
  x = shots_16_5v5[sample(1:nrow(shots_16_5v5), size=87189, replace = TRUE), ]
  init_logit_16_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = x,
                      family = "binomial") 
  lst_sa16[i] <- init_logit_16_5v5$coefficients[2]
  lst_sd16[i] <- init_logit_16_5v5$coefficients[3]
  sa16_5v5_mean <- mean(init_logit_16_5v5$coefficients[2])
  shot16_5v5_d_mean <- mean(init_logit_16_5v5$coefficients[3])
  #sa16_sd <- sd(round(init_logit_16$coefficients[2],5))
  #shot16_d_sd <- sd(round(init_logit_16$coefficients[3], 5))
  
}  
lst_sa16 <- data.frame(lst_sa16)
lst_sd16 <- data.frame(lst_sd16)


sd_sa16 <- sd(lst_sa16)
sd_sd16 <- sd(lst_sd16)


lst_sa17 <- list()
lst_sd17 <- list()

set.seed(17)
for (i in 1:n){
  y = shots_17_5v5[sample(1:nrow(shots_17_5v5), size=93528, replace = TRUE), ]
  init_logit_17_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = y,
                      family = "binomial") 
  lst_sa17[i] <- init_logit_17_5v5$coefficients[2]
  lst_sd17[i] <- init_logit_17_5v5$coefficients[3]
  sa17_5v5_mean <- mean(init_logit_17_5v5$coefficients[2])
  shot17_5v5_d_mean <- mean(init_logit_17_5v5$coefficients[3])
  #sa17_sd <- sd(round(init_logit_17$coefficients[2],5))
  #shot17_d_sd <- sd(round(init_logit_17$coefficients[3], 5))
  
}  

lst_sa17 <- data.frame(lst_sa17)
lst_sd17 <- data.frame(lst_sd17)


sd_sa17 <- sd(lst_sa17)
sd_sd17 <- sd(lst_sd17)


lst_sa18 <- list()
lst_sd18 <- list()

set.seed(18)
for (i in 1:n){
  z = shots_18_5v5[sample(1:nrow(shots_18_5v5), size= 93378, replace = TRUE), ]
  init_logit_18_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = z,
                      family = "binomial") 
  lst_sa18[i] <- init_logit_18_5v5$coefficients[2]
  lst_sd18[i] <- init_logit_18_5v5$coefficients[3]
  sa18_5v5_mean <- mean(init_logit_18_5v5$coefficients[2])
  shot18_5v5_d_mean <- mean(init_logit_18_5v5$coefficients[3])
  #sa18_sd <- sd(round(init_logit_18$coefficients[2],5))
  #shot18_d_sd <- sd(round(init_logit_18$coefficients[3], 5))
  
}  

lst_sa18 <- data.frame(lst_sa18)
lst_sd18 <- data.frame(lst_sd18)


sd_sa18 <- sd(lst_sa18)
sd_sd18 <- sd(lst_sd18)

lst_sa19 <- list()
lst_sd19 <- list()

set.seed(19)
for (i in 1:n){
  x = shots_19_5v5[sample(1:nrow(shots_19_5v5), size= 81710, replace = TRUE), ]
  init_logit_19_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                      data = x,
                      family = "binomial") 
  lst_sa19[i] <- init_logit_19_5v5$coefficients[2]
  lst_sd19[i] <- init_logit_19_5v5$coefficients[3]
  sa19_5v5_mean <- mean(init_logit_19_5v5$coefficients[2])
  shot19_5v5_d_mean <- mean(init_logit_19_5v5$coefficients[3])
  #sa19_sd <- sd(round(init_logit_19$coefficients[2],5))
  #shot19_d_sd <- sd(round(init_logit_19$coefficients[3], 5))
  
}  

lst_sa19 <- data.frame(lst_sa19)
lst_sd19 <- data.frame(lst_sd19)


sd_sa19 <- sd(lst_sa19)
sd_sd19 <- sd(lst_sd19)



lst_sa20 <- list()
lst_sd20 <- list()

set.seed(20)
for (i in 1:n){
  x = shots_20_5v5[sample(1:nrow(shots_20_5v5), size= 62311, replace = TRUE), ]
  init_logit_20_5v5 = glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = x,
                          family = "binomial") 
  lst_sa20[i] <- init_logit_20_5v5$coefficients[2]
  lst_sd20[i] <- init_logit_20_5v5$coefficients[3]
  sa20_5v5_mean <- mean(init_logit_20_5v5$coefficients[2])
  shot20_5v5_d_mean <- mean(init_logit_20_5v5$coefficients[3])
  
}  

lst_sa20 <- data.frame(lst_sa20)
lst_sd20 <- data.frame(lst_sd20)


sd_sa20 <- sd(lst_sa20)
sd_sd20 <- sd(lst_sd20)


# Putting bootstrap coefficients into data frame --------------------------



df <- tibble(boot_shot_d = c(shot10_5v5_d_mean, shot11_5v5_d_mean, shot12_5v5_d_mean,
                        shot13_5v5_d_mean, shot14_5v5_d_mean, shot15_5v5_d_mean,
                        shot16_5v5_d_mean, shot17_5v5_d_mean, shot18_5v5_d_mean,
                        shot19_5v5_d_mean, shot20_5v5_d_mean),
             boot_shot_a = c(sa10_5v5_mean, sa11_5v5_mean, sa12_5v5_mean, sa13_5v5_mean,
                        sa14_5v5_mean, sa15_5v5_mean, sa16_5v5_mean, sa17_5v5_mean,
                        sa18_5v5_mean, sa19_5v5_mean, sa20_5v5_mean),
             boot_se_a = c(sd_sa10, sd_sa11, sd_sa12, sd_sa13,
                           sd_sa14, sd_sa15, sd_sa16, sd_sa17,
                           sd_sa18, sd_sa19, sd_sa20),
             boot_se_d = c(sd_sd10, sd_sd11, sd_sd12, sd_sd13,
                           sd_sd14, sd_sd15, sd_sd16, sd_sd17,
                           sd_sd18, sd_sd19, sd_sd20),
             seasons = c(2010,2011,2012,2013,2014,2015, 2016, 2017, 2018, 2019, 2020))




# Plotting Coefficients of sample and nonsample ---------------------------



ggplot(data = combined_df, aes(x = season))+
  geom_point(aes(y= boot_shot_a), size = 2, color = "red")+
  geom_line(aes(y = boot_shot_a), color = "red")+
  geom_errorbar(aes(ymin = boot_shot_a - 2 * boot_se_a,
                    ymax = boot_shot_a + 2 * boot_se_a), 
                color = "red", 
                width = 0.1)+
  geom_point(aes(y = angle_c), size = 2, color = "blue")+
  geom_line(aes(y = angle_c), color = "blue")+
  geom_errorbar(aes(ymin = angle_c - 2 * angle_e,
                    ymax = angle_c + 2 * angle_e),
                color = "blue",
                width = 0.1)+
  labs(title = "xG Coefficient Trends Over Time: Angle, 5-on-5",
       x = "Season",
       y = "Shot Angle Coefficient") +
  scale_x_continuous(breaks = c(2010:2020))+
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))



ggplot(data = combined_df, aes(x = season))+
  geom_point(aes(y= boot_shot_d), size = 2, color = "red")+
  geom_line(aes(y = boot_shot_d), color = "red")+
  geom_errorbar(aes(ymin = boot_shot_d - 2 * boot_se_d,
                    ymax = boot_shot_d + 2 * boot_se_d), 
                color = "red", 
                width = 0.1)+
  geom_point(aes(y = distance_c), size = 2, color = "blue")+
  geom_line(aes(y = distance_c), color = "blue")+
  geom_errorbar(aes(ymin = distance_c - 2 * distance_e,
                    ymax = distance_c + 2 * distance_e),
                color = "blue",
                width = 0.1)+
  labs(title = "xG Coefficient Trends Over Time: Distance, 5-on-5",
       x = "Season",
       y = "Shot Distance Coefficient") +
  scale_x_continuous(breaks = c(2010:2020))+
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))




# combine nonsample and sample coefficients  ------------------------------

combined_df <- cbind(trends_even, df)  


combined_df <- combined_df %>%
  select(-seasons)
  


