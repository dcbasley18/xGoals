
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






# Making the Bootstrap ----------------------------------------------------


set.seed(100)


n <- 200 # number of bootstrap samples






for (a in 1:n){
  samp_10 <- shots_10[sample(nrow(shots_10), size=111405, replace = TRUE), ]

}  
  


for (b in 1:n){
  samp_11 <- shots_11[sample(nrow(shots_11), size=108753, replace = TRUE), ]
  
}  


for (c in 1:n){
  samp_12 <- shots_12[sample(nrow(shots_12), size=66087, replace = TRUE), ]
  
}  



for (d in 1:n){
  samp_13 <- shots_13[sample(nrow(shots_13), size=110682, replace = TRUE), ]
  
}  



for (e in 1:n){
  samp_14 <- shots_14[sample(nrow(shots_14), size=109627, replace = TRUE), ]
  
}  



for (f in 1:n){
  samp_15 <- shots_15[sample(nrow(shots_15), size=109461, replace = TRUE), ]
  
}  



for (g in 1:n){
  samp_16 <- shots_16[sample(nrow(shots_16), size=110953, replace = TRUE), ]
  
}  


for (h in 1:n){
  samp_17 <- shots_17[sample(nrow(shots_17), size=119715, replace = TRUE), ]
  
}  


for (i in 1:n){
  samp_18 <- shots_18[sample(nrow(shots_18), size=117622, replace = TRUE), ]
  
}  


for (j in 1:n){
  samp_19 <- shots_19[sample(nrow(shots_19), size=104172, replace = TRUE), ]
  
}  



# Logistic Regression Models for each sample season ------------------------




init_logit_10 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                  data = samp_10,
                  family = "binomial")


init_logit_11 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_11,
                     family = "binomial")


init_logit_12 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_12,
                     family = "binomial")


init_logit_13 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_13,
                     family = "binomial")

init_logit_14 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_14,
                     family = "binomial")

init_logit_15 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_15,
                     family = "binomial")

init_logit_16 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_16,
                     family = "binomial")

init_logit_17 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_17,
                     family = "binomial")

init_logit_18 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_18,
                     family = "binomial")

init_logit_19 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = samp_19,
                     family = "binomial")



# Angle and Distance Coefficients for each season -------------------------



angle_10 <- init_logit_10$coefficients[2]
angle_11 <- init_logit_11$coefficients[2]
angle_12 <- init_logit_12$coefficients[2]
angle_13 <- init_logit_13$coefficients[2]
angle_14 <- init_logit_14$coefficients[2]
angle_15 <- init_logit_15$coefficients[2]
angle_16 <- init_logit_16$coefficients[2]
angle_17 <- init_logit_17$coefficients[2]
angle_18 <- init_logit_18$coefficients[2]
angle_19 <- init_logit_19$coefficients[2]




distance_10 <- init_logit_10$coefficients[3]
distance_11 <- init_logit_11$coefficients[3]
distance_12 <- init_logit_12$coefficients[3]
distance_13 <- init_logit_13$coefficients[3]
distance_14 <- init_logit_14$coefficients[3]
distance_15 <- init_logit_15$coefficients[3]
distance_16 <- init_logit_16$coefficients[3]
distance_17 <- init_logit_17$coefficients[3]
distance_18 <- init_logit_18$coefficients[3]
distance_19 <- init_logit_19$coefficients[3]





# standard error for each year --------------------------------------------

std_err <- coef(summary(init_logit_10))[, "Std. Error"]

samp_10_error <- samp_10 %>%
  mutate(distance_coef = init_logit_10$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_10$coefficients[2],
         angle_err = std_err[2])


std_err_11 <- coef(summary(init_logit_11))[, "Std. Error"]

samp_11_error <- samp_11 %>%
  mutate(distance_coef = init_logit_11$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_11$coefficients[2],
         angle_err = std_err[2])

std_err_12 <- coef(summary(init_logit_12))[, "Std. Error"]

samp_12_error <- samp_12 %>%
  mutate(distance_coef = init_logit_12$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_12$coefficients[2],
         angle_err = std_err[2])

std_err_13 <- coef(summary(init_logit_13))[, "Std. Error"]

samp_13_error <- samp_13 %>%
  mutate(distance_coef = init_logit_13$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_13$coefficients[2],
         angle_err = std_err[2])

std_err_14 <- coef(summary(init_logit_14))[, "Std. Error"]

samp_14_error <- samp_14 %>%
  mutate(distance_coef = init_logit_14$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_14$coefficients[2],
         angle_err = std_err[2])

std_err_15 <- coef(summary(init_logit_15))[, "Std. Error"]

samp_15_error <- samp_15 %>%
  mutate(distance_coef = init_logit_15$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_15$coefficients[2],
         angle_err = std_err[2])


std_err_16 <- coef(summary(init_logit_16))[, "Std. Error"]

samp_16_error <- samp_16 %>%
  mutate(distance_coef = init_logit_16$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_16$coefficients[2],
         angle_err = std_err[2])

std_err_17 <- coef(summary(init_logit_17))[, "Std. Error"]

samp_17_error <- samp_17 %>%
  mutate(distance_coef = init_logit_17$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_17$coefficients[2],
         angle_err = std_err[2])

std_err_18 <- coef(summary(init_logit_18))[, "Std. Error"]

samp_18_error <- samp_18 %>%
  mutate(distance_coef = init_logit_18$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_18$coefficients[2],
         angle_err = std_err[2])

std_err_19 <- coef(summary(init_logit_19))[, "Std. Error"]

samp_19_error <- samp_19 %>%
  mutate(distance_coef = init_logit_19$coefficients[3],
         distance_err = std_err[3],
         angle_coef = init_logit_19$coefficients[2],
         angle_err = std_err[2])

 
summary(init_logit_19)
trends <- rbind(samp_10_error, samp_11_error, samp_12_error, 
                samp_13_error, samp_14_error, samp_15_error,
                samp_16_error, samp_17_error, samp_18_error, samp_19_error)



# Turning Coefficients into Graphable data frame --------------------------



data <- data.frame(angle_c = c(angle_10,angle_11, angle_12, angle_13, angle_14, angle_15, angle_16, angle_17, angle_18 ,angle_19),                    # Create data frame 
                   distance_c = c(distance_10, distance_11, distance_12, distance_13, distance_14, distance_15, distance_16, distance_17, distance_18, distance_19),
                   seasons = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                   std_error = c(std_err[3], std_err_11[3], std_err_12[3],
                                 std_err_13[3],
                                 std_err_14[3], std_err_15[3], std_err_16[3],
                                 std_err_17[3], std_err_18[3], std_err_19[3]))
              


std_data <- data.frame( std_err[3], std_err_11[3], std_err_12[3],
                        std_err_13[3],
                       std_err_14[3], std_err_15[3], std_err_16[3],
                       std_err_17[3], std_err_18[3], std_err_19[3])


# Coefficient Trends over Time --------------------------------------------



ggplot(data,aes(x=seasons,y=distance_c))+
  geom_point()+
  geom_line(aes(group=1))+
  labs(title = "xG Coefficient Trends Over Time: Distance",
       x = "Season",
       y = "Shot Distance Coefficient")+
  scale_x_continuous(breaks = c(2010:2019))+
  geom_errorbar(aes(ymin = distance_c - 2 * std_error,
                    ymax = distance_c + 2 * std_error,
                    color = "red",
                    width = 0.5)) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))


ggplot(data,aes(x=seasons,y=angle_c))+
  geom_point()+
  geom_line(aes(group=1))+
  labs(title = "xG Coefficient Trends Over Time: Angle",
       x = "Season",
       y = "Shot Angle Coefficient")+
  scale_x_continuous(breaks = c(2010:2019)) +
  geom_errorbar(aes(ymin = angle_c - 2 * std_error,
                    ymax = angle_c + 2 * std_error,
                    color = "red",
                    width = 0.5)) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))
















#set.seed(0)
#library(boot)

#define function to calculate fitted regression coefficients
#coef_function <- function(formula, data, indices) {
 # d <- data[indices,] #allows boot to select sample
  #fit <- glm(goal~ shotAngleAdjusted + arenaAdjustedShotDistance, data=d, family = "binomial") #fit regression model
  #return(coef(fit)) #return coefficient estimates of model
#}

#perform bootstrapping with 200 replications
#reps_2010 <- boot(data=shots_10, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2011 <- boot(data=shots_11, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2012 <- boot(data=shots_12, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2013 <- boot(data=shots_13, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2014 <- boot(data=shots_14, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2015 <- boot(data=shots_15, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2016 <- boot(data=shots_16, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2017 <- boot(data=shots_17, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2018 <- boot(data=shots_18, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)
#reps_2019 <- boot(data=shots_19, statistic=coef_function, R=200, formula=goal~ shotAngleAdjusted + arenaAdjustedShotDistance)



#coef_function(fit)




