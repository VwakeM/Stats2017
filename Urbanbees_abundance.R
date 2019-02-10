source("pairsPannelFunctions.r")

#Read data
bee_data <- read.table("urbanBees_preprocessed.txt", header= TRUE)

pairs(bee_data[,1:13], lower.panel = panel.smooth, diag.panel = panel.hist, upper.panel = panel.cor)

# Set NAs to 0.
bee_data$or_chambers <-  ifelse(is.na(bee_data$or_chambers), 0, bee_data$or_chambers)

plot(x = bee_data$share_int, y = bee_data$or_chambers)
hist(x = bee_data$share_int)

# glm trials
g_bee <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + floor + flowers_100m + tubetotal, data = bee_data, family=poisson )

summary(g_bee)
exp(coef(g_bee))

drop1(g_bee)

g_bee_2  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + floor + flowers_100m, data = bee_data, family=poisson )

drop1(g_bee_2)

g_bee_3  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + floor + flowers_100m + dist_forage0, data = bee_data, family=poisson )

drop1(g_bee_3)

g_bee_4  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + floor + flowers_100m + share_n2_500, data = bee_data, family=poisson )

drop1(g_bee_4)

g_bee_5  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + floor + flowers_100m + NumP_250, data = bee_data, family=poisson )

drop1(g_bee_5)

g_bee_6  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + flowers_100m, data = bee_data, family=poisson )

drop1(g_bee_6)

model.matrix(g_bee_6)

library(glmulti)

res <- glmulti(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + flowers_100m, crit="aicc", confsetsize=128, data = bee_data, level = 2)

glm_pos_multi <- glm(or_chambers~1+hang_location.t+juliandays.t+share_int+hang_location_sp.t:share_int+shade.t:juliandays.t, data = bee_data, family=poisson)
summary(glm_pos_multi)

drop1(glm_pos_multi)

1 - glm_pos_multi$deviance/glm_pos_multi$null.deviance

# Trial for quasi-possion
#g_bee_qpos  <- glm(or_chambers ~ juliandays.t + share_int + hang_location.t + hang_location_sp.t + shade.t + flowers_100m, data = bee_data, family= quasipoisson )

drop1(g_bee_qpos)
summary(g_bee_qpos)

newD <- expand.grid(juliandays.t= seq(2, 52, length.out = 100), hang_location.t= levels(bee_data$hang_location.t),  share_int= c(20, 40.36, 60), hang_location_sp.t= levels(bee_data$hang_location_sp.t), shade.t = levels(bee_data$shade.t) )

View(newD)

pred_g <- as.data.frame(predict.glm(glm_pos_multi, newdata =newD , se.fit =TRUE ))

pred_g <- cbind(pred_g, newD)
?predict.glm
View(pred_g)

#reduced_oc <- bee_data$or_chambers[1:210]


plot(or_chambers ~ juliandays.t, data= bee_data, xlab ="Days before the nest was removed", ylab="No of chambers occupied by O.rufa")

lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_sunlit", col= "red")

lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassE" & shade.t ==  "Fully_sunlit", col= "blue")

lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassC" & shade.t ==  "Fully_sunlit", col= "green")

plot(or_chambers ~ juliandays.t, data= bee_data, xlab ="Days before the nest was removed", ylab="No of chambers occupied by O.rufa")


lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassC" & shade.t ==  "Fully_shaded", col= "blue")
lines(exp(fit + 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)
lines(exp(fit - 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)

lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassE" & shade.t ==  "Fully_sunlit", col= "red")
lines(exp(fit + 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)
lines(exp(fit - 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)


lines(exp(fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Garden_or_allotment" & share_int == 40.36 & hang_location_sp.t == "ClassC" & shade.t ==  "Partly_shaded", col= "green")
lines(exp(fit + 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)
lines(exp(fit - 1.96* se.fit) ~ juliandays.t, data=pred_g, subset= hang_location.t== "Backyard_or_park" & share_int == 40.36 & hang_location_sp.t == "ClassA" & shade.t ==  "Fully_shaded", col= "blue", lty=3)

boxplot(fit ~ shade.t, data = pred_g)

require(faraway)
cooks_bees <- cooks.distance(glm_pos_multi)
halfnorm(cooks_bees)

#?halfnorm

plot(residuals(glm_pos_multi) ~ fitted(glm_pos_multi), bg = "blue", ylab = "Residuals" , xlab = "Fitted", las = 1) 

abline(h = 0)

fitted(glm_pos_multi)
