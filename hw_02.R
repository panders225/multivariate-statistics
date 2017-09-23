

# Philip Anderson
# HW02 program

######################
# import third-party modules
######################

library("car")
library("MASS")
library("MVN")
library("tidyverse")

######################
# Question 3
######################

used_cars <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/hw_02/used_cars.csv")
colnames(used_cars) <- tolower(colnames(used_cars)) 

dim(used_cars)
summary(used_cars)
head(used_cars)


# conduct the bc transformation on the age variable
bc_age <- MASS::boxcox(age ~ 1
                       , data=used_cars
                       , lambda=seq(-6, 6, by=0.1)
                       )
# see what we've done
glimpse(bc_age)

Cox <- data.frame(bc_age$x, bc_age$y)
Cox2 <- dplyr::arrange(Cox, desc(bc_age.y))
# extract the lambda
car_bc_lambda <- Cox2[1,1]

used_cars$age ** car_bc_lambda

# let's get some visuals
par(mfrow=c(2,1))
qqnorm(used_cars$age, main="Original Age Variable")
qqline(used_cars$age)

qqnorm(used_cars$age ** car_bc_lambda, main="Transformed Age Variable")
qqline(used_cars$age ** car_bc_lambda)
par(mfrow=c(1,1)) # reset

# both were normal the whole time anyway
shapiro.test(used_cars$age)
shapiro.test(used_cars$age ** car_bc_lambda)


# conduct the bc transformation for the price variable

bc_price <- MASS::boxcox(price ~ 1
                         , data=used_cars
                         , lambda=seq(-6, 6, by=0.1)
                         )

Cox <- data.frame(bc_x=bc_price$x, bc_y=bc_price$y)
(price_bc_lambda <- Cox[with(Cox, order(-Cox$bc_y)) , ][1,1])


# graphics
par(mfrow=c(2,1))
qqnorm(used_cars$price, main="Original Price Variable")
qqline(used_cars$price)

qqnorm(used_cars$price ** price_bc_lambda, main="Transformed Price Variable")
qqline(used_cars$price ** price_bc_lambda)

shapiro.test(used_cars$price)
shapiro.test(used_cars$price ** price_bc_lambda)

# multivariate BC transformation for both variables
mv_bc <- car::powerTransform(
                    cbind(used_cars$age, used_cars$price)
                    , family="bcPower"
                    )


glimpse(mv_bc)
(mv_lambda_age <- mv_bc$lambda[1])
(mv_lambda_price <- mv_bc$lambda[2])

car_bc_lambda
price_bc_lambda

shapiro.test(used_cars$age ** mv_lambda_age)
shapiro.test(used_cars$price ** mv_lambda_price)

# multivariate normality transformation is inferior to univariate transformations
# in assessing univariate normality



##################
# Question 4
##################

sweat <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/hw_02/sweat.csv")
names(sweat) <- tolower(names(sweat)) # drop it low
str(sweat)

# part a - construct univariate QQ plots for each of the three variables
shapiro.test(sweat$sweat)
shapiro.test(sweat$sodium)
shapiro.test(sweat$potassium)
# everything appears staunchly normal heading into this next piece 

par(mfrow=c(1,1))
qqnorm(sweat$sweat, main="sweat$sweat Normal Q-Q Plot")
qqline(sweat$sweat)

qqnorm(sweat$sodium, main="sweat$sodium Normal Q-Q Plot")
qqline(sweat$sodium)

qqnorm(sweat$potassium, main="sweat$potassium Normal Q-Q Plot")
qqline(sweat$potassium)

pairs(sweat)

MVN::mardiaTest(data=sweat, qqplot=T)
# multivariate assumption seems reasonable

# B - compute the 95% confidence ellipsoid for mu
# n (xbar - mu)' S**-1 (xbar - mu) < c2

x_bar <- colMeans(sweat)
S <- var(sweat)

(ee <- eigen(S))
(lambda <- ee$values)
(e_vec <- ee$vectors)
e_vec[,1]

# the ellipse will be centered at x_bar, which are the column means
x_bar


# the axes for the ellipse are given by
c <- sqrt(((p * (n - 1)) / (n * (n - p))) * qf(1-alpha, p, n-p))
c / sqrt(lambda[1]) * e_vec[,1]

((p * (n - 1)) / (n * (n - p))) * qf(1-alpha, p, n-p)
sqrt(n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0))


# C - compute 95% simultanous T2 confidence intervals for the mean components

(x_bar <- colMeans(sweat))
(n <- nrow(sweat))
(S <- var(sweat))
(p <- ncol(sweat))
(alpha <- 0.05)
(c2 <- (n - 1) * p * qf(1 - alpha, p, n - p) / (n - p))

# print out the T2 CI's
x_bar
ci_sim_1 <- x_bar[1] + c(-1, 1) * sqrt(c2 * S[1, 1] / n)
ci_sim_2 <- x_bar[2] + c(-1, 1) * sqrt(c2 * S[2, 2] / n)
ci_sim_3 <- x_bar[3] + c(-1, 1) * sqrt(c2 * S[3, 3] / n)

rbind(
cbind(ci_sim_1[1], x_bar[1], ci_sim_1[2])
, cbind(ci_sim_2[1], x_bar[2], ci_sim_2[2])
, cbind(ci_sim_3[1], x_bar[3], ci_sim_3[2])
) 

# D -  Compute the 95% Bonferroni simultaneous CIs
x_bar
ci_bon_1 <- x_bar[1] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[1, 1] / n)
ci_bon_2 <- x_bar[2] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[2, 2] / n)
ci_bon_3 <- x_bar[3] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[3, 3] / n)

rbind(
cbind(ci_bon_1[1], x_bar[1], ci_bon_1[2])
, cbind(ci_bon_2[1], x_bar[2], ci_bon_2[2])
, cbind(ci_bon_3[1], x_bar[3], ci_bon_3[2])
) 

# E - carry out a Hotelling's T2 of the null hypothesis mu0 <- c(4.0, 45.0, 10.0)

mu_0 <- c(4.0, 45.0, 10.0)

(T2 <- n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0))
(p_value_par <- 1 - pf((n - p) * T2 / ((n - 1) * p), p, n - p))


# F - is this consistent with what was seen in part B?

