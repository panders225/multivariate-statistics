

# Philip Anderson
# HW02 program

######################
# import third-party modules
######################

library("biotools")
library("car")
library("DescTools")
library("MASS")
library("MVN")
library("tidyverse")

##################
# Question 1
##################

(X <- matrix(c(3, 4, 5, 4, 6, 4, 7, 7), nrow=4))
(n <- nrow(X))

# x_bar MLE
(x_bar <- colMeans(X))
# sigma_hat MLE
((n - 1) / n) # component 1
var(X) # component 2
((n - 1) / n) * var(X) # final

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
(car_bc_lambda <- Cox2[1,1])

used_cars$age ** car_bc_lambda

# let's get some visuals
par(mfrow=c(1,1))
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
par(mfrow=c(1,1))
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

(multi_mat <- cbind(used_cars$age ** mv_lambda_age, used_cars$price ** mv_lambda_price))
MVN::mardiaTest(data=multi_mat, qqplot=T)

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

(p <- ncol(sweat))
(n <- nrow(sweat))

# the ellipse will be centered at x_bar, which are the column means
x_bar
alph <- 0.05 # confidence level

# the axes for the ellipse are given by
# axis one
(c <- sqrt(((p * (n - 1)) / (n * (n - p))) * qf(1 - alph, p, n-p)))
(c / sqrt(lambda[1])) * e_vec[,1]

# axis two
(c / sqrt(lambda[2])) * e_vec[,2]

# axis three
(c / sqrt(lambda[3])) * e_vec[,3]



((p * (n - 1)) / (n * (n - p))) * qf(1-alph, p, n-p)
sqrt(n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0))

# all ellipsoidal points should be within 95% of fixed distance

check_inclusion <- function(n, p, x_bar, S, mu_0, alph=0.05){
n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0) <= ((p * (n - 1)) / (n * (n - p))) * qf(1-alph, p, n-p)
}

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

t2_cis <- rbind(
cbind(ci_sim_1[1], x_bar[1], ci_sim_1[2])
, cbind(ci_sim_2[1], x_bar[2], ci_sim_2[2])
, cbind(ci_sim_3[1], x_bar[3], ci_sim_3[2])
) %>% data.frame() 

names(t2_cis) <- c('left', 'mean', 'right')
t2_cis


# D -  Compute the 95% Bonferroni simultaneous CIs
x_bar
ci_bon_1 <- x_bar[1] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[1, 1] / n)
ci_bon_2 <- x_bar[2] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[2, 2] / n)
ci_bon_3 <- x_bar[3] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[3, 3] / n)

bon_cis <- rbind(
cbind(ci_bon_1[1], x_bar[1], ci_bon_1[2])
, cbind(ci_bon_2[1], x_bar[2], ci_bon_2[2])
, cbind(ci_bon_3[1], x_bar[3], ci_bon_3[2])
) %>% data.frame()

names(bon_cis) <- c('left', 'mean', 'right')
bon_cis

# E - carry out a Hotelling's T2 of the null hypothesis mu0 <- c(4.0, 45.0, 10.0)

mu_0 <- c(4.0, 45.0, 10.0)

(T2 <- n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0))
(p_value_par <- 1 - pf((n - p) * T2 / ((n - 1) * p), p, n - p))

# critical value given by:
(((n - 1) * p) / (n -p)) * qf(1 - 0.05, n, n - p)

# alt check
DescTools::HotellingsT2Test(x=sweat, mu=c(4.0, 45.0, 10.0))

# F - is this consistent with what was seen in part B?
# use our function from above
check_inclusion(n = nrow(sweat), p = ncol(sweat), x_bar = colMeans(sweat)
                , S = var(sweat), mu_0 = c(4.0, 45.0, 10.0))

check_inclusion
(n <- nrow(sweat))
(p <- ncol(sweat))
(x_bar <- colMeans(sweat))
(S <- var(sweat))
(mu_0 <- c(4.0, 45.0, 10.0))

n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0) <= ((p * (n - 1)) / (n - p)) * qf(1-alph, p, n-p)

n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0)
((p * (n - 1)) / (n - p)) * qf(1-alph, p, n-p)

# G - test the same null hypothesis as in part E, but using the bootstraped test statistic

# first, calculate the test statistic on our observed data
S <- var(sweat)
X_bar <- colMeans(sweat)
mu_0 <- c(4.0, 45.0, 10.0)
X_0 <- sweat - rep(1, n) %*% t(X_bar) + rep(1, n) %*% t(mu_0)

colMeans(X_0)

S_0 <- var(X_0)

base_ts <- (det(S) / det(S_0)) ** (nrow(sweat) / 2)

# now, conduct the bootstrapping
set.seed(101)
B <- 500
out_out <- numeric(B)
for (i in 1:10) {
 hold_samp <- X_0[sample(1:20, replace=T), ]
 init_S <- var(sweat)
 hold_S <- var(hold_samp)
 LR <- (det(init_S) / det(hold_S)) ** (nrow(sweat) / 2)
 out_out[i] <- LR
}

mean(out_out > drop(base_ts))

##################
# Question 5
##################

# import the peanut data

peanut <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/hw_02/peanut.csv")
names(peanut) <- tolower(names(peanut))
str(peanut)

# assess the landscape
table(peanut$location); table(peanut$variety); table(peanut$location, peanut$variety)

# due to the small sample size, we will need to evaluate the multivariate normality 
# of the response variables
# using the mardia test

MVN::mardiaTest(data=cbind(peanut$x_1, peanut$x_2, peanut$x_3))

# we will now need to assess the homogeneity of the variances 
biotools::boxM(data=peanut[, -c(1,2,6)], grouping=factor(peanut[, c(2)]))
biotools::boxM(data=peanut[, -c(1,2,6)], grouping=factor(peanut[, c(2)]))

# covariance matrices appear good across both groupings of factors

# set up the model
# response matrix
(Y <- cbind(peanut$x_1, peanut$x_2, peanut$x_3))

fit1 <- manova(Y ~ peanut$location*peanut$variety)
summary(fit1, test="Wilks")

fit2 <- manova(Y ~ peanut$variety*peanut$location)
summary(fit2, test="Wilks")

# order seems to have an impact on the obtained results

fit3 <- manova(Y ~ peanut$location + peanut$variety)
summary(fit3, test="Wilks")

# part B - construct the two-way Sums of Squares MANOVA table
test <- summary(fit1, test="Wilks")

test 

str(test)

test$SS

# we are going to now extract Wilk's lambda statistic for each main effect 
# and interaction, and calculate the p-value

# preliminaries
g <- length(unique(peanut$location))
b <- length(unique(peanut$variety))
n <- 2 # replications per factor interaction
p <- 3 # output parameters

diag(test$SS$`peanut$location`)

aov(fit1)

# factor 1

# compute wilks lambda
(fact_one <- det(test$SS$Residuals) / det(test$SS$`peanut$location` + test$SS$Residuals))

const <- (((g * b * (n - 1)) - p + 1) / 2) / ((abs((g - 1) - p) + 1) / 2)
(test_stat <- const * ((1 - fact_one) / fact_one))

(nu1 <- abs((g - 1) - p) + 1)
(nu2 <- (g * b * (n - 1)) - p + 1 )

1 - pf(test_stat, df1=nu1, df2=nu2)


# factor 2


(fact_two <- det(test$SS$Residuals) / det(test$SS$`peanut$variety` + test$SS$Residuals))
const <- (((g * b * (n - 1)) - p + 1) / 2) / ((abs((b - 1) - p) + 1) / 2)
(test_stat <- const * ((1 - fact_two) / fact_two))

(nu1 <- abs((b - 1) - p) + 1)
(nu2 <- (g * b * (n - 1)) - p + 1 )

1 - pf(test_stat, df1=nu1, df2=nu2)


# interaction term

(int <- det(test$SS$Residuals) / det(test$SS$`peanut$location:peanut$variety`+ test$SS$Residuals))
const <- (((g * b * (n - 1)) - p + 1) / 2) / ((abs(((g - 1) * (b - 1)) - p) + 1) / 2)
(test_stat <- const * ((1 - int) / int))

(nu1 <- abs(((g - 1) * (b - 1)) - p) + 1)
(nu2 <- (g * b * (n - 1)) - p + 1 )

1 - pf(test_stat, df1=nu1, df2=nu2)

