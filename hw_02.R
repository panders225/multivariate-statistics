

# Philip Anderson
# HW02 program

# import third-party modules
library("MASS")
library("tidyverse")

# Question 3

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
qqnorm(used_cars$age)
qqline(used_cars$age)

qqnorm(used_cars$age ** car_bc_lambda)
qqline(used_cars$age ** car_bc_lambda)

par(mfrow=c(1,1))
plot(density(used_cars$age))
lines(density(used_cars$age ** car_bc_lambda)
      , lty=2)

shapiro.test(used_cars$age)
shapiro.test(used_cars$age ** car_bc_lambda)


