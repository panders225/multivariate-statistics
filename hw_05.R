
# homework 5

##################
# import third-party modules
##################

library("tidyverse")
library("glmnet")

##################
# Import the data and fix it up
##################


happy <- read.csv("/Users/Philip/Schools/TAMU/STAT_636/homework/happiness.csv")
names(happy) <- tolower(names(happy))

happy <- happy %>%
  dplyr::select("dystopia.residual", "economy..gdp.per.capita."
                , "family", "health..life.expectancy."
                , "freedom", "trust..government.corruption."
                , "generosity")

# prep our data to eventually be used in glmnet
Y <- happy[, 1] %>% as.matrix()
colnames(Y) <- c("Y")
X <- happy[, 2:7] %>% as.matrix()
colnames(X) <- c("x1", "x2", "x3", "x4", "x5", "x6")

# also for lm, everything will need to be together in a df
lin_dat <- cbind(Y, X) %>% data.frame()

##################
# Question 1
##################

##################
#  function for leave-one-out cv (LOOCV)
##################

loocv_lm <- function(x) {
  # input is the data frame for use in linear regression
  # create a holding vector for our squared error
  sq_err <- numeric(nrow(x))
  # create a holding matrix for our model coefficients
  mod_coef <- matrix(0, nrow=nrow(x), ncol=ncol(x))

  for (i in 1:nrow(x)){

      # test data - every record minus 1
      loo_dat <- x[-i, ]
      # validation record
      valid_dat <- x[i, ]
      # create the linear model
      lin_mod <- lm(Y ~ .
                    , data=loo_dat
                    )
      # make a prediction
      loo_pred <- predict(lin_mod, valid_dat)
      # record our squared error
      sq_err[i] <- (valid_dat[, 1] - loo_pred)**2
      # save the model coefficients
      mod_coef[i, ] <- t(matrix(coef(lin_mod)))
      }

  MSE <- sum(sq_err) / nrow(x)

  return(list(sqr_err=sq_err
              , MSE=MSE
              , mod_coef=mod_coef
              )
         )
}

loo_results <- loocv_lm(lin_dat)


# take a look at the density of the MSE values
plot(density(loo_results$sqr_err))

# print the MSE value we arrived at with the LOOCV LM
loo_results$MSE


##################
# boostrapping function
##################

loo_bootstrap <- function(x, B=10000){
  # input x is a list of squared errors from our LOO exercise
  bs_holding <- numeric(B)

    for (i in 1:B){
      set.seed(1738+i)
      bs_inter <- sample(
                  x=x
                  , size=length(x)
                  , replace=T
                  )
      bs_holding[i] <- mean(bs_inter)
     }
  return(bs_holding)
}

bs_output <- loo_bootstrap(x=loo_results$sqr_err)
plot(density(bs_output))
sd(bs_output)

# make sure this looks good
mean(bs_output)
loo_results$MSE
abline(v=loo_results$MSE)


# above approach was incorrect.  Fixing it here

loo_bootsrap2 <- function(X, B=1000) {

  bs_mse <- numeric(B)

  for (i in 1:B){
    set.seed(1738+i)
    hold_dat <- X[sample(x=1:nrow(X), replace=T) , ]
    hold_mod <- lm(Y ~ .
                   , data=hold_dat)
    hold_predict <- predict(hold_mod, hold_dat)

    bs_mse[i] <- mean((hold_dat$Y - hold_predict)**2)
  }

 return(bs_mse)

}

round_two <- loo_bootsrap2(X=lin_dat)
sd(round_two)

##################
# Question 2A - determine the optimal value of lambda
##################

cv_output <- glmnet::cv.glmnet(
          x=X
          , y=Y
          , family="gaussian"
          , nfolds=10
          , alpha=1
        )


# should keep the optimal lambda based on the type.measure input
# (will choose max or min based on context)
(cv_lambda <- cv_output$lambda.min)

# see how the mean of the MSE compares to what I got
mean(cv_output$cvm)
loo_results$MSE


##################
# Question 2B - use LOOCV and glmnet to estimate LASSO MSE
##################

loocv_glmnet <- function(x, y, lambda_opt) {

  # create a holding vector for our squared error
  sq_err <- numeric(nrow(x))

  # create a holding matrix for our coefficients
  mod_coef <- matrix(0, nrow=nrow(x), ncol=ncol(x) + 1) # add one for intercept

  for (i in 1:nrow(x)){
      # every record minus 1
      loo_dat <- x[-i, ]
      loo_resp <- y[-i, ]
      # validation record
      valid_dat <- t(matrix(x[i, ]))
      valid_resp <- t(matrix(y[i, ]))
      # create the linear model
      glmnet_mod <- glmnet::glmnet(x=loo_dat
                                   , y=loo_resp
                                   , family="gaussian"
                                   , alpha=1
                                   , lambda=lambda_opt
                                   )

      # make a prediction
      loo_pred <- predict(glmnet_mod, valid_dat)

      # record our squared error
      sq_err[i] <- (valid_resp[, 1] - loo_pred)**2
      # record the model coefficients
      mod_coef[i, ] <- t(matrix(coef(glmnet_mod)))
      }
  MSE <- sum(sq_err) / nrow(x)

  return(list(sqr_err=sq_err
              , MSE=MSE
              , mod_coef=mod_coef
              )
         )
}


glmnet_out <- loocv_glmnet(x=X, y=Y, lambda_opt=cv_lambda)

# print the MSE
glmnet_out$MSE

##################
# Question 2C - bootstrap the sd of the MSE
##################

glmnet_boot <- loo_bootstrap(x=glmnet_out$sqr_err)

plot(density(glmnet_boot))
sd(glmnet_boot)


##################
# Question 2D - compare the B coefficients from the LASSO to the LS model
##################

t(matrix(colMeans(glmnet_out$mod_coef)))

lin_mod <- lm(Y~X)
glmnet_mod <- glmnet::glmnet(x=X
                             , y=Y
                             , family="gaussian"
                             , alpha=1
                             , lambda=cv_lambda
                             )
final_coef <- cbind(
            matrix(coef(lin_mod))
            , matrix(colMeans(glmnet_out$mod_coef))
            )


colnames(final_coef) <- c("lin_mod", "reg_mod")
final_coef

# everything except for the intercept term has been shrunk towards 0
# this is likely because the terms were not significant in the linear model to begin with


