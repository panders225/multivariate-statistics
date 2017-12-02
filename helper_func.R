


loocv_lm <- function(x, mod_form) {
  ###
  # Parameters:
  # x = data.frame with response and predictors. Response called 'Y' in col 1
  # mod_form = str containing model expression
  ###
  # create a holding vector for our squared error
  sq_err <- numeric(nrow(x))

    for (i in 1:nrow(x)) {

      # data for building model.  Every record minus 1
      loo_dat <- x[-i, ]
      # hold-out record for validation
      valid_dat <- x[i, ]
      # create a linear model on n-1 obs
      lin_mod <- lm(
                    formula=mod_form
                    , data=loo_dat
                      )
      # make a prediction on hold-out
      loo_pred <- predict(lin_mod, valid_dat)
      # record our squared error for hold-out
      sq_err[i] <- (valid_dat[, 1] - loo_pred)**2

    }
  # calculate the average MSE for all n models
  MSE <- sum(sq_err) / nrow(x)

  return(list(
              sq_err=sq_err
              , MSE=MSE
              )
         )
}

# EG
#mod_one <- loocv_lm(x=lin_dat, mod_form="Y ~ x1 + x2")
#mod_two <- loocv_lm(x=lin_dat, mod_form="Y ~ x1 + x2 + x3")
#mod_three <- loocv_lm(x=lin_dat, mod_form="Y ~ x1 + x2 + x3 + x4 + x5 + x6")



loo_bootstrap <- function(X, mod_form, B=1000) {
  ###
  # Parameters:
  # X: data.frame - entire training set including response and predictors
  # response labeled 'Y' in col 1.
  # mod_form: str - desired model formula
  # B: numeric - number of bootstrap replicates - defaults to 1000
  ###

  # holding vector for our bootstrapped MSE values.
  bs_mse <- numeric(B)

  for (i in 1:B){
    # seed for replicability
    set.seed(1738+i)
    # take a sample with as many rows in the data set, with replacement
    hold_dat <- X[sample(x=1:nrow(X), replace=T) , ]
    # create a model
    hold_mod <- lm(formula=mod_form, data=X)
    # make a prediction on the sampled data set
    pred <- predict(hold_mod, hold_dat)
    # mse
    bs_mse[i] <- mean((hold_dat$Y - pred)**2)

  }

  return(bs_mse)
}

# Example
#mod_three_bs <- loo_bootstrap(X=lin_dat
#                              ,mod_form="Y ~ x1 + x2 + x3 + x4 + x5 + x6"
#                              , B=1000
#                              )
#
#sd(mod_three_bs)
