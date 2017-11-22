
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

# also for lm
lin_dat <- cbind(Y, X) %>% data.frame()

##################
#  function for leave-one-out cv (LOOCV)
##################

# for holding the MSE values
loocv_lm <- function(x) {
  # create a holding vector for our squared error
  sq_err <- numeric(nrow(x))

  for (i in 1:nrow(x)){

      # every record minus 1
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
      }
  MSE <- sum(sq_err) / nrow(x)

  return(list(sqr_err=sq_err
              , MSE=MSE))
}

loo_results <- loocv_lm(lin_dat)


# take a look at the density of the MSE values
plot(density(loo_results$sqr_err))

##################
# boostrapping function
##################

loo_bootstrap <- function(x, B=10000){

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

hold <- loo_bootstrap(x=loo_results$sqr_err)


B <- 10000
bs_holding <- numeric(B)

for (i in 1:B){
  set.seed(1738+i)
  bs_inter <- sample(
              x=loo_results$sqr_err
              , size=length(loo_results$sqr_err)
              , replace=T
              )
  bs_holding[i] <- mean(bs_inter)
  }

plot(density(bs_holding))
sd(bs_holding)
