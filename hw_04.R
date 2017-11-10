
# Homework 04
# Phil Anderson
# panders2@tamu.edu
# Due: 12 Nov 17

##################
# import third-party modules
##################
library("tidyverse")
library("MASS")
library("caret")

##################
# import the data
##################
hof <- read.csv("/Users/Philip/Schools/TAMU/STAT_636/homework/hof_data.csv")
names(hof) <- tolower(names(hof))

##################
# manipulate the data
##################

# we are only keeping a handful of predictors - limit down to these now
hof2 <- hof %>%
  dplyr::select(id, name, hof, h, hr, rbi, avg, slg, obp)

# get a sense of hof proportion
table(hof2$hof) / length(hof2$hof) # check
# getting into the hall of fame appears to be a relatively rare event

##################
# Linear Discriminant Analysis
##################

lda1 <- MASS::lda(hof ~ h + hr + rbi + avg + slg + obp
                  , data=hof2
#                  , prior=c(0.5, 0.5) # non-informative prior
                  , CV=TRUE
                  )

# match the posterior classification estimates with the true classification

lda_pred <- cbind(hof2$hof
                  , data.frame(lda1$posterior)
                  )
names(lda_pred) <- c("true_class", "pred_N_pr", "pred_y_pr")


# loop preliminaries
kappa <- seq(from=0.01, to=0.5, by=0.01)
n <- length(kappa)
lda_sens <- numeric(n)
lda_spec <- numeric(n)
lda_ppv <- numeric(n)
lda_npv <- numeric(n)
lda_ba <- numeric(n)

for (i in 1:n) { 

lda_pred$pred_class <- ifelse(lda_pred$pred_y_pr > kappa[i]
                              , 'Y'
                              , 'N'
                              )

pre_conf <- table(lda_pred$pred_class, lda_pred$true_class)

caret::confusionMatrix(pre_conf)

conf <- caret::confusionMatrix(pre_conf, positive='Y')

lda_sens[i] <- conf$byClass[1]
lda_spec[i] <- conf$byClass[2]
lda_ppv[i] <- conf$byClass[3]
lda_npv[i] <- conf$byClass[4]
lda_ba[i] <- ((lda_sens[i] + (3 * lda_spec[i])) / 4)
  }

##################
# Quadratic Discriminant Analysis
##################

qda1 <- MASS::qda(hof ~ h + hr + rbi + avg + slg + obp
                  , data=hof2
#                  , prior=c(0.5, 0.5) # non-informative prior
                  , CV=TRUE
                  )
# match the posterior classification estimates with the true classification

qda_pred <- cbind(hof2$hof
                  , data.frame(qda1$posterior)
                  )
names(qda_pred) <- c("true_class", "pred_N_pr", "pred_y_pr")

# loop preliminaries
kappa <- seq(from=0.01, to=0.5, by=0.01)
n <- length(kappa)
qda_sens <- numeric(n)
qda_spec <- numeric(n)
qda_ppv <- numeric(n)
qda_npv <- numeric(n)
qda_ba <- numeric(n)

for (i in 1:n) { 

qda_pred$pred_class <- ifelse(qda_pred$pred_y_pr > kappa[i]
                              , 'Y'
                              , 'N'
                              )

pre_conf <- table(qda_pred$pred_class, qda_pred$true_class)

conf <- caret::confusionMatrix(pre_conf, positive='Y')

qda_sens[i] <- conf$byClass[1]
qda_spec[i] <- conf$byClass[2]
qda_ppv[i] <- conf$byClass[3]
qda_npv[i] <- conf$byClass[4]
qda_ba[i] <- ((qda_sens[i] + (3 * qda_spec[i])) / 4)
  }


##################
# Graphics
##################

# Sensitivity 
plot(x=NULL, y=NULL
    , xlim=c(min(kappa), max(kappa))
    , ylim=c(min(lda_sens, qda_sens), 1)
    , main="Model Sensitivity Comparison"
    )
lines(kappa, lda_sens, type="l", col="dodgerblue", lwd=2
     , xlab=expression(kappa)
     , ylab="Sensitivity"
      )
lines(kappa, qda_sens, type="l", col="forestgreen", lwd=2)
legend(0.35, 0.35
       , c("LDA", "QDA")
       , lty=c(1,1)
       , lwd=c(2,2)
       , col=c("dodgerblue", "forestgreen")
       )

# Specificity 
plot(x=NULL, y=NULL
    , xlim=c(min(kappa), max(kappa))
    , ylim=c(min(lda_spec, qda_spec), 1)
    , main="Model Specificity Comparison"
    )
lines(kappa, lda_spec, type="l", col="dodgerblue", lwd=2
     , main="Model Specificity Comparison"
     , xlab=expression(kappa)
     , ylab="Specificity"
      )
lines(kappa, qda_spec, type="l", col="forestgreen", lwd=2)
legend(0.05, 0.983
       , c("LDA", "QDA")
       , lty=c(1,1)
       , lwd=c(2,2)
       , col=c("dodgerblue", "forestgreen")
       )


# Positive Predictive Value 
plot(x=NULL, y=NULL
     , xlim=c(min(kappa), max(kappa))
     , ylim=c(min(lda_ppv, qda_ppv) , 1)
     , main="Model PPV Comparison"
     )
lines(kappa, lda_ppv, type="l", col="dodgerblue", lwd=2
     , xlab=expression(kappa)
     , ylab="PPV"
      )
lines(kappa, qda_ppv, type="l", col="forestgreen", lwd=2)
legend(0.35, 0.91
       , c("LDA", "QDA")
       , lty=c(1,1)
       , lwd=c(2,2)
       , col=c("dodgerblue", "forestgreen")
       )


# Negative Predictive Value 
plot(x=NULL, y=NULL
     , xlim=c(min(kappa), max(kappa))
     , ylim=c(min(lda_npv, qda_npv) , 1)
     , main="Model NPV Comparison"
    )
lines(kappa, lda_npv, type="l", col="dodgerblue", lwd=2
     , xlab=expression(kappa)
     , ylab="NPV"
      )
lines(kappa, qda_npv, type="l", col="forestgreen", lwd=2)
legend(0.35, 0.89
       , c("LDA", "QDA")
       , lty=c(1,1)
       , lwd=c(2,2)
       , col=c("dodgerblue", "forestgreen")
       )

# Balanced Accuracy
plot(x=NULL, y=NULL
     , xlim=c(min(kappa), max(kappa))
     , ylim=c(min(lda_ba, qda_ba) , 1)
     , main="Model Balanced Accuracy Comparison"
    )
lines(kappa, lda_ba, type="l", col="dodgerblue", lwd=2
     , xlab=expression(kappa)
     , ylab="Balanced Accuracy"
      )
lines(kappa, qda_ba, type="l", col="forestgreen", lwd=2)
legend(0.35, 0.98
       , c("LDA", "QDA")
       , lty=c(1,1)
       , lwd=c(2,2)
       , col=c("dodgerblue", "forestgreen")
       )


##################
# Best choice of kappa
##################
which.max(lda_ba)
lda_final <- data.frame(
                kappa[which.max(lda_ba)]
                , lda_sens[which.max(lda_ba)]
               , lda_spec[which.max(lda_ba)]
               , lda_ppv[which.max(lda_ba)]
               , lda_sens[which.max(lda_ba)]
               )
names(lda_final) <- c("Kappa" , "Sensitivity", "Specificity",
                      "PPV", "NPV")

which.max(qda_ba)
qda_final <- data.frame(
              kappa[which.max(qda_ba)]
              , qda_sens[which.max(qda_ba)]
              , qda_spec[which.max(qda_ba)]
              , qda_ppv[which.max(qda_ba)]
              , qda_sens[which.max(qda_ba)]
)
names(qda_final) <- c("Kappa", "Sensitivity", "Specificity",
                      "PPV", "NPV")

print(lda_final)
print(qda_final)
