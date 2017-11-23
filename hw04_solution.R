

##################
# hw04 solutions from alan dabney
##################

##################
# third-party modules
##################
library("MASS")

##################
# data import and manipulation
##################


DTA <- read.csv("/Users/Philip/Schools/TAMU/STAT_636/homework/hof_data.csv")

num_vars <- c("H", "HR", "RBI", "AVG", "SLG", "OBP")
X <- as.matrix(DTA[, num_vars])
X_st <- scale(X, center=T, scale=T)
DTA_st <- data.frame(DTA$HOF, X_st)
colnames(DTA_st) <- c("HOF", num_vars)

p <- ncol(X)

x_bar <- colMeans(X)
S <- var(X)
R <- cor(X)

##################
# using LDA and QDA with CV
##################

# set-up
kappa <- seq(from=0, to=0.5, by=0.01)
lda_out <- MASS::lda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data=DTA_st, CV=T)
qda_out <- MASS::qda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data=DTA_st, CV=T)

bacc_lda <- sens_lda <- spec_lda <- ppv_lda <- npv_lda <- numeric(length(kappa))
bacc_qda <- sens_qda <- spec_qda <- ppv_qda <- npv_qda <- numeric(length(kappa))


# loop it
for (i in length(kappa)){
  cls_lda <- ifelse(lda_out$posterior[,2] > kappa[i], "Y", "N")
  sens_lda[i] <- mean(lda_out$posterior[DTA_st$HOF == "Y", 2] > kappa[i])
  spec_lda[i] <- mean(lda_out$posterior[DTA_st$HOF == "N", 2] <= kappa[i])
  ppv_lda[i] <- mean(DTA_st$HOF[lda_out$posterior[, 2] > kappa[i]] == "Y")
  npv_lda[i] <- mean(DTA_st$HOF[lda_out$posterior[, 2] <= kappa[i]] == "N")
  bacc_lda[i] <- (sens_lda[i] + 3 * spec_lda[i]) / 4

  cls_qda <- ifelse(qda_out$posterior[,2] > kappa[i], "Y", "N")
  sens_qda[i] <- mean(qda_out$posterior[DTA_st$HOF == "Y", 2] > kappa[i])
  spec_qda[i] <- mean(qda_out$posterior[DTA_st$HOF == "N", 2] <= kappa[i])
  ppv_qda[i] <- mean(DTA_st$HOF[qda_out$posterior[, 2] > kappa[i]] == "Y")
  npv_qda[i] <- mean(DTA_st$HOF[qda_out$posterior[, 2] <= kappa[i]] == "N")
  bacc_qda[i] <- (sens_qda[i] + 3 * spec_qda[i]) / 4
}





