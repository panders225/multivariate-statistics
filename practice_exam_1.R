
# practice exam 1
library("DescTools")
library("Hotelling")
library("tidyverse")


# read in the data
pref <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/exam_prep/consumer_pref.csv")
names(pref) <- tolower(names(pref))

str(pref)

##################
# Question 1A
##################

# generate single-pane qqplots
qqnorm(pref$taste); qqline(pref$taste)
qqnorm(pref$value); qqline(pref$value)
qqnorm(pref$flavor); qqline(pref$flavor)
qqnorm(pref$snack); qqline(pref$snack)
qqnorm(pref$energy); qqline(pref$energy)

##################
# Question 1B
##################


x_bar <- colMeans(pref)
S <- var(pref)
(p <- ncol(pref))
(n <- nrow(pref))


eigen_S <- eigen(S)
(primary_axes <- eigen_S$vectors)
(half_lengths <- sqrt(eigen_S$values) * 
    sqrt(((p * (n - 1)) / (n * (n - p)) ) * 
           qf(0.95, p, n - p)))


##################
# Question 1C
##################

bonf_intervals <- matrix(NA, nrow = p, ncol = 2) 

for(i in 1:p) 
  bonf_intervals[i, ] <- x_bar[i] + c(-1, 1) * qt(1 - 0.025 / p, n - 1) * sqrt(S[i, i] / n)

# display the results
cbind(bonf_intervals[,1], x_bar, bonf_intervals[,2])


##################
# Question 1D
##################

(mu_0 <- rep(0, p))

# drop turns this into a scalar
T2_obs <- drop(n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0)) 

test_stat <- ((n - p) / ((n - 1) * p)) * T2_obs
p_val <- 1 - pf(test_stat,p, n - p) 
print(p_val)

# let's use the slightly easier version
DescTools::HotellingsT2Test(x=pref, mu=mu_0)

##################
# Question 1E
##################

B <- 1000
T2_b <- numeric(B)
set.seed(101)

# this will recenter our data 
pref_0 <- scale(pref, center=T, scale=F)

for (b in 1:B) {
  X_b <- pref_0[sample(1:n, replace=T), ]
  x_bar_b <- colMeans(X_b)
  S_b <- var(X_b)
  T2_b[b] <- n * t(x_bar_b - mu_0) %*% solve(S_b) %*% (x_bar_b - mu_0)
}

# obtain our p-value
sum(T2_b > T2_obs) / B





