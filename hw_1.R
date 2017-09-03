
#anderson_philip.pdf

# hw_08
library("tidyverse")
library("GGally")
library("lattice")
# question 1

oxygen <- read.delim("C:/Users/Philip/Schools/TAMU/STAT_636/homework/oxygen.dat"
                   , header=F
                   , sep="")

# report a table showing sample averages for each variable, by gender

oxy_summ <- oxygen %>%
  dplyr::group_by(V5) %>%
  dplyr::summarise(
                  X1_mean=mean(V1)
                   , X1_sd=sd(V1)
                   , X2_mean=mean(V2)
                   , X2_sd=sd(V2)
                   , X3_mean=mean(V3)
                   , X3_sd=sd(V3)
                   , X4_mean=mean(V4)
                   , X4_sd=sd(V4)
                   )

oxy_summ

# without conducting any tests, it appears that men have, 
# on average, higher oxygen volumes in all scenarios, but higher
# variance on these metrics while conducting strenuous exercise


# B - make a pairs plot (pairwise scatterplot)

pairs(oxygen)
# replaces one half of symmetrical matrix with other info
GGally::ggpairs(oxygen)

# one individual appears to have very high values for 
# V1 and V2 - let's determine who that is
summary(oxygen)

subset(oxygen, V1 >= 0.65)

# record 48 appears to be an outlier
# there appears to be a strong relationship between V1 and V2,
# resting metrics.  Gender appears to be a stronger discriminator
# across V3 and V4 than V1 or V2


# C make a co-plot 

coplot(V1 ~ V3 | V5, columns=2
       , data=oxygen
      )

# it appears that V1 and V3 are more strongly related 
# for male subjects than female

# question 3

# B - use the persp function to graph the pdf

library(MASS)
mu <- c(1,-1)
sigma <- matrix(c(1.0, -1.6, -1.6, 4.0), 2)
bivn <- mvrnorm(n=5000, mu=mu, Sigma=sigma)
plot(density(bivn[,1]))
plot(density(bivn[,2]))

bivn.kde <- kde2d(
                  x=bivn[,1]
                  , y=bivn[,2]
                  , n=50
                  )


persp(bivn.kde, phi=20, theta=35, ticktype = 'detailed'
      ,xlab="x coordinate"
      , ylab="y coordinate")

help(dmvnorm)
matrix(-10:9, nrow=10)

fill <- matrix(0, nrow=100, ncol=100)

base_df <- data.frame(i_val=0
           ,j_val=0
           , density_val=0)

for (i in seq(-5,5, by=0.1))
  {
   for (j in seq(-5,5, by=0.1))
   {
   hold <- dmvnorm(
                   x=c(i,j)
                  , mean=c(1,-1)
                  , sigma=matrix(c(1.0, -1.6, -1.6, 4.0), nrow=2 , byrow=T)
                   )
   iter_df <- data.frame(
                         i_val=i
                         , j_val=j
                         , density_val=hold
                         )
   base_df <- rbind(base_df, iter_df)
   }
  }

base_df <- base_df[-1,]

den_val <- as.matrix(base_df['density_val'])
head(den_val)

persp(
  x=seq(-5,5,by=0.1)
  , y=seq(-5,5,by=0.1)
  , z=den_val
  )



# specify a function

phil_mvnorm <- function(mu_vec, sigma_mat, p=2, x_input)
  {
term_one <- (1 / ( ( (2*pi)**(p/2) ) * ( (det(sigma_mat))**(1/2) ) ) ) 

term_two <- exp(((-t(x_input - mu_vec)) %*% (solve(sigma_mat)) %*% (x_input - mu_vec)) / 2)

term_three <- term_one * term_two

return(term_three)
  }


fill <- matrix(0, nrow=100, ncol=100)

# x
for (i in seq(-2,4,length.out=100))
{
  # y 
  for (j in seq(-10,5,length.out=100))
  {
  fill[i,j] <- phil_mvnorm(
                           mu_vec=matrix(c(1 ,-1), nrow=2)
                           , matrix(c(1.0, -1.6, -1.6, 4.0), nrow=2 , byrow=T)
                           , x_input=matrix(c(i,j), nrow=2)
                            )
  }
}

nrow(fill)

persp(x=seq(-2, 4, length.out=100)
      , y=seq(-10,5, length.out=100)
      , z=fill
      , ticktype="detailed"
      , xlab='x-axis'
      , ylab='y-axis')

View(fill)
help(persp)

# question 4

one <- matrix(c(4.000, 4.001, 4.001, 4.002), nrow = 2, byrow=T)
one
one_inv <- solve(one)

1 / det(one)

two <- matrix(c(4.000, 4.001, 4.001, 4.002001)
              ,nrow=2
              , byrow=T)
two
two_inv <- solve(two)

1 / det(two)

one_inv
two_inv

3*two_inv


# question 6
