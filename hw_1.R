
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




# specify a function
# im going to be hard-coding the mean and covariance matrices

phil_mvnorm <- function(x, y)
  {
    x_input <- matrix(c(x,y), nrow=2)
    mu_vec <- matrix(c(1 ,-1), nrow=2)
    sigma_mat <- matrix(c(1.0, -1.6, -1.6, 4.0), nrow=2 , byrow=T)
    p <- 2
    
    term_one <- (1 / ( ( (2*pi)**(p/2) ) * ( (det(sigma_mat))**(1/2) ) ) ) 
    
    term_two <- exp(((-t(x_input - mu_vec)) %*% (solve(sigma_mat)) %*% (x_input - mu_vec)) / 2)
    
    term_three <- term_one * term_two
    
    return(term_three)
  }

# set up a loop to create a 'long' dataset

base_df <- data.frame(x_val=0
                      , y_val=0
                      , z_val=0)

for (i in seq(-4,4,length.out=50))
  {
    for (j in seq(-4,4, length.out=50))
    {
      iter_df <- data.frame(x_val=i
                            , y_val=j
                            , z_val=phil_mvnorm(x=i, y=j)
                            )
      base_df <- rbind(base_df, iter_df)
    }
  }

base_df <- base_df[-1,]

final_grid <- reshape::cast(base_df, x_val ~ y_val
                            , fun.aggregate=mean
                            ) 


final_grid <- final_grid[,-1]
final_grid <- data.frame(final_grid)
final_grid <- as.matrix(final_grid)

x <- y <- seq(-4, 4, length.out=50)
persp(x, y, final_grid, phi=30, theta=60
      , ticktype='detailed'
      , main="Multivariate Density")



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

library("plotrix")

plot(c(0,10), c(0,10), type="n", main="test draw.ellipse")
draw.ellipse(c(3,7), c(8,8), c(5,1), c(1,5), col=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)))

draw.ellipse(c(7,7), c(7,8), c(0.5,1), c(1,0.5), col=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)))
draw.ellipse(c(7,7), c(7,8), c(0.5,1), c(1,0.5), col=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)))

help("draw.ellipse")

# 95th percentile of the chi squared distribution
qchisq(p=0.95, df=2)
sigma1 <- matrix(c(1.00, 0.80, 0.80, 1.00), nrow=2, byrow=F)
solve(sigma1)
help("draw.ellipse")

plot(c(-5,5), c(-5,5), type="n")
lines(mv_one[,1], mv_one[,2])

mean(mv_one[,1])
mean(mv_one[,2])

sigma1
mv_one <- rmvnorm(n = 5000, mean=c(1,1), sigma=sigma1)

head(mv_one)

bivn.kde <- kde2d(
                  x=mv_one[,1]
                  , y=mv_one[,2]
                  , n=50
                  )

persp(bivn.kde, phi=20, theta=35, ticktype = 'detailed'
      ,xlab="x coordinate"
      , ylab="y coordinate")



draw.ellipse()

# question 6

ellipse_draw <-function(mu_vec=c(1,1), cov_mat, title="Sigma 1", seed=1738)
{ 

         car::ellipse( center=mu_vec , shape=cov_mat , radius=sqrt(qchisq(p=0.95, df=2)) , lwd=5
                  , add=FALSE
                  , xlab="x-axis"
                  , ylab="y-axis"
                  , grid=TRUE
                  , main=title
                  , col = "black"
                  )
           
set.seed(seed)                          
rando <- mvtnorm::rmvnorm(n=5000, mean=c(1,1), sigma=cov_mat)
points(rando)              

rando_df <- data.frame(rando)
names(rando_df) <- tolower(names(rando_df))
base_df <- data.frame(point=0, in_out=9)

for (i in 1:nrow(rando_df))
  {
  temp <- t(matrix(c(rando_df[i,1] - 1, rando_df[i,2] - 1))) %*%
   solve(cov_mat) %*%
   matrix(c(rando_df[i,1] - 1, rando_df[i,2] - 1))
 
temp_df <- data.frame(point=temp[1,1])
temp_df$in_out <- ifelse(
  temp_df$point <= qchisq(p=0.95, df=2), 1, 0  
                        )
  base_df <- rbind(base_df, temp_df)
    }

base_df <- base_df[-1,]
table(base_df$in_out) / nrow(base_df)
}


# Execute the functions
# 1
ellipse_draw(cov_mat=matrix(c(1.00, 0.80, 0.80, 1.00), nrow=2)
             , seed=1
             , title="Sigma 1")
# 2
ellipse_draw(cov_mat=matrix(c(1.00, 0.0, 0.0, 1.00), nrow=2)
             , seed=2
             , title="Sigma 2")
# 3
ellipse_draw(cov_mat=matrix(c(1.00, -0.80, -0.80, 1.00), nrow=2)
             , seed=3
             , title="Sigma 3")
#4
ellipse_draw(cov_mat=matrix(c(1.00, 0.40, 0.40, 0.25), nrow=2)
             , seed=4
             , title="Sigma 4")
# 5
ellipse_draw(cov_mat=matrix(c(1.00, 0.0, 0.0, 0.25), nrow=2)
             , seed=5
             , title="Sigma 5")
# 6
ellipse_draw(cov_mat=matrix(c(1.00, -0.40, -0.40, 0.25), nrow=2)
             , seed=6
             , title="Sigma 6")
# 7
ellipse_draw(cov_mat=matrix(c(0.25, 0.40, 0.40, 1.00), nrow=2)
             , seed=7
             , title="Sigma 7")
# 8
ellipse_draw(cov_mat=matrix(c(0.25, 0.0, 0.0, 1.00), nrow=2)
             , seed=8
             , title="Sigma 8")
# 9
ellipse_draw(cov_mat=matrix(c(0.25, -0.40, -0.40, 1.00), nrow=2)
             , seed=9
             , title="Sigma 9")


# on average, I would expect that probability to be 0.95


