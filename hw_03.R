# homework 3

library("tidyverse")

##################
# Question 1
##################


stock <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_636/homework/stock_prices.csv")
names(stock) <- tolower(names(stock))
str(stock)

# Part A - perform principal components analysis
pc <- princomp(stock)
str(pc)
summary(pc)

# ~53% of the total variance in the data set is explained by PC 1
# ~27% of the total variance in the data set is explained by PC 2

screeplot(pc, type="l")

# Part B - report and comment on the coefficients of the linear combo that 
# defines the first PC 
pc$loadings

# the signs of the coefficients in the linear combination should not be of 
# importance; we can see here that the most important components are 
# shell and exxonmobil.  This indicates that oil companies are reponsible
# for more of the variation in stock prices than the financial companies.
# This PC will highlight weeks with greater fluctutations in oil stock prices
# this makes sense as the oil companies have the greatest distributional spread

# part C report and comment on the coefficients of the linear combo that 
# defines the second PC
pc$loadings
apply(stock, MARGIN=2, FUN=summary)
apply(stock, 2, sd)

# The second PC moves to the finance companies, but highlights jp morgan and 
# citibank.  This makes sense, as these companies have a standard deviation
# that is ~33% greater than that of wells fargo.  


# part D - verify that the coefficients on the first two PCs equal the 
# first two eigenvalues of the sample covariance matrix S

S <- var(stock)  
eeS <- eigen(S)
eeS

round(pc$sdev[1:2] ** 2, 4)
round(eeS$values[1:2], 4)


# part E - generate a scatter plot of the first two PCs

apply(pc$scores[, 1:2], 2, summary)

# base R plotting
#plot(pc$scores[,1], pc$scores[,2])

# ggplot version
pc_scores <- pc$scores %>% data.frame()

p <- ggplot(pc_scores, aes(pc_scores$Comp.1, pc_scores$Comp.2))
p + geom_point() + 
  coord_cartesian(xlim=c(-.12, .12), ylim=c(-.12, .12)) +
  ggtitle("Principal Components 1 + 2 Scatterplot") +
  xlab("PC 1") + 
  ylab("PC 2")

# note that PC 1 has more variance than PC 2.
# max first
pc_scores[which.max(pc_scores$Comp.1), ]
# 63
# min second
pc_scores[which.min(pc_scores$Comp.1), ]
# 56
stock            

# week 63 has all negative inputs; week 56 has all positive inputs.  
# Given that the PC linear combination has all negative signs, it makes
# sense that all negative inputs have resulted in a large value for PC 1


##################
# Question 2
##################
