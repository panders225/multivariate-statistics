
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


