
# Homework 04

# import third-party modules
library("tidyverse")
library("MASS")
library("caret")
# import the data

hof <- read.csv("/Users/Philip/Schools/TAMU/STAT_636/homework/hof_data.csv")
names(hof) <- tolower(names(hof))

# we are only keeping a handful of predictors - limit down to these now
hof2 <- hof %>%
  dplyr::select(id, name, hof, h, hr, rbi, avg, slg, obp)

# create a numeric indicator for HOF status
hof2$hof_ind <- as.numeric(hof$hof)
table(hof2$hof_ind, hof2$hof) # check
# getting into the hall of fame appears to be a relatively rare event

#hof2 <- hof2 %>%
#  dplyr::select(-c(id, name, hof))
str(hof2)

# let's get some initial models built

lda1 <- MASS::lda(hof ~ h + hr + rbi + avg + slg + obp
                  , data=hof2
                  , prior=c(0.95, 0.05)
                  , CV=TRUE
                  )

help(lda)
str(lda1)
View(lda1$posterior)

pred <- lda1$class %>%
  data.frame()
names(pred) <- c("predicted_class")

pred2 <- cbind(pred, hof$hof)
caret::confusionMatrix(table(pred2), positive='Y')


##################
# QDA
##################

qda1 <- MASS::qda(hof ~ h + hr + rbi + avg + slg + obp
                  , data=hof2
                  , prior=c(0.95, 0.05)
                  , CV=TRUE
                  )


qda_pred <- cbind(qda1$class, hof$hof) %>%
  data.frame()
names(qda_pred) <- c("predicted_class", "orig")
head(qda_pred)

caret::confusionMatrix(table(qda_pred$predicted_class, qda_pred$orig)
                       
                       )
