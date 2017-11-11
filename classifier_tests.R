
library("xgboost")

hof <- read.csv("/Users/Philip/Schools/TAMU/STAT_636/homework/hof_data.csv")
names(hof) <- tolower(names(hof))

hof2 <- hof %>%
  dplyr::select(id, name, hof, h, hr, rbi, avg, slg, obp)

hof2$hof_ind <- as.numeric(hof2$hof)
table(hof2$hof_ind, hof2$hof_ind)


set.seed(1738)
train <- dplyr::sample_frac(hof2, size=0.5)
train_dat <- train %>%
  dplyr::select(h, hr, rbi, avg, slg, obp) 
# make everything numeric, not int for xgboost
train_dat2 <- sapply(train_dat, as.numeric)




train_lab <- train %>%
  dplyr::select(hof_ind) %>%
  dplyr::mutate(hof_ind2=hof_ind-1)

# create the test set
test <- dplyr::anti_join(x=hof2, y=train, by=c("id"))

test_dat <- test %>%
  dplyr::select(h, hr, rbi, avg, slg, obp)
test_lab <- test %>%
  dplyr::select(hof_ind) %>%
  dplyr::mutate(hof_ind2=hof_ind-1)
test_dat2 <- sapply(test_dat, as.numeric)


xgmod <- xgboost::xgboost(
                          data=train_dat2
                          , label=train_lab[,2] # just using the [0,1]
                          , max_depth=4
                          , nrounds=2
                          , objective="binary:logistic"
                          , verbose=2
                          )

str(xgmod)
summary(xgmod)

# try predicting new variables 
xg_pred <- predict(xgmod, test_dat2)
xg_pred_class <- as.numeric(xg_pred > median(xg_pred))
class(test_lab[,2])
class(xg_pred_class)
test_acc <- cbind(as.data.frame(xg_pred_class)
                  , as.data.frame(test_lab[,2])
                  )

caret::confusionMatrix(table(test_acc), positive='1')

# h20 stuff
library("h2o")

h2o.init(nthreads=1, max_mem_size="1G")

train_h2o <- as.h2o(train_dat2)
train_h2o_lab <- as.h2o(train_lab)

test_h2o <- as.h2o(test_dat2)
test_h2o_lab <- as.h2o(test_lab)

tot_test <- cbind(train_h2o, train_h2o_lab)

rf1 <- h2o.randomForest(
  training_frame=tot_test
  , validation_frame=
)

##################
# shutdown
##################
h2o.removeAll()
h2o.shutdown(prompt=FALSE)



##################
# ISLR stuff CV/Bootstrap
##################
library("ISLR")
set.seed(1)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower
             , data=Auto
             , subset=train)

mean( (Auto$mpg - predict(lm.fit, Auto))[-train]**2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2)
              , data=Auto
              , subset=train
              )
mean((Auto$mpg - predict(lm.fit2, Auto))[-train]**2)


glm.fit <- glm(mpg ~ horsepower
               , data=Auto)
coef(glm.fit)


library(boot)

glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i) 
                 , data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

set.seed(17)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10



