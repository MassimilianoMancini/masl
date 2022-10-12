################################
# Let us understand better rpart
################################

### my functions

datagen <- function(seed = 111, n = 100, a = 3, b = 3, linear = TRUE) {
  set.seed(seed)
  X1 <- rnorm(n)
  X2 <- a*X1+rnorm(n)
  X3 <- a*X1+rnorm(n)
  X4 <- a*X1+rnorm(n)
  if (linear) {
    Y <- b*X2 + b*X3 + b*X4 + rnorm(n)
  } else {
    Y <- b*X2^2 + b*X3 + b*X4 + rnorm(n)
  }
  
  return (data.frame(X1=X1, X2=X2, X3=X3, X4=X4, Y=Y))
}


datagen2 <- function(seed = 111, n = 200, a = 3, linear = TRUE) {
  set.seed(seed)
  X1 <- rnorm(n)
  if (linear) {
    Y <- a*X1 + rnorm(n)
  } else {
    Y <- a*X1^2 + rnorm(n)
  }
  
  return (data.frame(X1=X1, Y=Y))
}

### my analysis
mydatTrain <- datagen(a = 0.1)
mydatTest <- datagen(a = 0.1, seed = 1)

mytree <- rpart::rpart(Y ~ ., data = mydatTrain)

rpart.plot::rpart.plot(mytree)

mydatTrain2 <- datagen2(linear = FALSE)
mydatTest2 <- datagen2(seed = 1)

mytree2 <- rpart::rpart(Y ~ ., data = mydatTrain2)

rpart.plot::rpart.plot(mytree2)

plot(mydatTrain2$X1, mydatTrain2$Y)

summary(lm(Y ~ ., data = mydatTrain))

round(cor(mydatTrain), 2)

round(ggm::parcor(var(mydatTrain)), 2)


summary(lm(Y ~ X1, data = mydatTrain))
