
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1)

x1 <- runif(100, 3, 500)
x2 <- rexp(100)
x3 <- runif(100, 5, 100)
x4 <- rexp(100)
x5 <- runif(100, 7, 300)
e <- rnorm(10)

b0 <- 5
b1 <- 7
b2 <- 3
b3 <- 6
b4 <- 10
b5 <- 20

y <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + e

b1 <- -5
b2 <- 100
b3 <- 4
b4 <- 0
b5 <- -10

b1s = b2s = b3s = b4s = b5s = c()

for (i in 1:10) {
  a <- y - b1 * x1 - b3 * x3 - b4 * x4 - b5 * x5
  b2 <- lm(a ~ x2)$coef[2]
  b2s[i] <- b2
  
  a <- y - b1 * x1 - b2 * x2 - b4 * x4 - b5 * x5
  b3 <- lm(a ~ x3)$coef[2]
  b3s[i] <- b3
  
  a <- y - b1 * x1 - b2 * x2 - b3 * x3 - b5 * x5
  b4 <- lm(a ~ x4)$coef[2]
  b4s[i] <- b4
  
  a <- y - b1 * x1 - b2 * x2 - b3 * x3 - b4 * x4
  b5 <- lm(a ~ x5)$coef[2]
  b5s[i] <- b5
  
  a <- y - b2 * x2 - b3 * x3 - b4 * x4 - b5 * x5
  b1 <- lm(a ~ x1)$coef[2]
  b1s[i] <- b1
  
}

plot(b1s, type='b')
plot(b2s, type='b')
plot(b3s, type='b')
plot(b4s, type='b')
plot(b5s, type='b')

