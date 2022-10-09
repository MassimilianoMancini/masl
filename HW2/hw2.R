rm(list = ls())

set.seed(1)

x1 <- runif(100, 3, 500)
x2 <- rexp(100)
e <- rnorm(10)

b0 <- 5
b1 <- 7
b2 <- 3

y <- b0 + b1*x1 + b2*x2

b1s = c()
b2s = c()

b1 <- -5000

for (i in 1:5) {
  a <- y - b1 * x1
  b2 <- lm(a ~ x2)$coef[2]
  b2s[i] <- b2
  
  a <- y - b2 * x2
  b1 <- lm(a ~ x1)$coef[2]
  b1s[i] <- b1
}

plot (b1s)
plot (b2s)


