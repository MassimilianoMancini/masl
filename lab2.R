n <- 50
a <- 1
b <- 2
mU <- (a+b)/2
sdU <- ((1/12)*(b-a)^2)^0.5
sdmU <- sdU/sqrt(n)

nsim <- 1000
media <- c()

for (i in 1:nsim) {
  set.seed((111+i))
  x <- runif(n, a, b)
  media[i] <- mean(x)
}
c(mean(media), mU)
c(sd(media), sdmU)



