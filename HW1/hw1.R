rm(list = ls())

library(igraph)
library(ergm)

set.seed(1)

# Binomial Random Graphs
data(florentine)
names = get.vertex.attribute(flobusiness, "vertex.names")

Y <- as.matrix.network(flobusiness, mode = 'undirected')
n <- nrow(Y)
pmle <- mean(Y, na.rm = TRUE)

# Binomial random graph
brg <- graph_from_adjacency_matrix(Y, mode = 'undirected')
png(filename = 'graph.png', width = 1024, height = 1024)
plot (brg, vertex.size = 20, edge.arrow.size = 0.005, vertex.label = names)
dev.off()

# density
rhoObs <- mean(Y, na.rm = TRUE)

# transitivity
traObs <- transitivity(brg)

B <- 1000
rhoSim <- c()
traSim <- c()
for (b in 1:B) {
  Ysim = matrix(0, n, n)
  tmp = rbinom(n*(n-1)/2, 1, pmle)
  Ysim[lower.tri(Ysim)] = tmp 
  Ysim = Ysim + t(Ysim)
  brgSim <- graph_from_adjacency_matrix(Ysim)
  rhoSim[b] <- mean(Ysim)
  traSim[b] <- transitivity(brgSim)
}

# graphical comparison
png(filename = 'mean.png', width = 1024, height = 1024)
hist(rhoSim, col = "lightgray", main = "Mean")
abline(v = rhoObs, col = "red", lwd = 2)
dev.off()
png(filename = 'tran.png', width = 1024, height = 1024)
hist(traSim, col = "lightgray", main = "Transitivity")
abline(v = traObs, col = "red", lwd = 2)
dev.off()

mean(rhoObs > rhoSim)
mean(traObs > traSim)
