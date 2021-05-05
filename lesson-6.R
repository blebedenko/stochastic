

P <- matrix(c(0.1,0.1,0.4,0.4,
              0.6,0.4,0,0,
              0.2,0.4,0.4,0,
              0.1,0.1,0.4,0.4),nrow = 4,byrow = T)

v <- eigen(t(P))$vectors[,1] 
v <- Re(v/sum(v)) # stationary vector
COST <- 20 * 0:3
sum(v * COST)

library(markovchain)

mc <- new('markovchain',transitionMatrix = P,states=as.character(0:3))
steadyStates(mc)


P %*% P%*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P  

