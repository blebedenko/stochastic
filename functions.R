# Functions for the stochastic shiny app
library(tidyverse)

# Lesson 1 ----------------------------------------------------------------


# For n days catch fish with Poiss(lambda) and prob. p of keeping
CatchFish <- function(n,lambda){
  fish_caught <- rpois(n=n,lambda=lambda)
  return(fish_caught)
}

KeepFish <- function(fish_caught,p){
  rbinom(n = length(fish_caught),size = fish_caught,prob = p)
}

PlotFish <- function(n,lambda,p){
  caught <- CatchFish(n,lambda)
  kept <- KeepFish(caught,p)

  caught_props <- prop.table(table(caught))
  caught_values <- as.numeric(names(caught_props))
  kept_props <- prop.table(table(kept))
  kept_values <- as.numeric(names(kept_props))
  par(mfrow=c(1,2))
  plot(caught_props,main="Sample proportions of # fish caught")
  points(x=caught_values,dpois(x=caught_values,lambda = lambda),type = "p",col="red")
  legend(x="topright",legend = c("True PMF"),col = "red",pch=1)
  plot(kept_props,main="Sample proportions of # fish kept")
  points(x=kept_values,dpois(x=kept_values,lambda = lambda*p),type = "p",col="blue")
  legend(x="topright",legend = c("True PMF"),col = "blue",pch=1)
  
}


FishShortage <- function(fish_kept,mu){
  demand <- rpois(length(fish_kept),mu)
  shortage <- pmax(0,demand-fish_kept)
  return(shortage)
}

# FishShortage(KeepFish(CatchFish(100,5),p = 0.5),mu = 5)
