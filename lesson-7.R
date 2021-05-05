
replicate(1000,{
  
  Xk <- 1
  k <- 0
  while(Xk > 0){
    k <- k + 1
    N.k <- sample(0:2,size = Xk,replace = TRUE,prob = c(0.5,0.25,0.25))
    Xk <- sum(N.k)
  }
  k
  
})





K <- 
replicate(1000,{
  
  Xk <- 1
  k <- 0
  while(Xk > 0 && k<50){
    k <- k + 1
    N.k <- sample(0:2,size = Xk,replace = TRUE,prob = c(0.25,0.25,0.5))
    Xk <- sum(N.k)
  }
  k
  
})
mean(K==50)




# Poisson Offspring ------------------------------------------------------
samp_10_generations <- 
replicate(100,{
  Xk <- 1
  k <- 0
  while(Xk > 0 && k<10){
    k <- k + 1
    N.k <- rpois(n = Xk,lambda = 1.5)
    Xk <- sum(N.k)
  }
  Xk
  
})



samp_20_generations <- 
  replicate(100,{
    Xk <- 1
    k <- 0
    while(Xk > 0 && k<20){
      k <- k + 1
      N.k <- rpois(n = Xk,lambda = 1.5)
      Xk <- sum(N.k)
    }
    Xk
    
  })

samp_30_generations <- 
  replicate(100,{
    Xk <- 1
    k <- 0
    while(Xk > 0 && k<30){
      k <- k + 1
      N.k <- rpois(n = Xk,lambda = 1.5)
      Xk <- sum(N.k)
    }
    Xk
    
  })


mean(samp_10_generations==0)
mean(samp_20_generations==0)
mean(samp_30_generations==0)
