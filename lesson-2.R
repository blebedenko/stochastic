library(pbapply)

res <- pbreplicate(1e5,{
  HH <- FALSE
  coin1 <- sample(c("H","T"),1)
  coin2 <- sample(c("H","T"),1)
  number_flips <- 2
  while(!HH){
    
    if(coin1=="H" && coin2=="H")
      HH <- TRUE
    else{
      coin1 <- coin2
      coin2 <- sample(c("H","T"),1)
      number_flips <- number_flips + 1
    }
  }
  
  number_flips
}
)

mean(res)
