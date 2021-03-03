
# sample() examples -----------------------

sample(x = c("H","T"),size = 1) # fair coin

sample(x = c("H","T"),size = 2,prob = c(0.6,0.4),replace = TRUE) # biased coin

sample(1:6, size =  10,replace = TRUE) # roll a fair die

urn <- c(rep("black",5), rep("white",3))
sample(urn,size = 3,replace = FALSE) # draw 3 balls without replacement


blood <- c("AB","B","A","O")
p <- c(4, 11 , 40 , 45) * 0.01 # use probabilities if possible
sample(blood,1,prob = p)

# test that this works
b <- sample(blood,1e4,prob = p,replace = TRUE)
plot(table(b))


# some more examples ------------------------------------------------------

chores <- c("David","Ilana","Amir")
sample(chores) # random permutation

M <- matrix(1:9,3,3)

M[sample(1:nrow(M),size = 1), ] # sample a random row

dplyr::sample_n(mtcars,2) # sample n rows




# Roulette ----------------------------------------------------------------


roul <- c(rep("red",16),
          rep("black",16),
          rep("no-color",8))

roul2 <- c("red","black","no-color")
sample(roul,size=1,replace = TRUE)
sample(roul2,1,replace = TRUE,prob = c(16/40,16/40,8/40))


roulette <- function(n = 1,red=16,black=16,nocolor=8){
  
  roul_colors <- c(rep("red",red),
                   rep("black",black),
                   rep("no-color",nocolor))
  
  res <- sample(roul_colors, size = n, replace = TRUE)
  return(res)
}

roulette(1,19,19,2)



# Stocks ------------------------------------------------------------------


# For one time period :

100 * sample(c(0.9,1.12),size = 1) # stock A

80 * sample(c(0.85,1.15),size = 1) # stock B

n <- 5
100*prod(sample(c(0.9,1.12),size = n,replace = TRUE))
80*prod(sample(c(0.85,1.15),size = n,replace = TRUE))

stock_A <- function(n=1,amount=100){
  # amount of stock bought (in $)
  mult <- prod(sample(c(0.9,1.12),size = n,replace = TRUE))
  return(amount*mult)
}

stock_B <- function(n=1,amount=80){
  # amount of stock bought (in $)
  mult <- prod(sample(c(0.85,1.15),size = n,replace = TRUE))
  return(amount*mult)
}



stock_A_after_12_time <- replicate(n = 1e4,
                                 {
                                   stock_A(n=12)
                                 })

hist(stock_A_after_12_time)


stock_B_after_12_time <- replicate(n = 1e4,
                                   {
                                     stock_B(n=12)
                                   })
hist(stock_B_after_12_time)



# roll 10 dice ----

expand.grid(c("A","B"),1:2)
DAT <- expand.grid(1:6,1:6,1:6,1:6,1:6,1:6,1:6,1:6,1:6,1:6)
head(DAT)
dim(DAT)
library("pbapply")
length(unique(as.numeric(DAT[1e6,])))
number_unique <- function(x){length(unique(x))}
pbapply(DAT,1,number_unique)





# simulation
MC <- 1e4
res <- numeric(MC)

for(i in 1:MC){
  svMisc::progress(i,MC)
  dice <- sample(1:6,10,TRUE)
  res[i] <- length(unique(dice))
}

mean(res == 6)


