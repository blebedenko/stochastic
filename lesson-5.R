p <- 0.4
R <- matrix(c(0,p,0,1-p,
              1-p,0,p,0,
              0,1-p,0,p,
              p,0,1-p,0),nrow = 4,byrow = T)

R
matrixcalc::matrix.power(R,2)
matrixcalc::matrix.power(R,3)
matrixcalc::matrix.power(R,4)
matrixcalc::matrix.power(R,5)
matrixcalc::matrix.power(R,5000)
matrixcalc::matrix.power(R,5001)


R <- matrix(c(0,p,0,0,1-p,
              1-p,0,p,0,0,
              0,1-p,0,p,0,
              0,0,1-p,0,p,
              p,0,0,1-p,0),nrow = 5,byrow = T)

R
matrixcalc::matrix.power(R,2)
matrixcalc::matrix.power(R,3)
matrixcalc::matrix.power(R,4)
matrixcalc::matrix.power(R,5)
matrixcalc::matrix.power(R,10)
matrixcalc::matrix.power(R,20)
matrixcalc::matrix.power(R,500)
matrixcalc::matrix.power(R,501)




## Targil 3 example 

P <- 0.1 * matrix(c(0,9,1,0,
              8,1,0,1,
              0,5,3,2,
              1,0,0,9), nrow = 4, byrow = T)

v <- eigen(t(P))$vectors[,1] # stationary vector = left eigenvector of P 
v <- v/sum(v) # stationary vector
v
v %*% P %*% P %*% P%*% P%*% P%*% P%*% P%*% P

matrixcalc::matrix.power(P,500)
v
