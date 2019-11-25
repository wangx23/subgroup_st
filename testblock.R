##### glmnet or lars to test 
library(glmnet)
library(lars)

library(blockseg)
n <- 50
K <- 5
mu <- suppressWarnings(matrix(rep(c(1,0),ceiling(K**2/2)), K,K))
Y <- rblockdata(n,mu,sigma=.5)$Y

image(Y)

ncols = ncol(Y)
U1 = matrix(1,ncols,ncols)
U1[upper.tri(U1)] = 0
Xmat = U1 %x% U1

Yvec = c(Y)

res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,
              lambda = 0.001)

Ypred = matrix(predict.glmnet(object = res1,newx = Xmat[,-1]),ncols, ncols)
image(Ypred)
