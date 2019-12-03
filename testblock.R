##### glmnet or lars to test 
library(glmnet)
library(blockseg)

n <- 100
K <- 5
mu <- suppressWarnings(matrix(rep(c(2,-2),ceiling(K**2/2)), K,K))
Y <- rblockdata(n,mu,sigma=.5)$Y
res <- blockSeg(Y, 50)
stab.out <- stab.blockSeg(Y, 100, 15)
plot(stab.out,Y)

predict(stab.out)


image(Y)

ncols = ncol(Y)
U1 = matrix(1,ncols,ncols)
U1[upper.tri(U1)] = 0
Xmat = U1 %x% U1

Yvec = c(Y)

res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,
              lambda = 0.05)

coefmat = matrix(coef(res1), ncols, ncols)
sum(coefmat!=0)
image(coefmat)

Ypred1 = U1 %*% coefmat %*% t(U1)
Ypred2 = matrix(predict.glmnet(object = res1, Xmat[,-1]),ncols, ncols)
