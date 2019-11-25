###### slightly change of the blockseg algorithm #### 
## the column and the row can be different 
## maxK 
library(glmnet)
irregblockseg = function(Ymat, maxK, lam)
{
  if(class(Ymat) != "matrix")
  {
    "Ymat needs to be matrix"
  }
  ncols = ncol(Ymat)
  nrows = nrow(Ymat)
  U1 = matrix(1,ncols,ncols)
  U2 = matrix(1,nrows,nrows)
  U1[upper.tri(U1)] = 0
  U2[upper.tri(U2)] = 0
  
  Yvec = c(Ymat)
  
  Xmat = U2 %x% U1
  
  res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,lambda = 0.2)
  Ypred = matrix(predict.glmnet(object = res1,newx = Xmat[,-1]),nrows, ncols)
  
  plot(Ypred, Yvec)
  matrix(coef(res1), nrows, ncols)
  
  
  Bmm = solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% Yvec
  Ypred1 = Xmat %*% Bmm


}