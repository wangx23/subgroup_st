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
  
  res1 = glmnet(x = Xmat,y = Yvec, intercept = FALSE, standardize = TRUE,standardize.response = FALSE,lambda = 0.1)

  plot(coef(res1)[-1], Bmm)
  
  matrix(coef(res1)[-1], nrows, ncols)
  
  
  matrix(predict.glmnet(object = res1,newx = Xmat),nrows, ncols)

  Bmm = solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% Yvec
  matrix(Bmm, nrows, ncols)
  
  res2 = lars(Xmat, Yvec, normalize = FALSE, intercept = FALSE)

}