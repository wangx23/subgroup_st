###### slightly change of the blockseg algorithm #### 
## the column and the row can be different 
## maxK 
library(glmnet)
library(lars)
irregblockseg = function(Ymat, maxK, lam)
{
  if(class(Ymat) != "matrix")
  {
    "Ymat needs to be matrix"
  }
  
  
  ncols = ncol(Ymat)
  nrows = nrow(Ymat)
  U1 = matrix(1,nrows,nrows)
  U2 = matrix(1,ncols,ncols)
  U1[upper.tri(U1)] = 0
  U2[upper.tri(U2)] = 0

  Yvec = c(Ymat)
  Xmat = U2 %x% U1
  
  res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, 
               standardize = FALSE,lambda = 0.02)
  coefmat1 = matrix(coef(res1), nrows, ncols)
  Ypred11 = matrix(predict.glmnet(object = res1,newx = Xmat[,-1]),nrows, ncols)
  Ypred12 = U1 %*% coefmat %*% t(U2)
  
  
  res2 = lars(x = Xmat,y = Yvec,intercept = FALSE,normalize = FALSE,
              max.steps = 50, type = "lar")
  coefmat2 = matrix(coef(res2)[50,], nrows, ncols)
  Ypred22 = U1 %*% coefmat2 %*% t(U2)
  
  plot(Ypred22[,1],Ymat[,1])


}
