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
  
  res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,lambda = 0.02)
  Ypred = matrix(predict.glmnet(object = res1,newx = Xmat[,-1]),nrows, ncols)
  
  par(mfrow = c(1,2))
  image(Ymat)
  image(Ypred)
  
  
  dat1 = select(datadf, y, x, year, obs) %>% 
    mutate(index = rep(1:100, 4)) %>%
    mutate(ypred = predict.glmnet(object = res1,newx = Xmat[,-1]))
  
  
  library(gridExtra)
  
  g1 = ggplot(data = dat1, aes(x = year, y = index, color = obs)) + geom_point()
  g2 = ggplot(data = dat1, aes(x = year, y = index, color = ypred)) + geom_point()
  grid.arrange(g1,g2)
 
  coefmat = matrix(coef(res1), nrows, ncols)
  image(coefmat)
}
