##### test for irregular matrix ####

Ymat = matrix(0, 100, 4)
Ymat[1:50,1:2] = rnorm(100)*0.1 + 2
Ymat[51:100,1:2] = rnorm(100)*0.1 - 2

Ymat[1:30,3:4] = rnorm(60)*0.1 + 2
Ymat[31:100,3:4] = rnorm(140)*0.1 - 2


ncols = ncol(Ymat)
nrows = nrow(Ymat)
U1 = matrix(1,nrows,nrows)
U2 = matrix(1,ncols,ncols)
U1[upper.tri(U1)] = 0
U2[upper.tri(U2)] = 0

Yvec = c(Ymat)

Xmat = U2 %x% U1

res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,lambda = 0.02)
Ypred1 = predict.glmnet(object = res1,Xmat[,-1])
Ypred2 = U1 %*% coefmat %*% t(U2)
  
  
coefmat = matrix(coef(res1), nrows, ncols, byrow = TRUE)
