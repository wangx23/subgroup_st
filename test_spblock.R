##### test for irregular matrix ####

U1 = matrix(1,4,4)
U2 = matrix(1,3,3)
U1[upper.tri(U1)] = 0
U2[upper.tri(U2)] = 0

B1 = matrix(0,4,3)
B1[,1] = rnorm(4)
B1[,3] = rnorm(4)

Y1 = U1 %*% B1 %*% t(U2)
X1 = U2 %x% U1
max(abs(c(Y1)  - X1 %*% c(B1)))
  
######## sp matrix ###
library(glmnet)
library(lars)
Ymat = matrix(0, 100, 4)
Ymat[1:50,1:2] = rnorm(100)*0.1 + 5
Ymat[51:100,1:2] = rnorm(100)*0.1 - 5

Ymat[1:30,3:4] = rnorm(60)*0.1 + 5
Ymat[31:100,3:4] = rnorm(140)*0.1 - 5
image(Ymat)

ncols = ncol(Ymat)
nrows = nrow(Ymat)
U1 = matrix(1,nrows,nrows)
U2 = matrix(1,ncols,ncols)
U1[upper.tri(U1)] = 0
U2[upper.tri(U2)] = 0

Yvec = c(Ymat)

Xmat = U2 %x% U1

res1 = glmnet(x = Xmat[,-1],y = Yvec, intercept = TRUE, standardize = FALSE,
              lambda = 0.05)

Ypred1 = predict.glmnet(object = res1,Xmat[,-1])
coefmat = matrix(coef(res1), nrows, ncols)
Ypred2 = U1 %*% coefmat %*% t(U2)
image(Ypred2)
max(abs(Ypred1 - c(Ypred2)))
plot(Ypred1, Yvec)
sum(coefmat!=0)



res2 = lars(x = Xmat,y = Yvec,intercept = FALSE,normalize = FALSE,
            max.steps = 20, type = "lar")
coefmat2 = matrix(coef(res2)[9,], nrows, ncols)
Ypred3 = U1 %*% coefmat2 %*% t(U2)
image(Ypred3)
plot(c(Ypred3), Yvec)
sum(coefmat2!=0)



#### confirmed correct method 
