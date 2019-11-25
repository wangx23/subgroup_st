#### use glmnet and lars to test total variation algorithm ####

Y = c(rnorm(50,-2,0.1), rnorm(50,2,0.1))
ny = length(Y)
X = matrix(1,ny, ny)
X[upper.tri(X)] = 0

res1 = glmnet(x = X[,-1], y = Y,family = "gaussian",intercept = TRUE, 
              standardize = FALSE,
              lambda = 0)

Bmm = solve(t(X) %*% X)%*%t(X) %*% Y


res1$beta
Ypred = predict.glmnet(object = res1,newx = X)

res2 = lars(x = X, y = Y, intercept = FALSE,normalize = FALSE)
X %*% res2$beta[3,]
