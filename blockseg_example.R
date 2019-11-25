library(blockseg)
n <- 50
K <- 5
mu <- suppressWarnings(matrix(rep(c(1,0),ceiling(K**2/2)), K,K))
Y <- rblockdata(n,mu,sigma=.5)$Y
res <- blockSeg(Y, 20)
criteria(res, Y, sigma=.5)
res <- blockSeg(Y, 100)
predict(res, Y, lambda=slot(res, "Lambda")[1:3])

stab.out <- stab.blockSeg(Y, 100, 15)
plot(stab.out,Y)


res1 <- blockSeg(Y1, 50)
predict(res, Y, lambda=slot(res, "Lambda")[1:3])

stab.out <- stab.blockSeg(Y, 100, 15)
plot(stab.out,Y)