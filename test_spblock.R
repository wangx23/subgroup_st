##### test for irregular matrix with spatial block####
######## sp matrix ###
library(glmnet)
library(lars)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)

n = 10 ## regular grid size 
ntime = 6
n0 = n^2
n1 = 50
n2 = 30
time1 = 3
grids = matrix(1,ntime,1) %x% as.matrix(expand.grid(1:n,1:n))
colnames(grids) = c("x","y") ### row number and column number 
grids = as.data.frame(grids)

Ymat = matrix(0, n0, ntime)
Ymat[1:n1,1:time1] = rnorm(n1*time1)*0.1 + 4
Ymat[(n1+1):n0,1:time1] = rnorm((n0-n1)*time1)*0.1 - 4

Ymat[1:n2,(time1+1):ntime] = rnorm(n2*(ntime - time1))*0.1 + 4
Ymat[(n2+1):n0,(time1 + 1):ntime] = rnorm((n0-n2)*(ntime - time1))*0.1 - 4

datadf = grids %>% mutate(
  year = rep(1:ntime, each = n^2),
  grouptime = rep(1:2, each = ntime/2*n*n),
  groupsp = c(rep(rep(1:2, c(n1,n0-n1)),time1), rep(rep(1:2, c(n2,n0-n2)),ntime - time1)),
  obs = c(Ymat)) %>%
  mutate(grouptime = as.factor(grouptime),
         groupsp = as.factor(groupsp))

ggplot(data = filter(datadf, year ==3), aes(x = y, y=n+1 - x, color = obs)) +
  geom_point() + theme_bw() + theme(legend.position = "none")

ggplot(data = filter(datadf, year ==4), aes(x = y, y=n+1 - x, color = obs)) +
  geom_point() + theme_bw() + theme(legend.position = "none")

theme1 = theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")

g1 = ggplot(data = filter(datadf, year ==3), 
            aes(x = y, y=n+1 - x, color = as.factor(groupsp))) +
  geom_point() + theme1

g2 = ggplot(data = filter(datadf, year ==4), 
            aes(x = y, y=n+1 - x, color = as.factor(groupsp))) +
  geom_point() + theme1

pdf("/Volumes/GoogleDrive/My Drive/Research/Subgroups_st/docs/figures/twogroups_regular.pdf",height = 3,width = 7)
grid.arrange(g1,g2, ncol = 2)
dev.off()

### column matrix 
datmat =  select(datadf, y, x, year, obs) %>% spread(year, obs)
Ymat = datmat[,-(1:2)]
Ymat = as.matrix(Ymat)

dat1 = select(datadf, y, x, year, obs, groupsp) %>% 
  mutate(index = rep(n0:1, ntime))

ggplot(data = dat1, aes(x = year, y = index)) + geom_raster(aes(fill = obs))
ggplot(data = dat1, aes(x = year, y = index)) + geom_raster(aes(fill = groupsp))

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
coefmat = matrix(coef(res1), nrows, ncols)
Ypred1 = U1 %*% coefmat %*% t(U2)
plot(Ypred1, Yvec)
sum(coefmat!=0)
table(Ypred1)

dat2 = dat1
dat2$obs = c(Ypred1)
dat2$groupsp = as.factor(dat2$obs)
ggplot(data = dat2, aes(x = year, y = index)) + geom_raster(aes(fill = obs))
ggplot(data = dat2, aes(x = year, y = index)) + geom_raster(aes(fill = groupsp))


res3 = lars(x = Xmat,y = Yvec,intercept = FALSE,normalize = FALSE,
            max.steps = 20, type = "lar")
coefmat3 = matrix(coef(res3)[10,], nrows, ncols)
Ypred3 = U1 %*% coefmat3 %*% t(U2)
image(Ypred3)
plot(c(Ypred3), Yvec)
sum(coefmat3!=0)
table(Ypred3)

dat3 = dat1
dat3$obs = c(Ypred3)
dat3$groupsp = as.factor(dat3$obs)
g1 = ggplot(data = dat3, aes(x = year, y = index)) + geom_raster(aes(fill = obs))
g2 = ggplot(data = dat3, aes(x = year, y = index)) + geom_raster(aes(fill = groupsp))





##### row order 


datmat2 =  select(datadf, x, y, year, obs) %>% arrange(year,x) %>% spread(year, obs)
Ymat2 = datmat2[,-(1:2)]
Ymat2 = as.matrix(Ymat2)
Yvec2 = c(Ymat2)

dat21 = select(datadf, x, y, year, obs)  %>% arrange(year,x) %>%
  mutate(index = rep(n0:1, ntime))

ggplot(data = dat21, aes(x = year, y = index)) + geom_raster(aes(fill = obs))



res2 = glmnet(x = Xmat[,-1],y = Yvec2, intercept = TRUE, standardize = FALSE,
              lambda = 0.01)
coefmat21 = matrix(coef(res2), nrows, ncols)
Ypred21 = U1 %*% coefmat21%*% t(U2)
plot(Ypred21, Yvec2)
sum(coefmat21!=0)
table(Ypred21)


res23 = lars(x = Xmat,y = Yvec2,intercept = FALSE,normalize = FALSE,
            max.steps = 20, type = "lar")
coefmat23 = matrix(coef(res23)[20,], nrows, ncols)
Ypred23 = U1 %*% coefmat23 %*% t(U2)
image(Ypred23)
plot(c(Ypred23), Yvec2)
sum(coefmat23!=0)
