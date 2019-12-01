#### simulation ####
library(ggplot2)
library(tidyverse)
library(gridExtra)

### a function to define value 
mufun = function(x,y,threshold,mu1 = -2, mu2 = 2)
{
  if(x^2 + y^2 <= threshold){muvalue = mu1}else{
    muvalue = mu2
  }
  return(muvalue)
}

mufun = Vectorize(mufun)


### two groups 
groupassign = function(x,y,threshold)
{
  gr = 1
  if(x^2 + y^2 <= threshold){gr = 1}else{
    gr = 2
  }
  return(gr)
}

groupassign = Vectorize(groupassign)




#### several time observations and spatial lattice #####
n = 10 ## regular grid size 
ntime = 6

grids = matrix(1,ntime,1) %x% as.matrix(expand.grid(1:n,1:n))
colnames(grids) = c("x","y") ### row number and column number 
grids = as.data.frame(grids)


datadf = grids %>% mutate(
               year = rep(1:ntime, each = n^2),
               grouptime = rep(1:2, each = ntime/2*n*n),
               groupsp = case_when(grouptime == 1 ~ groupassign(x,y,36),
                                   grouptime == 2 ~ groupassign(x,y,64)),
               obs = case_when(grouptime ==1  ~  c(-4,4)[groupsp],
                               grouptime == 2 ~ c(-4,4)[groupsp])) %>%
  mutate(obs = obs + 0.1*rnorm(n()),
         grouptime = as.factor(grouptime),
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

pdf("/Volumes/GoogleDrive/My Drive/Research/Subgroups_st/docs/figures/twogroups.pdf",height = 3,width = 7)
grid.arrange(g1,g2, ncol = 2)
dev.off()

### column matrix 

datmat =  select(datadf, y, x, year, obs) %>% spread(year, obs)
Ymat = datmat[,-(1:2)]
Ymat = as.matrix(Ymat)

dat1 = select(datadf, y, x, year, obs) %>% 
  mutate(index = rep(100:1, ntime))

ggplot(data = dat1, aes(x = year, y = index, color = obs)) + geom_point()


dat1 = select(datadf, y, x, year, obs) %>% 
  mutate(index = rep(1:100, 4)) %>%
  mutate(ypred = predict.glmnet(object = res1,newx = Xmat[,-1]))


library(gridExtra)

g1 = ggplot(data = dat1, aes(x = year, y = index, color = obs)) + geom_point()
g2 = ggplot(data = dat1, aes(x = year, y = index, color = ypred)) + geom_point()
grid.arrange(g1,g2)

coefmat = matrix(coef(res1), nrows, ncols)
image(coefmat)



### row matrix 
datmat1 =  select(datadf, x, y, year, obs) %>% spread(year, obs)



