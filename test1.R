#### simulation ####
library(ggplot2)
library(tidyverse)

### a function to define value 
mufun = function(x,y,threshold,mu1 = -2, mu2 = 2)
{
  if(x^2 + y^2 <= threshold){muvalue = mu1}else{
    muvalue = mu2
  }
  return(muvalue)
}

mufun = Vectorize(mufun)

#### several time observations and spatial lattice #####
n = 10 ## regular grid size 
ntime = 4

grids = matrix(1,ntime,1) %x% as.matrix(expand.grid(1:n,1:n))
colnames(grids) = c("x","y") ### row number and column number 
grids = as.data.frame(grids)


datadf = grids %>% mutate(
               year = rep(1:ntime, each = n^2),
               group = rep(1:2, each = ntime/2*n*n),
               obs = case_when(group==1 ~  mufun(x,y,25, -4,4),
                               group == 2 ~ mufun(x,y,49, -4,4))) %>%
  mutate(obs = obs + 0.01*rnorm(n()))



ggplot(data = filter(datadf, year ==4), aes(x = y, y=n+1 - x, color = obs)) +
  geom_point()


### column matrix 

datmat =  select(datadf, y, x, year, obs) %>% spread(year, obs)
Ymat = datmat[,-(1:2)]
Ymat = as.matrix(Ymat)

### row matrix 
datmat1 =  select(datadf, x, y, year, obs) %>% spread(year, obs)



