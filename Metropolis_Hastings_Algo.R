lg=function(mu,n,ybar){
  mu2=mu^2
  n*(ybar*mu-mu2/2)-log(1+mu2)
}

mh=function(n,ybar,n_iter,mu_init,cand_sd){
  mu_out=numeric(n_iter)
  accpt=0
  mu_now=mu_init
  lg_now=lg(mu=mu_now,n=n,ybar=ybar )
  
  for(i in 1:n_iter){
    mu_cand=rnorm(1,mean=mu_now,sd=cand_sd)
    
    lg_cand=lg(mu=mu_cand,n=n,ybar=ybar)
    alpha=exp(lg_cand-lg_now)
    
    u=runif(1)
    if(u<alpha){
      mu_now=mu_cand
      accpt=accpt+1
      lg_now=lg_cand
    }
    mu_out[i]=mu_now
    
  }
  list(mu=mu_out,accpt=accpt/n_iter)
}


##set up

y=c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.3,1.9)

ybar=mean(y)
n=length(y)

hist(y,freq=FALSE,xlim=c(-1.0,3.0))

points(y,rep(0,n))
points(ybar,0,pch=19)

curve(dt(x,df=1),lty=2,add=TRUE)#prior dist for mu whose mean is at zero 

#compared to the prior distribution, the histogram indicates data mean of 1
## so we would expect the mean of posterior distribution to be lying between zero(prior mean) and one(the data mean)

# SO NOW LOAD THE METROPOLIS HASTINGS FUNCTION FROM ABOVE in console

##posterior sampling
set.seed(43)
post=mh(n=n,ybar=ybar,n_iter=1000,mu_init=0,cand_sd=3)

str(post)

install.packages("coda")
library("coda")
traceplot(as.mcmc(post$mu))

##the plot would show that we need different candidate
post=mh(n=n,ybar=ybar,n_iter=1000,mu_init=0,cand_sd=0.05)
str(post)
traceplot(as.mcmc(post$mu))
#now change the candidate to something between 3 and 0.05
post=mh(n=n,ybar=ybar,n_iter=1000,mu_init=0,cand_sd=0.9)
str(post)
traceplot(as.mcmc(post$mu))

# we want our acceptance rate to be between 0.23 to 0.5
#the above candidate had  $ mu   : num [1:1000] 0.475 0.92 1.109 1.109 0.546 ...
##$ accpt: num 0.38

###post analysis

post$mu_keep=post$mu[-c(1:100)]
plot(density(post$mu_keep),xlim=c(-1,3))
curve(dt(x,df=1),lty=2,add=TRUE)
points(ybar,0,pch=19)
