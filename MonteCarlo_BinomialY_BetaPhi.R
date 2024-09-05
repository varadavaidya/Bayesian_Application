########Problem based on identical function 1a ##########
set.seed(32)
m=-100
a=2.0
b=(1.0/3.0)
theta=rgamma(n=100,shape=2.0,rate=(1/3))
theta
hist(theta,freq=FALSE)
curve(dgamma(x,shape=2,rate=(1/3)),color="blue",add=TRUE)

sum(theta)/100  #mean of theta by monte carlo simulation m=100 and theta generated
#for Gamma dist. the expected value of theta is shape/rate i.e 2/(1/3)=6

E_of_theta=a/b

E_of_theta
#Now we know that mean of monte carlo simulation is near to Expected value of theta for same Gamma distribution

#but we can still improve

#now increase m to 10000

m=10000
#new simulation
theta=rgamma(n=10000,shape=2,rate=(1/3))
head(theta)
tail(theta)
#new mean by new monte carlo simulation
mean(theta)
#which is near to true E_theta by Gamma
var(theta) #variance by monte carlo simulation
#variance for gamma distr. is a/b^2
var_by_std_gamma=a/b^2


#both are quiet close

#indicator function to know how many theta values are less than 5 i.e I{theta<5}(theta)
ind=theta<5
head(ind)
mean(ind) # we get 0.497
pgamma(q=5,shape=2,rate=1/3) #quintiles=5 means less than 5
# we get 0.4963317
quantile(theta,probs = 0.9) #under monte carlo simulation the 90th percentile value of theta in the vector generated is 11.74
qgamma(0.9,shape=2,rate=1/3) # we get 11.66916

########

se=sd(theta)/sqrt(m) #std error for our approximation of theta
se # we get 0.04247727

mean(theta)-2*se # we get5.938319
mean(theta)+2*se # we get 6.108228

ind=theta<5
mean(ind) # we get 0.497
pgamma(5,2,1/3) # we get 0.4963317
se=sd(ind)/sqrt(m)
se
2*se

####PROBLEM 1b###########
#####From Heirarchical model##########
#Given phi_i from Beta(2,2) and y_i from Binom(10,phi_i)

m=100000

y=numeric(m)
head(y)
phi=numeric(m)
head(phi)

for(i in 1:m){
  phi[i]=rbeta(1,shape1=2,shape2=2) # phi are chosen from Beta distribution
  y[i]=rbinom(1,size=10,prob=phi[i]) # y from binomial
}

# or in vectorized code

phi=rbeta(m,shape1=2,shape2=2)
y=rbinom(m,size=10,prob=phi)

mean(y)#we get 5.00008
