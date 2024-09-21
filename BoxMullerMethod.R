n = 1000

samples1 = rep(NA,n)
samples2 = rep(NA,n)

for (sim in 1:n) {
  U1 = runif(1)
  U2 = runif(1)
  R = sqrt(-2*log(U1))
  theta = 2*pi*U2
  samples1[sim] = R*cos(theta)
  samples2[sim] = R*sin(theta) 
}

#compare to rnorm
label = rep("s1",1000)
df = as.data.frame(cbind(samples1, label))

samples1 = rnorm(1000)
label = rep("rvals",1000)

df2 = as.data.frame(cbind(samples1,label))

df3 = rbind(df,df2)

df3$samples1 = as.numeric(df3$samples1)

library(ggplot2)

ggplot(df3, aes(x=samples1, color=label, fill=label)) +
  geom_histogram(position="identity", alpha=0.5)+
  theme(legend.position="top") + theme_classic()


