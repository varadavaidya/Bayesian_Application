oring=read.delim("path")
plot(Temp,Index)
oring.lm=lm(Index~Temp)
summary(oring.lm)
#add fitted line to scatter plot
  
lines(Temp,fitted(oring.lm))
 
#95% posterior probability of slope

-0.24337-0.06349*qt(0.975,21)
-0.24337+0.06349*qt(0.975,21)
#probablity of failure when Temp=31 F

#Index=Temp(-0.24337)+18.36508
Index=31*(-0.24337)+18.36508
Index
coefficients(oring.lm)
#we now predict what is 95% confidence interval of Damage Index if Temp=31F
predict(oring.lm,data.frame(31),interval="predict")
predict(oring.lm,data.frame(Temp=31),interval="predict")
Index=31*(-0.24337)+18.36508