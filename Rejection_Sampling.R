curve((3/2)*x^3+(11/8)*x^2+(1/6)*x+(1/12), 0, 1)
abline(a = 3.125, b = 0)

X = runif(4500, 0, 1)
U = runif(4500, 0, 1)

pi_x <- function(x) {
  new_x = (3/2)*(x^3)+(11/8)*(x^2)+(1/6)*(x)+(1/12)
  return(new_x)
}

count = 1
accept = c()

while(count <= 4500 & length(accept) < 1000){
  test_u = U[count]
  test_x = pi_x(X[count])/(3.125*dunif(X[count],0,1))
  if (test_u <= test_x){
    accept = rbind(accept, X[count])
    count = count + 1
  }
  count = count + 1
}

hist(accept)
