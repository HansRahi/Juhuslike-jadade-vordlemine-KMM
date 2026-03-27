n = 100
m = 1

p = 0.51

#Bernoulli jaotuse generaator
rber = function(p) {
  return(rbinom(1,1,p))
}

teekonnad = matrix(nrow = m, ncol = n)
for (j in 1:m) {
  #if (j == 1) start = eelmine = Sys.time()
  Z = rep(NA, n)
  Z[1] = 0
  for (i in 2:n) {
    samm = 2*rber(p)-1
    if (Z[i-1] >= 0) {
      Z[i] = Z[i-1] + samm
    } else {
      Z[i] = Z[i-1] - samm
    }
  }
  teekonnad[j,] = Z
}

x = 1:n
plot(x, teekonnad[1,], type="l", col="slateblue", lwd=2, xlab="aeg",
     ylab="olek")

#Naasmistõenäosuste arvutamine
catalan = function(n) {
  return(choose(2*n,n)*(n+1))
}
ent_2n = function(n,p) {
  return(catalan(n-1)*(1-p)^(n)*p^(n-1))
}
sum(ent_2n(1:500, 0.98) )
