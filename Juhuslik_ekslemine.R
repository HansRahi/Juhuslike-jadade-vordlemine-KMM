library(ggplot2)
library(VaRES)
#Juhusliku ekslemise tüüpi mudel


n = 10000
m = 1000

p = 0.5

Z = rep(NA, n)
Prop = rep(NA, m)
for (j in 1:m) {
  if (j == 1) start = eelmine = Sys.time()
  Z[1] = 2*(rbinom(1, 1, p)-0.5)
  for (i in 2:n) {
    Z[i] = Z[i-1] + 2*(rbinom(1, 1, p)-0.5)
  }
  Prop[j] = length(Z[Z > 0])/n
  vahe = as.numeric(difftime(Sys.time(), eelmine), units = "secs")
  if (vahe > 2) {
    kulunud = as.numeric(difftime(Sys.time(), start, units = "secs"))
    jäänud = kulunud*(m/j - 1)
    cat(sprintf("Möödunud: %0.2f sekundit, jäänud: %0.2f sekundit, tehtud: %0.2f%% \n", kulunud, jäänud, j/m*100))
    eelmine = Sys.time()
  }
}

mean(Prop)
var(Prop)

histogramm_graafikuga = ggplot(data = data.frame(Prop)) + 
  geom_histogram(aes(x = Prop, y = after_stat(density))) +
  stat_function(data = data.frame(x = c(0,1)), aes(x = x), fun = darcsine)
histogramm_graafikuga

