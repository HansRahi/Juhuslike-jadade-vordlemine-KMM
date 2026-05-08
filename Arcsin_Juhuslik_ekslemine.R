library(ggplot2)
library(VaRES)
library(scales)
#Juhusliku ekslemise tüüpi mudel


n = 5000
m = 10000

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

histogramm_graafikuga = ggplot(data = data.frame(Prop)) + 
  geom_histogram(aes(x = Prop, y = after_stat(density)), colour = 1, fill = "white", breaks = seq(0,1,0.04), ) +
  stat_function(data = data.frame(x = c(0,1)), aes(x = x, colour = "tihedusfunkstioon"), fun = darcsine, lwd = 0.9) +
  theme_bw() + 
  labs(x = expression(L[n]/n), y = "Tihedus") +
  scale_colour_manual("", labels = c(expression(frac(1,pi * sqrt(x * (1 - x))))), values = c("deepskyblue"))
ggsave(filename = "JE_Ln_histogramm.pdf",
       plot = histogramm_graafikuga,
       device = "pdf",
       width = 18,
       height = 12,
       units = "cm")
histogramm_graafikuga

