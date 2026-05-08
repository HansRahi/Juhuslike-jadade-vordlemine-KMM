#Positiivselt korduva juhusliku ekslemise režiimiga VMM-i näite genereerimine

library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)

#abifunktsioon juhusliku ekslemise sammude genereerimiseks
samm = function(p) {
  return(2*rbinom(1,1,p)-1)
}

#Graafikul kuvatavate jadade pikkus
n = 40

#burn-in perioodi pikkus
m = 30

#p
p = 0.4
#emissioonitõenäosuste vektorit genereeriv funktsioon
emis = function(z) {
  return(c(rep(1/4 + atan((2*z+1)/4)/(2*pi),2), rep(1/4 - atan((2*z+1)/4)/(2*pi),2)))
}

#Võimalikud emissioonid
U = list(c(0,0), c(1,1), c(0,1), c(1,0))

#burn-in periood
Z = rep(NA, m)
Z[1] = 0
for (i in 2:m) {
  if (Z[i-1] >= 0) {
    Z[i] = Z[i-1] + samm(p)
  } else {
    Z[i] = Z[i-1] - samm(p)
  }
}

#Näite genereerimine
z = Z[m]
X = Y = Z = rep(NA, n)
if (z >= 0) {
  Z[1] = z + samm(p)
} else {
  Z[1] = z - samm(p)
}
u = sample(U, 1, prob = emis(Z[1]))
X[1] = u[[1]][1]
Y[1] = u[[1]][2]

for (i in 2:n) {
  if (Z[i-1] >= 0) {
    Z[i] = Z[i-1] + samm(p)
  } else {
    Z[i] = Z[i-1] - samm(p)
  }
  u = sample(U, 1, prob = emis(Z[i]))
  X[i] = u[[1]][1]
  Y[i] = u[[1]][2]
}

t = 1:n
W = data.frame(t,X,Y,Z)
W_long = W %>% arrange(t) %>%
  pivot_longer(cols = c(X, Y), names_to = "Jada", values_to = "Seisund")

#Režiimi olekute ristkülikute loomine
Z = W %>% mutate(xmin = pmax(t - 0.5, 1), xmax = pmin(t + 0.5, n))

#Esimese 40 vaatluse graafik
U_graafik = ggplot() +
  geom_rect(data = Z, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, 
                fill = Z), alpha = 0.6) +
  geom_line(data = W_long, 
            aes(x = t, y = Seisund, group = Jada, color = Jada), 
            size = 1, alpha = 0.6) +
  scale_fill_gradient2(low = "lightsalmon2", 
                       mid = "white", 
                       high = "palegreen2", 
                       midpoint = -0.5,
                       breaks = seq(-6, 6, by = 2),
                       guide = guide_colorbar(
                         barheight = unit(4, "cm"), 
                         nbin = 100,                
                         ticks = TRUE,              
                         frame.colour = "black"     
                       )) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(1, n)) +
  scale_y_continuous(breaks = c(0, 1), 
                     labels = c("0","1"), 
                     limits = c(-0.1, 1.1)) +
  scale_color_manual(values = c("X" = "blue", "Y" = "green")) +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.5))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 0.15) +
  labs(#title = sprintf("Varjatud Markovi Mudel, p = %0.2f, q = %0.2f, P(X=Y|Z=1) = %0.2f", p, q, f1),
    x = "Samm",
    y = "Emissioon",
    fill = "Režiim",
    color = "Jada")

U_graafik

ggsave(filename = sprintf("PK_JE_esimesed_%d_graafik.pdf", n),
       plot = U_graafik,
       device = "pdf",
       width = 24,
       height = 8,
       units = "cm")
