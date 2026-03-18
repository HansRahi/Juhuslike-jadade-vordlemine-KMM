#Programm sõltuvuse/sõltumatuse saartega VMM-i korral jadade genereerimiseks

library(ggplot2)
library(dplyr)
library(tidyr)

  #Bernoulli jaotuse generaator
rber = function(p) {
  return(rbinom(1,1,p))
}

#Graafikul kuvatavate jadade pikkus
n = 60
#järjestikuste 0-ide ja 1-de saarte arv
m = 10000

#q_Z(0|0)
p = 0.9
#q_Z(1|1)
q = 0.85
#P(X=Y|Z=1)
f1 = 0.96

#statsionaarne jaotus
pi_Z = c((q-1)/(q+p-2), (p-1)/(q+p-2))

#Režiimi Z üleminekute generaator
Z_yleminek = function(z) {
  if (z == 0) {
    return((1-rber(p)))
  }
  return(rber(q))
}


#Emissioonide (X,Y) generaator
emissioon = function(z) {
  if (z == 0) {
    x = rber(0.5)
    y = rber(0.5)
    return(c(x,y))
  }
  if (z == 1) {
    s = rber(f1)
    if (s == 1) {
      x = rber(0.5)
      return(c(x,x))
    }
    x = rber(0.5)
    return(c(x,(1-x)))
  }
}
#Jadade X,Y,Z vektorid, 2x pikemad, kui m režiimi ülemineku korral keskmiselt vaja oleks
X = Y = Z = rep(NA, m*(1/(1-p)+1/(1-q)))

#genereerime jadasid, kuni oleme lõpetanud m järjestikuste režiimi olekute saart
i = 0
rez_loendur = 1

Z_esimene = Z_jarg = rber(pi_Z[2])

while (rez_loendur <= m) {
  i = i + 1
  Z[i] = Z_jarg
  u = emissioon(Z[i])
  X[i] = u[1]
  Y[i] = u[2]
  Z_jarg = Z_yleminek(Z[i])
  if (Z[i] != Z_jarg) rez_loendur = rez_loendur + 1
}

#jätame tühjaks jäänud väljad andmetest välja
X = X[1:i]
Y = Y[1:i]
Z = Z[1:i]
t = 1:i

#esimesed n vaatlust
W = data.frame(t,X,Y,Z) %>% filter(t <= n)
W_long = W %>% arrange(t) %>%
  pivot_longer(cols = c(X, Y), names_to = "Jada", values_to = "Seisund")

#Režiimi saarte ristkülikute loomine
Z_saared = W %>%
  mutate(xmin = t - 0.5, xmax = t + 0.5)

#Esimese 60 vaatluse graafik
U_graafik = ggplot() +
  geom_rect(data = Z_saared, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = factor(Z)), 
            alpha = 1) +
  geom_line(data = W_long, 
            aes(x = t, y = Seisund, group = Jada, color = Jada), 
            size = 1, alpha = 0.6) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(1, n)) +
  scale_y_continuous(breaks = c(0, 1), 
                     labels = c("0","1"), 
                     limits = c(-0.1, 1.1)) +
  scale_fill_manual(values = c("0" = "white", "1" = "lightgrey")) +
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

ggsave(filename = sprintf("VMM_esimesed_%d_graafik.pdf", n),
       plot = U_graafik,
       device = "pdf",
       width = 24,
       height = 6,
       units = "cm")
U_graafik

#Leiame järjestikuste 0-ide ja 1-de saarte pikkused
#Loome andmestiku saarte pikkuste sagedustest mõlema režiimi korral.
saared = rle(Z)
saared_df = data.frame(
  seisund = factor(saared$values),
  pikkus = pmin(saared$lengths, 20)) %>%
  group_by(seisund, pikkus) %>%
  summarise(sagedus = n(), .groups = 'drop_last') %>%
  mutate(p = sagedus / (m/2), tyyp = "Empiiriline")

#loome sarnase tabeli 
teoreetilised_df = 
  expand.grid(seisund = c("0", "1"),
              pikkus = 1:20) %>%
  mutate(p = ifelse(seisund == "0", dgeom(pikkus - 1, 1-p), dgeom(pikkus - 1, 1-q)),
         tyyp = "Teoreetiline") %>% 
  group_by(seisund) %>%
  mutate(p = ifelse(pikkus == 20, 1 - sum(p[pikkus < 20]), p))

#ühendame andmestikud
vordlus_df <- bind_rows(saared_df, teoreetilised_df)

#loome empiiriliste ja teoreetiliste jaotuste tulpdiagrammid
x_labels <- c(as.character(1:19), "20+")

jaotused = ggplot(vordlus_df, aes(x = factor(pikkus), y = p, fill = tyyp)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~seisund, scales = "free_x", 
             labeller = as_labeller(c("0" = "0-ide saared", "1" = "1-de saared"))) +
  scale_x_discrete(labels = x_labels) + 
  scale_fill_manual(values = c("Empiiriline" = "palegreen", "Teoreetiline" = "tan1")) +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.5))) +
  theme_bw() +
  labs(x = "Saare pikkus",
       y = "Sagedus",
       fill = "Jaotus")
ggsave(filename = "VMM_saarte_pikkused.pdf",
       plot = jaotused,
       device = "pdf",
       width = 24,
       height = 12,
       units = "cm")
jaotused
