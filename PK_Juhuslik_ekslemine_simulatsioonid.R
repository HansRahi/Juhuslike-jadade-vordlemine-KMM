#Programm positiivselt korduva juhusliku ekslemise režiimiga VMM-i korral L_n/n koondumise kontrollimiseks

library(ggplot2)
library(dplyr)
library(Rcpp)
sourceCpp ("NW2.cpp")

#abifunktsioon juhusliku ekslemise sammude genereerimiseks
samm = function(p) {
  return(2*rbinom(1,1,p)-1)
}

#emissioonitõenäosuste vektorit genereeriv funktsioon
emis = function(z) {
  return(c(rep(1/4 + atan((2*z+1)/4)/(2*pi),2), rep(1/4 - atan((2*z+1)/4)/(2*pi),2)))
}

#Võimalikud emissioonid
U = list(c(0,0), c(1,1), c(0,1), c(1,0))

#Funktsioon gamma hindamiseks ette antud režiimi üleminekumaatriksiga VMM-i korral
#n_vec - genereeritavate jadade pikkuste arv
#m - iga pikkuse korral genereeritud jadade arv
#P - režiimi üleminekumaatriks
#a - emissioonijaotuse parameeter
genereeri_JE_andmestik = function(n_vec, m, p, a) {
  keskmised = kvantiil_25 = kvantiil_75 = rep(NA, length(n_vec))
  for (i in (1:length(n_vec))) {
    cat(sprintf("Genereerin m = %i jada pikkusega n = %i \n", m, n_vec[i]))
    Ln_n = rep(NA,m)
    n = n_vec[i]
    #burn-in perioodi pikkus
    #arvestame, et suurema p korral esineb rohkem triivimist
    b = round(p * 500)
    for (j in (1:m)) {
      if (j == 1) start = eelmine = Sys.time()
      #burn-in periood
      Z = rep(NA, b)
      Z[1] = 0
      for (k in 2:b) {
        if (Z[k-1] >= 0) {
          Z[k] = Z[k-1] + samm(p)
        } else {
          Z[k] = Z[k-1] - samm(p)
        }
      }
      #jadade genereerimine
      z = Z[b]
      X = Y = Z = rep(NA, n)
      if (z >= 0) {
        Z[1] = z + samm(p)
      } else {
        Z[1] = z - samm(p)
      }
      u = sample(U, 1, prob = emis(Z[1]))
      X[1] = u[[1]][1]
      Y[1] = u[[1]][2]
      for (k in 2:n) {
        if (Z[k-1] >= 0) {
          Z[k] = Z[k-1] + samm(p)
        } else {
          Z[k] = Z[k-1] - samm(p)
        }
        u = sample(U, 1, prob = emis(Z[k]))
        X[k] = u[[1]][1]
        Y[k] = u[[1]][2]
      }
      #Ln/n leidmine
      x = paste(X, collapse = "")
      y = paste(Y, collapse = "")
      Ln_n[j] = LLCS(x,y)/n
      vahe = as.numeric(difftime(Sys.time(), eelmine), units = "secs")
      if (vahe > 2) {
        kulunud = as.numeric(difftime(Sys.time(), start, units = "secs"))
        jäänud = kulunud*(m/j - 1)
        cat(sprintf("Möödunud: %0.2f sekundit, jäänud: %0.2f sekundit, tehtud: %0.2f%% \n", kulunud, jäänud, j/m*100))
        eelmine = Sys.time()
      }
    }
    keskmised[i] = mean(Ln_n)
    kvantiilid = quantile(Ln_n, probs = c(25, 75)/100)
    kvantiil_25[i] = kvantiilid[1]
    kvantiil_75[i] = kvantiilid[2]
  }
  return(data.frame(n_vec, keskmised, kvantiil_25, kvantiil_75))
}

n_vec = c(100, 250, 500, 1000, 2000, 3000, 4000)
m = 100

tulemused_1_JE = genereeri_JE_andmestik(n_vec = n_vec, m = m, 0.3) %>% mutate(p = "p = 0,25")
tulemused_2_JE = genereeri_JE_andmestik(n_vec = n_vec, m = m, 0.4) %>% mutate(p = "p = 0,4")
tulemused_3_JE = genereeri_JE_andmestik(n_vec = n_vec, m = m, 0.45) %>% mutate(p = "p = 0,48")

andmestik_JE = bind_rows(tulemused_1_JE, tulemused_2_JE, tulemused_3_JE) %>% 
  mutate(p = factor(p, levels = c("p = 0,25","p = 0,4","p = 0,48")))

save(andmestik_JE, file = "PK_JE_Ln_n_andmed.RData")

JE_Koondumise_graafik = ggplot(andmestik_JE, aes(x = n_vec, y = keskmised, colour = p)) +
  geom_errorbar(
    aes(ymin = kvantiil_25, ymax = kvantiil_75), 
    width = 100,
    alpha = 0.7,
    linewidth = 0.4) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_x_continuous(breaks = unique(andmestik_JE$n)) +
  scale_colour_manual(values = c("dodgerblue2", "springgreen3", "orange2")) +
  labs(
    x = "n",
    y = expression(paste("keskmine  ", frac(L[n],n))),
    color = "Režiim") +
  theme_bw() +
  theme(legend.position = "bottom")

JE_Koondumise_graafik

ggsave(filename = sprintf("PK_JE_koondumise_graafik.pdf"),
       plot = JE_Koondumise_graafik,
       device = "pdf",
       width = 18,
       height = 12,
       units = "cm")
