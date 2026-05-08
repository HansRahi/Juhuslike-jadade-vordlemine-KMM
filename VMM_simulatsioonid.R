#Programm sõltuvuse/sõltumatuse saartega VMM-i korral L_n/n koondumise kontrollimiseks

library(ggplot2)
library(dplyr)
library(Rcpp)
sourceCpp ("NW2.cpp")

#Saartega VMM-i üleminekumaatriksi loomise funktsioon
YM_maatriks = function(p, q) {
  return(matrix(c(p, 1-p, 1-q, q), nrow = 2, byrow = TRUE))
}

#võimalikud emissioonde paarid  
U = list(c(0,0), c(1,1), c(0,1), c(1,0))

#emissioonijaotuse vektori funktsioon
emis = function(z, a) {
  if (z == 1) return(c(rep(1/2 - a, 2), rep(a, 2)))
  return(rep(1/4, 4))
}

#Funktsioon gamma hindamiseks ette antud režiimi üleminekumaatriksi ja emissioonijaotuse parameetri korral
#n_vec - genereeritavate jadade pikkuste arv
#m - iga pikkuse korral genereeritud jadade arv
#P - režiimi üleminekumaatriks
#a - emissioonijaotuse parameeter
genereeri_VMM_andmestik = function(n_vec, m, P, a) {
  #statsionaarne jaotus
  pi_Z = c(P[2,1]/(P[2,1] + P[1,2]), P[1,2]/(P[2,1] + P[1,2]))
  keskmised = kvantiil_25 = kvantiil_75 = rep(NA, length(n_vec))
  for (i in (1:length(n_vec))) {
    cat(sprintf("Genereerin m = %i jada pikkusega n = %i \n", m, n_vec[i]))
    Ln_n = rep(NA,m)
    n = n_vec[i]
    for (j in (1:m)) {
      if (j == 1) start = eelmine = Sys.time()
      Z = X = Y = rep(NA, n)
      Z[1] = sample(c(0,1), 1, prob = pi_Z)
      #W_1
      u = sample(U, 1, prob = emis(Z[1], a))
      X[1] = u[[1]][1]
      Y[1] = u[[1]][2]
      #W_2,...W_n
      for (k in (2:n)) {
        Z[k] = sample(c(0,1), 1, prob = P[Z[k-1]+1,])
        u = sample(U, 1, prob = emis(Z[k], a))
        X[k] = u[[1]][1]
        Y[k] = u[[1]][2]
      }
      #Ln leidmine
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
a = 0.025

tulemused_1_VMM = genereeri_VMM_andmestik(n_vec = n_vec, m = m, P = YM_maatriks(0.9,0.8), a = a) %>% mutate(pq = "p = 0,9; q = 0,8")
tulemused_2_VMM = genereeri_VMM_andmestik(n_vec = n_vec, m = m, P = YM_maatriks(0.95,0.9), a = a) %>% mutate(pq = "p = 0,95; q = 0,9")
tulemused_3_VMM = genereeri_VMM_andmestik(n_vec = n_vec, m = m, P = YM_maatriks(0.8,0.8), a = a) %>% mutate(pq = "p = 0,8; q = 0,8")
tulemused_4_VMM = genereeri_VMM_andmestik(n_vec = n_vec, m = m, P = YM_maatriks(0.9,0.9), a = a) %>% mutate(pq = "p = 0,9; q = 0,9")

andmestik_VMM = bind_rows(tulemused_1_VMM, tulemused_2_VMM, tulemused_3_VMM, tulemused_4_VMM) %>% 
  mutate(pq = factor(pq, levels = c("p = 0,8; q = 0,8", "p = 0,9; q = 0,9", "p = 0,9; q = 0,8", "p = 0,95; q = 0,9")))

save(andmestik_VMM, file = "VMM_Ln_n_andmed.RData")

VMM_Koondumise_graafik = ggplot(andmestik_VMM, aes(x = n_vec, y = keskmised, colour = pq)) +
  geom_errorbar(
    aes(ymin = kvantiil_25, ymax = kvantiil_75), 
    width = 100,
    alpha = 0.7,
    linewidth = 0.4) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_x_continuous(breaks = unique(andmestik_VMM$n)) +
  scale_colour_manual(values = c("dodgerblue2", "dodgerblue4", "springgreen3", "springgreen4")) +
  labs(
    x = "n",
    y = expression(paste(frac(L[n],n), "  valimikeskmine")),
    color = "Režiim") +
  theme_bw() +
  theme(legend.position = "bottom")

VMM_Koondumise_graafik

ggsave(filename = sprintf("VMM_koondumise_graafik.pdf"),
       plot = VMM_Koondumise_graafik,
       device = "pdf",
       width = 18,
       height = 10,
       units = "cm")    
