library(VGAM) # for triangle dist
library(dplyr)
library(ggplot2)

samples <- 1000000L
n_known <- 1L

r <- c(.5,3,9)        # Rate of star formation, solar masses / year
f_ghz <- c(0.3, 0.4, 0.5)   # Fraction of stars in the galactic habitable zone
f_sl <- c(0.15, 0.2, 0.25)   # Fraction of such stars suitable for life
f_p <- c(0.04, 0.75, 0.9)   # Fraction of suitable stars that have planetary systems
f_pr <- c(.1, 0.75, 0.9)  # Fraction of planetary systems with rocky planets
#n_e <- c(3) # Number of rocky planets per system
f_shz <- c(.05, 0.07, 0.15) # Fraction of rocky planets in the stellar habitable zone (including around large gas giants)
f_l <- c(0.01, 0.75, 0.95)   # Fraction of such planets that develop life of any sort
f_i <- c(.001, 0.1, 0.33)    # Fraction of planets with life that develop intelligent life
f_c <- c(0.0, 0.1, 1.0)   # Fraction of intelligent life that "leaks" or sends signals extra-stellar
L <- c(50., 100., 100000.)      # Length of time such civilizations communicate

rdist_func <- function(n, params, dist = "rtriangle") {
  func <- match.fun(dist)
  func(n, params[2], params[1], params[3])
}
N <- round(rdist_func(samples, r) *
             rdist_func(samples, f_ghz) *
             rdist_func(samples, f_sl) *
             rdist_func(samples, f_p) *
             rdist_func(samples, f_pr) *
             #rbinom(samples, n_e)
             rdist_func(samples, f_shz) *
             rdist_func(samples, f_l) *
             rdist_func(samples, f_i) *
             rdist_func(samples, f_c) *
             rdist_func(samples, L),
           0)

P_exact <- as.numeric(table(N)/sum(table(N)))

P_ex1 <- P_exact
for(i in 1:length(P_exact)) {
  P_ex1[i] <- ifelse(i-1 < n_known, NA, P_exact[i] / P_exact[n_known+1])
}

P_al <- rep(NULL, length(P_exact))
P_al[1] <- sum(P_exact)
for(i in 2:length(P_exact)) {
  P_al[i] <- P_al[i-1] - P_exact[i-1]
}

P_al1 <- P_al
for(i in 1:length(P_al)) {
  P_al1[i] <- ifelse(i-1 < n_known, NA, P_al[i] / P_al[n_known+1])
}

civs_df <- data_frame(n = 1:length(P_al),
                      N_civs = as.numeric(table(N)),
                      P_exact,
                      P_ex1,
                      P_al,
                      P_al1)

base_p <- civs_df %>% 
  na.omit() %>% 
  ggplot(aes(x = n)) +
  geom_point()

base_p + aes(y = P_al1) + scale_x_log10() + geom_line()
base_p + aes(y = P_ex1)
