# Parâmetros do problema
n <- 1e7          # número de visitantes por dia
p <- 1e-7         # probabilidade individual
lambda <- n * p   # parâmetro da Poisson
Nsim <- 200000    # número de simulações

set.seed(123)

# Simulações
X_bin <- rbinom(Nsim, size = n, prob = p)
X_pois <- rpois(Nsim, lambda = lambda)

# Valores possíveis (foco nos mais prováveis)
k <- 0:7

# Frequências empíricas
freq_bin <- table(factor(X_bin, levels = k)) / Nsim
freq_pois <- table(factor(X_pois, levels = k)) / Nsim

# PMF teórica da Poisson
pmf_pois <- dpois(k, lambda)

# Gráfico comparativo
barplot(
  rbind(freq_bin, pmf_pois),
  beside = TRUE,
  names.arg = k,
  col = c("steelblue", "orange"),
  legend.text = c("Binomial (simulação)", "Poisson (teórica)"),
  ylim = c(0, max(freq_bin, pmf_pois) * 1.2),
  xlab = "Número de vencedores por dia",
  ylab = "Probabilidade / Frequência relativa",
  main = "Comparação: Binomial vs Aproximação de Poisson"
)
