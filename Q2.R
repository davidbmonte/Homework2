
#========== Item 1: PMF exata e aproximação de Poisson ==========# 
# Parâmetros
n <- 1e7
p <- 1e-7
lambda <- n * p

# Valores possíveis de X
k <- 0:7

# PMF exata (Binomial)
pmf_bin <- dbinom(k, size = n, prob = p)

# PMF aproximada (Poisson)
pmf_pois <- dpois(k, lambda = lambda)

# Comparação em tabela
pmf_table <- data.frame(
  k = k,
  Binomial = pmf_bin,
  Poisson = pmf_pois
)

pmf_table

# Esperança e variância exatas (Binomial)
E_bin <- n * p
Var_bin <- n * p * (1 - p)

# Esperança e variância aproximadas (Poisson)
E_pois <- lambda
Var_pois <- lambda

# Resultados
stats <- data.frame(
  Distribuicao = c("Binomial", "Poisson"),
  Esperanca = c(E_bin, E_pois),
  Variancia = c(Var_bin, Var_pois)
)

stats

#========== Item 2: Valor esperado e variância ==========#
# Esperança e variância exatas (Binomial)
E_bin <- n * p
Var_bin <- n * p * (1 - p)

# Esperança e variância aproximadas (Poisson)
E_pois <- lambda
Var_pois <- lambda

# Resultados
stats <- data.frame(
  Distribuicao = c("Binomial", "Poisson"),
  Esperanca = c(E_bin, E_pois),
  Variancia = c(Var_bin, Var_pois)
)

stats

#========== Item 3:Probabilidade de você receber o prêmio ==========#

# Valores possíveis de W
w <- 0:20

# Probabilidade de receber o prêmio
prob_receber <- sum((1 / (1 + w)) * dpois(w, lambda = 1))

prob_receber

set.seed(123)

Nsim <- 1e6
W_sim <- rpois(Nsim, lambda = 1)

prob_sim <- mean(1 / (1 + W_sim))
prob_sim

#==========Item 4: Simulação e comparação visual==========#
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
