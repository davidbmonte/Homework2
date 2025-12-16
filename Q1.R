# Parâmetros da binomial
n <- 50
p <- 0.7

# Suporte da variável aleatória
x <- 0:n

# PMF
pmf <- dbinom(x, size = n, prob = p)

# Gráfico da PMF
plot(
  x, pmf,
  type = "h",
  lwd = 2,
  xlab = "x",
  ylab = "P(X = x)",
  main = "Função Massa de Probabilidade"
)
points(x, pmf, pch = 16)

# CDF
cdf <- pbinom(x, size = n, prob = p)

# Gráfico da CDF
plot(
  x, cdf,
  type = "s",
  lwd = 2,
  xlab = "x",
  ylab = "P(X ≤ x)",
  main = "Função de Distribuição Acumulada"
)

# Valor esperado
m <- n * p

# Variância
Var <- n * p * (1 - p)

# Desvio padrão
SD <- sqrt(Var)

# Resultados
m
Var
SD

# (a) P(X >= 20)
# Usando o complemento 
P_a <- 1 - pbinom(19, size = n, prob = p)

# (b) P(30 < X < 43)
# Equivalente a P(31 <= X <= 42)
P_b <- pbinom(42, size = n, prob = p) - pbinom(30, size = n, prob = p)

# (c) P(X = 31)
P_c <- dbinom(31, size = n, prob = p)

############################
# Resultados
P_a
P_b
P_c
