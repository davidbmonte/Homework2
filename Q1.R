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

