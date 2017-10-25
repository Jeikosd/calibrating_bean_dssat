library(tidyverse)
library(sensitivity)

coeficientes <- read_csv(paste0('data/rangos_coeficientes.csv'))

proof <- coeficientes %>%
  mutate(data = pmap(list(min, max, salto), seq)) %>%
  select(coeficiente, data) %>%
  unnest() %>%
  group_by(coeficiente) %>%
  summarise(cantidad = n()) %>%
  select(cantidad) %>%
  magrittr::extract2(1) %>%
  prod()

### ahora generar muestras de mil

proof1 <- coeficientes %>%
  mutate(data = pmap(list(min, max, salto), seq)) %>%
  select(coeficiente, data) %>%
  mutate(muestra = map(data, sample, 1000, replace = T)) %>%
  select(coeficiente, muestra) %>%
  unnest() %>%
  group_by(coeficiente) %>%
  mutate(id = 1:length(coeficiente)) %>%
  ungroup() %>%
  spread(coeficiente, muestra) %>%
  select(-id) %>%
  as.data.frame()

proof2 <- coeficientes %>%
  mutate(data = pmap(list(min, max, salto), seq)) %>%
  select(coeficiente, data) %>%
  mutate(muestra = map(data, sample, 1000, replace = T)) %>%
  select(coeficiente, muestra) %>%
  unnest() %>%
  group_by(coeficiente) %>%
  mutate(id = 1:length(coeficiente)) %>%
  ungroup() %>%
  spread(coeficiente, muestra) %>%
  select(-id) %>%
  as.data.frame()

# sensitivity analysis

sa <- sobol(model = sobol.fun, proof1 , proof2, order = 1, nboot = 1000)
print(sa)
plot(sa)
library(boot)

n <- 100
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n)) #Random samples
x <- sobol(model = NULL, X1 = X1, X2 = X2, order = 1, nboot = 100) #Create model, first order only
r <- as.matrix(x$X,ncol=8) %*% c(1,2,3,4,5,6,7,8); #Response is proportional to factor number
tell( x, (r-mean(r))/sd(r) ); plot(x) #Standardised

n <- 100
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n)) #Random samples
x <- sobol(model = NULL, X1 = X1, X2 = X2, order = 1, nboot = 100) #Create model, first order only
r <- as.matrix(x$X,ncol=8) %*% c(1,2,3,4,5,6,7,8); #Response is proportional to factor number
tell( x, r); plot(x)


x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, nboot = 100)
x <- sobolEff(model = sobol.fun, X1 = X1, X2 = X2, nboot = 0)
print(x)
plot(x)


x <- sobolroalhs(model = NULL, factors = 3, N = 1000, order =1, nboot=0)
# X1 follows a log-normal distribution:
x$X[,1] <- qlnorm(x$X[,1])
# X2 follows a standard normal distribution:
x$X[,2] <- qnorm(x$X[,2])
# X3 follows a gamma distribution:
x$X[,3] <- qgamma(x$X[,3],shape=0.5)
# toy example
toy <- function(x){rowSums(x)}
y <- toy(x$X)
tell(x, y)
plot(x)


f<- AddFactor(name="cyclePoint",min=40,max=90)
f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
d<- AoE.LatinHypercube(100,f)

AoE.Sobol(n = 100, factors = d, o = 2, nb = 100,
          fun.doe = AoE.LatinHypercube, fun.sobol = sobolmartinez)
x <- list(x = c(1:100, NA, 1000), na.rm = TRUE, trim = 0.9)
lift_dl(mean)(x)




atantemp <- function(X, q = 100){
  n <- dim(X)[[1]]
  t <- (0:(q-1)) * (2*pi) / (q-1)
  res <- matrix(0,ncol=q,nrow=n)
  for (i in 1:n) res[i,] <- atan(X[i,1]) * cos(t) + atan(X[i,2]) * sin(t)
  return(res)
}

y0 <- atantemp(matrix(c(-7,0,7,-7,0,7),ncol=2))

n <- 100
X <- matrix(c(runif(2*n,-7,7)),ncol=2)
y <- atantemp(X)


n <- 1000
X1 <- data.frame(matrix(runif(2*n,-7,7), nrow = n))
X2 <- data.frame(matrix(runif(2*n,-7,7), nrow = n))

x11()
sa <- sobolMultOut(model=atantemp, q=100, X1, X2,
                   MCmethod="soboljansen", plotFct=T)


library(boot)
n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))
# sensitivity analysis
x <- sobolmartinez(model = sobol.fun, X1, X2, nboot = 0)
print(x)
plot(x)

sobol.fun_matrix <- function(X){
  res_vector <- sobol.fun(X)
  cbind(res_vector, 2 * res_vector)
}
x_matrix <- sobolmartinez(model = sobol.fun_matrix, X1, X2)
plot(x_matrix, y_col = 2)
title(main = "y_col = 2")
sobol.fun_array <- function(X){
  res_vector <- sobol.fun(X)
  res_matrix <- cbind(res_vector, 2 * res_vector)
  array(data = c(res_matrix, 5 * res_matrix),
        dim = c(length(res_vector), 2, 2))
}
x_array <- sobolmartinez(model = sobol.fun_array, X1, X2)
plot(x_array, y_col = 1, y_dim3 = 1)
title(main = "y_col = 2, y_dim3 = 2")


library(multisensi)
verhulst <- function(K, Y0, a, t) {
  output <- K/(1 + (K/Y0 - 1) * exp(-a * t))
  return(output)
}



T <- seq(from = 5, to = 100, by = 5)
verhulst2 <- function(X, t = T) {
  out <- matrix(nrow = nrow(X), ncol = length(t), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- verhulst(X$K[i], X$Y0[i], X$a[i], t)
  }
  out <- as.data.frame(out)
  names(out) <- paste("t", t, sep = "")
  return(out)
}


n <- 10
set.seed(1234)
X <- data.frame(K = runif(n, min = 100, max = 1000), Y0 = runif(n, min = 1,
                                                                max = 40), a = runif(n, min = 0.05, max = 0.2))
Y <- verhulst2(X)
par(cex.axis = 0.7, cex.lab = 0.8)
plot(T, Y[1, ], type = "l", xlab = "Time", ylab = "Population size",
     ylim = c(0, 1000))
for (i in 2:n) {
  lines(T, Y[i, ], type = "l", col = i)
}


verhulst.seq <- multisensi(model=verhulst2, reduction=NULL, center=FALSE,
                           design.args = list( K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2)))


plot(verhulst.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")
plot(verhulst.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")



X <- expand.grid(K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2))
Y <- verhulst2(X) ## this part can be performed outside R if necessary
verhulst.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE)
verhulst.pca <- multisensi(design=X, model=Y, reduction=basis.ACP, scale=FALSE)
summary(verhulst.pca, digits = 2)
plot(verhulst.pca, graph = 1)


m <- 10000
Xb <- data.frame(K = runif(m, min = 100, max = 1000), Y0 = runif(m, min = 1,
                                                                 max = 40), a = runif(m, min = 0.05, max = 0.2))
verhulst.seq.sobol <- multisensi(design = sobol2007, model = verhulst2,
                                 reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                 design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                 analysis.args = list(keep.outputs = FALSE))
plot(verhulst.seq.sobol, normalized = TRUE, color = terrain.colors)
