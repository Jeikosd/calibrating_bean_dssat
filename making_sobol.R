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



X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n)) #Random samples
x <- sobol(model = NULL, X1 = X1, X2 = X2, order = 1, nboot = 100) #Create model, first order only
r <- as.matrix(x$X,ncol=8) %*% c(1,2,3,4,5,6,7,8); #Response is proportional to factor number
tell( x, (r-mean(r))/sd(r) ); plot(x) #Standardised

