# Librerías
library(tidyverse)
library(MatchIt)

# Cargar datos
data("lalonde")
lalonde %>% head()

# Conteo de tratamiento
lalonde %>% group_by(treat) %>% summarise(count = n())

# Inicializar match
match <- matchit(formula = treat ~ age + educ + married + re75,
                 data = lalonde, method = 'nearest', distance = 'glm',
                 link = 'logit', estimad = 'ATT')

# Balance antes de emparejar
match %>% summary() %>% pluck('sum.all') %>% round(2)

# Balance después de emparejar
match %>% summary() %>% pluck('sum.matched') %>% round(2)

# Conteo de observaciones tras emparejar
match %>% summary() %>% pluck('nn')

# Extraer datos emparejados de `match`
df <- match.data(match)

# Plot
plot(match, type = "qq", interactive = FALSE,
     which.xs = c("age", "married", "re75"))

# Efecto estimado sin emparejar
t0 <- t.test(x = lalonde %>% filter(treat == 0) %>% pull(re78),
             y = lalonde %>% filter(treat == 1) %>% pull(re78))

# Efecto estimado tras emparejar
t1 <- t.test(x = df %>% filter(treat == 0) %>% pull(re78),
             y = df %>% filter(treat == 1) %>% pull(re78))

# Visualizar efectos estimados
t0$estimate %>% diff() # Sin matching es negativo
t1$estimate %>% diff() # Con matching es positivo
