# Ambiente ------------------------------------------------------------------------------------
# Cargar librerías
pacman::p_load(
  tidyverse, MatchIt, sandwich, lmtest, ATE,
  causalTree, zaminfluence, modelsummary, MatchItSE,
  margins, broom, sandwich
)

# Cargar datos
df <- read.csv('../base.csv') %>% as.data.frame()

# Modelo 1 ------------------------------------------------------------------------------------
# Kit por regla administrativa
df_mod1 <- df %>% mutate(kit = kit_administrativo)

# Match
match_mod1 <- matchit(
  kit ~
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data =
    df_mod1 %>%
    select(
      hosp, kit, hombre,
      comor_graves, comor_leves,
      sint_graves, sint_mod, sint_leves,
      grupo_edad, contacto_locatel
    ) %>%
    na.omit,
  method = "cem"
)

# Extraer datos
match_data_mod1 <- match.data(match_mod1)

# Logit
glm_mod1 <- glm(
  hosp ~
    kit +
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data   = match_data_mod1,
  weight = weights,
  family = "binomial"
)

# Modelo 2 ------------------------------------------------------------------------------------
# Kit administrativo y contactados por locatel
df_mod2 <- df %>% 
  mutate(
    kit = kit_administrativo
  ) %>% 
  filter(contacto_locatel == 1)

# Match
match_mod2 <- matchit(
  kit ~
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data =
    df_mod2 %>%
    select(
      hosp, kit, hombre,
      comor_graves, comor_leves,
      sint_graves, sint_mod, sint_leves,
      grupo_edad, contacto_locatel
    ) %>%
    na.omit,
  method = "cem"
)

# Extraer datos
match_data_mod2 <- match.data(match_mod2)
glm_mod2 <- glm(
  hosp ~
    kit +
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data   = match_data_mod2,
  weight = weights,
  family = "binomial"
)

# Modelo 3 ------------------------------------------------------------------------------------
# Regla administrativa y no contactados por locatel
df_mod3 <- df %>% 
  mutate(
    kit = kit_administrativo
  ) %>% 
  filter(contacto_locatel != 1)

# Match
match_mod3 <- matchit(
  kit ~
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data =
    df_mod3 %>%
    select(
      hosp, kit, hombre,
      comor_graves, comor_leves,
      sint_graves, sint_mod, sint_leves,
      grupo_edad, contacto_locatel
    ) %>%
    na.omit,
  method = "cem"
)

# Extraer datos
match_data_mod3 <- match.data(match_mod3)

# Logit
glm_mod3 <- glm(
  hosp ~
    kit +
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data   = match_data_mod3,
  weight = weights,
  family = "binomial"
)

# Modelo 4 ------------------------------------------------------------------------------------
# Reportan haber recibido el kit
df_mod4 <- df %>%
  mutate(
    kit = kit_administrativo
  ) %>% 
  filter(kit == 1)

# Match
match_mod4 <- matchit(
  contacto_locatel ~
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data =
    df_mod4 %>%
    select(
      contacto_locatel, hosp, hombre,
      comor_graves, comor_leves,
      sint_graves, sint_mod, sint_leves,
      grupo_edad, contacto_locatel
    ) %>%
    na.omit,
  method = "cem"
)

# Match
match_data_mod4 <- match.data(match_mod4)

# Logit
glm_mod4 <- glm(
  hosp ~
    contacto_locatel +
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data   = match_data_mod4,
  weight = weights,
  family = "binomial"
)

# Modelo 5 ------------------------------------------------------------------------------------
# Reportan no haber recibido el kit
df_mod5 <- df %>% 
  mutate(
    kit = kit_administrativo
  ) %>% 
  filter(kit != 1)
match_mod5 <- matchit(
  contacto_locatel ~
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data =
    df_mod5 %>%
    select(
      contacto_locatel, hosp, hombre,
      comor_graves, comor_leves,
      sint_graves, sint_mod, sint_leves,
      grupo_edad, contacto_locatel
    ) %>%
    na.omit,
  method = "cem"
)

# Match
match_data_mod5 <- match.data(match_mod5)

# Logit
glm_mod5 <- glm(
  hosp ~
    contacto_locatel +
    hombre +
    comor_graves +
    sint_graves +
    sint_mod +
    grupo_edad,
  data   = match_data_mod5,
  weight = weights,
  family = "binomial"
)

# Modelo 6 ------------------------------------------------------------------------------------
# Ocupación hosp. entre 80 y 85%
df_mod6 <- df %>% 
  filter(
    kit_administrativo == 1 | 
      (fecha < "2020-12-28" & sint_graves + sint_leves + sint_mod > 0)
  ) %>% 
  filter(fecha >= "2020-12-15") %>% 
  filter(ocupacion >= .80 & ocupacion <= 0.85) %>% 
  mutate(kit = kit_administrativo)
match_mod6 <- matchit(
  kit ~
    hombre +
    comor_graves +
    grupo_edad,
  data =
    df_mod6 %>%
    select(
      hosp, kit, hombre,
      comor_graves, comor_leves,
      grupo_edad
    ) %>%
    na.omit,
  method = "cem"
)

# Match
match_data_mod6 <- match.data(match_mod6)

# Logit
glm_mod6 <- glm(
  hosp ~
    kit +
    hombre +
    comor_graves +
    grupo_edad,
  data   = match_data_mod6,
  weight = weights,
  family = "binomial"
)

# Modelo 7 ------------------------------------------------------------------------------------
# Kit reportado y datos del 15 de Diciembre en adelante
df_mod7 <- df %>% 
  filter(fecha >= "2020-12-15") %>% 
  filter(!is.na(kit))
kit_hosp(df_mod7)
match_mod7 <- matchit(
  kit ~
    hombre +
    comor_graves +
    grupo_edad,
  data =
    df_mod7 %>%
    select(
      hosp, kit, hombre,
      comor_graves, comor_leves,
      grupo_edad
    ) %>%
    na.omit,
  method = "cem"
)

# Match
match_data_mod7 <- match.data(match_mod7)

# Logit
glm_mod7 <- glm(
  hosp ~
    kit +
    hombre +
    comor_graves +
    grupo_edad,
  data   = match_data_mod7,
  weight = weights,
  family = "binomial"
)


# Efectos marginales --------------------------------------------------------------------------
mar_mod1 <- margins(glm_mod1, vcov = vcovHC(glm_mod1, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()

mar_mod2 <- margins(glm_mod2, vcov = vcovHC(glm_mod2, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()
mar_mod3 <- margins(glm_mod3, vcov = vcovHC(glm_mod3, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()
mar_mod4 <- margins(glm_mod4, vcov = vcovHC(glm_mod4, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()

mar_mod5 <- margins(glm_mod5, vcov = vcovHC(glm_mod5, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()
mar_mod6 <- margins(glm_mod6, vcov = vcovHC(glm_mod6, type = "HC0")) %>%
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()

mar_mod7 <- margins(glm_mod7, vcov = vcovHC(glm_mod7, type = "HC0")) %>% 
  broom::tidy()
mar %>% 
  mutate(
    low = estimate - 1.96 * std.error,
    up = estimate + 1.96 * std.error,
    term = case_when(
      term == "kit" ~ "Medical kit",
      term == "hombre" ~ "Male",
      term == "comor_graves" ~ "Severe comorbidities",
      term == "sint_graves" ~ "Severe symptoms",
      term == "sint_mod" ~ "Moderate symptoms",
      term == "grupo_edad30 a 40" ~ "31-40 years",
      term == "grupo_edad40 a 50" ~ "41-50 years",
      term == "grupo_edad50 a 60" ~ "51-60 years",
      term == "grupo_edad60 a 70" ~ "61-70 years",
      term == "grupo_edadMayores de 70" ~ "70+ years", 
      term == "ocupacion" ~ "Occupancy", 
      term == "contacto_locatel" ~ "Locatel tracking"
    )
  )%>%
  ggplot(
    aes(x = term)
  ) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "red")+
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "Marginal average effects"
  ) +
  coord_flip()