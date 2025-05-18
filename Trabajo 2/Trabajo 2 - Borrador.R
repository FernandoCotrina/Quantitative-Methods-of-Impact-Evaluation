install.packages("haven")

library(haven)
library(MatchIt)
library(ggplot2)
library(tidyverse)
library(haven)
library(stargazer)
library(dplyr)
library(knitr)
library(scales)
library(dplyr)

# Cargar la base de datos
carpeta <- "C:/Users/josec/Desktop/Trabajos 2025-I/Evaluación de Impacto/Trabajo 2/Data"

# Especificar nombres de archivos
archivo1 <- "nsw_mixtape.dta"
archivo2 <- "cps_mixtape.dta"

# Leer los archivos
data_nsw <- read_dta(file.path(carpeta, archivo1))
data_cps <- read_dta(file.path(carpeta, archivo2))

#*******************************************************************************
  # Pregunta 1
reg1 <- lm(re78 ~ treat, data = data_nsw)
summary(reg1)

  # Pregunta 2
# Solo tomamos a los tratados
data_nsw_tratados <- data_nsw %>% 
  filter(treat == 1)

# Combinar los datos1 filtrados con datos2 (CPS)
data_nswt_cps <- bind_rows(data_nsw_tratados, data_cps)

# Regresión análoga a la pregunta 1
reg2 <- lm(re78 ~ treat, data = data_nswt_cps)
summary(reg2)

  # Pregunta 3
library(tidyr)

# Calcular estadísticas
tabla2 <- data_nswt_cps %>%
  summarise(
    Age_treat = mean(age[treat == 1], na.rm = TRUE),
    Age_control = mean(age[treat == 0], na.rm = TRUE),
    Age_se = sqrt(var(age[treat == 1])/sum(treat == 1) + var(age[treat == 0])/sum(treat == 0)),
    
    Education_treat = mean(educ[treat == 1], na.rm = TRUE),
    Education_control = mean(educ[treat == 0], na.rm = TRUE),
    Education_se = sqrt(var(educ[treat == 1])/sum(treat == 1) + var(educ[treat == 0])/sum(treat == 0)),
    
    Married_treat = mean(marr[treat == 1], na.rm = TRUE),
    Married_control = mean(marr[treat == 0], na.rm = TRUE),
    Married_se = sqrt(var(marr[treat == 1])/sum(treat == 1) + var(marr[treat == 0])/sum(treat == 0)),
    
    NoDegree_treat = mean(nodegree[treat == 1], na.rm = TRUE),
    NoDegree_control = mean(nodegree[treat == 0], na.rm = TRUE),
    NoDegree_se = sqrt(var(nodegree[treat == 1])/sum(treat == 1) + var(nodegree[treat == 0])/sum(treat == 0)),
    
    Black_treat = mean(black[treat == 1], na.rm = TRUE),
    Black_control = mean(black[treat == 0], na.rm = TRUE),
    Black_se = sqrt(var(black[treat == 1])/sum(treat == 1) + var(black[treat == 0])/sum(treat == 0)),
    
    Hispanic_treat = mean(hisp[treat == 1], na.rm = TRUE),
    Hispanic_control = mean(hisp[treat == 0], na.rm = TRUE),
    Hispanic_se = sqrt(var(hisp[treat == 1])/sum(treat == 1) + var(hisp[treat == 0])/sum(treat == 0)),
    
    RE74_treat = mean(re74[treat == 1], na.rm = TRUE),
    RE74_control = mean(re74[treat == 0], na.rm = TRUE),
    RE74_se = sqrt(var(re74[treat == 1])/sum(treat == 1) + var(re74[treat == 0])/sum(treat == 0)),
    
    RE75_treat = mean(re75[treat == 1], na.rm = TRUE),
    RE75_control = mean(re75[treat == 0], na.rm = TRUE),
    RE75_se = sqrt(var(re75[treat == 1])/sum(treat == 1) + var(re75[treat == 0])/sum(treat == 0))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  separate(Variable, into = c("Variable", "Tipo")) %>%
  pivot_wider(names_from = Tipo, values_from = Valor) %>%
  mutate(
    Diferencia = treat - control,
    Observaciones_Tratados = sum(data_nswt_cps$treat == 1),
    Observaciones_Controles = sum(data_nswt_cps$treat == 0)
  ) %>%
  select(Variable, treat, control, Diferencia, se, Observaciones_Tratados, Observaciones_Controles) %>%
  rename(
    `Media (Tratados)` = treat,
    `Media (Controles)` = control,
    `Diferencia` = Diferencia,
    `Error Estándar` = se,
    `Obs. Tratados` = Observaciones_Tratados,
    `Obs. Controles` = Observaciones_Controles
  )

# Mostrar tabla
kable(tabla2, format = "latex", digits = 2, booktabs = TRUE, caption = "Balance en covariables - datos no emparejados")

  # Pregunta 4
reg3 <- lm(re78 ~ treat + age + I(age^2) +
                 educ + I(educ^2) +
                 re74 + I(re74^2) +
                 re75 + I(re75^2) +
                 marr + black + hisp + nodegree,
               data = data_nswt_cps)

summary(reg3)

  # Pregunta 5
# Estimación del propensity score (sin hacer matching aún)
ps_model <- glm(treat ~ age + educ + marr + nodegree + re74 + re75,
                data = data_nswt_cps, family = binomial)

# Agregar el propensity score a la base
data_nswt_cps$pscore <- predict(ps_model, type = "response")

#Implementación del Soporte Común:
data_nswt_cps_f <- data_nswt_cps %>% 
  filter(pscore >= 0.05 & pscore <= 0.95)

# Histograma
ggplot(data_nswt_cps_f, aes(x = pscore, fill = factor(treat))) +
  geom_histogram(position = "identity", bins = 18, alpha = 0.6) +
  scale_fill_manual(values = c("pink", "lightblue"),
                    labels = c("Control", "Tratado")) +
  labs(title = "Propensity Score Estimado, Pre-Matching",
       x = "Propensity Score", y = "Frecuencia", fill = "Grupo") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +  
  theme_classic() 

  # Pregunta 6
#Matching
psm_model <- matchit(treat ~ age + educ 
                     +  marr + nodegree + re74 + re75,
                     data = data_nswt_cps, method = "nearest", distance = data_nswt_cps$pscore)

matched <- match.data(psm_model)

# Histograma (si soporte común)
ggplot(matched, aes(x = pscore, fill = factor(treat))) +
  geom_histogram(position = "identity", bins = 18, alpha = 0.6) +
  scale_fill_manual(values = c("pink", "lightblue"),
                    labels = c("Control", "Tratado")) +
  labs(title = "Propensity Score Estimado, Post-Matching",
       x = "Propensity Score", y = "Frecuencia", fill = "Grupo") + 
  scale_y_continuous(labels = label_number(accuracy = 1)) +  
  theme_classic() 


  # Pregunta 7
# Calcular estadísticas
tabla3 <- matched %>%
  summarise(
    Age_treat = mean(age[treat == 1], na.rm = TRUE),
    Age_control = mean(age[treat == 0], na.rm = TRUE),
    Age_se = sqrt(var(age[treat == 1])/sum(treat == 1) + var(age[treat == 0])/sum(treat == 0)),
    
    Education_treat = mean(educ[treat == 1], na.rm = TRUE),
    Education_control = mean(educ[treat == 0], na.rm = TRUE),
    Education_se = sqrt(var(educ[treat == 1])/sum(treat == 1) + var(educ[treat == 0])/sum(treat == 0)),
    
    Married_treat = mean(marr[treat == 1], na.rm = TRUE),
    Married_control = mean(marr[treat == 0], na.rm = TRUE),
    Married_se = sqrt(var(marr[treat == 1])/sum(treat == 1) + var(marr[treat == 0])/sum(treat == 0)),
    
    NoDegree_treat = mean(nodegree[treat == 1], na.rm = TRUE),
    NoDegree_control = mean(nodegree[treat == 0], na.rm = TRUE),
    NoDegree_se = sqrt(var(nodegree[treat == 1])/sum(treat == 1) + var(nodegree[treat == 0])/sum(treat == 0)),
    
    Black_treat = mean(black[treat == 1], na.rm = TRUE),
    Black_control = mean(black[treat == 0], na.rm = TRUE),
    Black_se = sqrt(var(black[treat == 1])/sum(treat == 1) + var(black[treat == 0])/sum(treat == 0)),
    
    Hispanic_treat = mean(hisp[treat == 1], na.rm = TRUE),
    Hispanic_control = mean(hisp[treat == 0], na.rm = TRUE),
    Hispanic_se = sqrt(var(hisp[treat == 1])/sum(treat == 1) + var(hisp[treat == 0])/sum(treat == 0)),
    
    RE74_treat = mean(re74[treat == 1], na.rm = TRUE),
    RE74_control = mean(re74[treat == 0], na.rm = TRUE),
    RE74_se = sqrt(var(re74[treat == 1])/sum(treat == 1) + var(re74[treat == 0])/sum(treat == 0)),
    
    RE75_treat = mean(re75[treat == 1], na.rm = TRUE),
    RE75_control = mean(re75[treat == 0], na.rm = TRUE),
    RE75_se = sqrt(var(re75[treat == 1])/sum(treat == 1) + var(re75[treat == 0])/sum(treat == 0))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  separate(Variable, into = c("Variable", "Tipo")) %>%
  pivot_wider(names_from = Tipo, values_from = Valor) %>%
  mutate(
    Diferencia = treat - control,
    Observaciones_Tratados = sum(matched$treat == 1),
    Observaciones_Controles = sum(matched$treat == 0)
  ) %>%
  select(Variable, treat, control, Diferencia, se, Observaciones_Tratados, Observaciones_Controles) %>%
  rename(
    `Media (Tratados)` = treat,
    `Media (Controles)` = control,
    `Diferencia` = Diferencia,
    `Error Estándar` = se,
    `Obs. Tratados` = Observaciones_Tratados,
    `Obs. Controles` = Observaciones_Controles
  )

# Mostrar tabla
kable(tabla3, format = "latex", digits = 2, booktabs = TRUE, caption = "Balance en covariables - datos no emparejados")
  

  # Pregunta 8
reg4 <- lm(re78 ~ treat, data = matched)
summary(reg4)

  # Pregunta 9
ps_model <- glm(treat ~ age + educ + marr + nodegree + re74 + re75,
                data = data_nswt_cps, family = binomial)

data_nswt_cps$pscore <- predict(ps_model, type = "response")
data_nswt_cps$w <- ifelse(data_nswt_cps$treat == 1, 1, data_nswt_cps$pscore / (1 - data_nswt_cps$pscore))

reg5 <- lm(re78 ~ treat, data = data_nswt_cps, weights = w)
summary(reg5)

  # Resultados Generales
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", title = "Impacto del programa en gasto de salud", digits = 2)
