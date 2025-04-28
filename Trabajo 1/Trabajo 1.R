## Trabajo 1 ##

# Bases de Datos

library(MatchIt)
library(ggplot2)
library(tidyverse)
library(haven)
library(stargazer)
library(dplyr)
library(knitr)
library(scales)

# Cargar la base de datos
data1 <- read.csv("Data/data_tarea1_aleatorizacion.csv")
head(data1)

#*******************************************************************************
# Pregunta 1
  # Analizar el balance de covariables entre los hogares elegibles.

data_total <- data1 %>%
  filter(eligible == 1) %>% 
  group_by(treatment) %>%
  summarise(
    Health_Expenditures = mean(health_expenditures_base, na.rm = TRUE),
    Age_HH = mean(age_hh, na.rm = TRUE),
    Age_SP = mean(age_sp, na.rm = TRUE),
    Educ_HH = mean(educ_hh, na.rm = TRUE),
    Educ_SP = mean(educ_sp, na.rm = TRUE),
    Hospital_Base = mean(hospital_base, na.rm = TRUE),
    Poverty_Index = mean(poverty_index, na.rm = TRUE),
    Female_HH = mean(female_hh, na.rm = TRUE),
    Indigenous = mean(indigenous, na.rm = TRUE),
    HH_Size = mean(hhsize, na.rm = TRUE),
    Dirtfloor = mean(dirtfloor, na.rm = TRUE),
    Bathroom = mean(bathroom, na.rm = TRUE),
    Land = mean(land, na.rm = TRUE),
    Hospital_Distance = mean(hospital_distance, na.rm = TRUE),
    n = n()
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Media") %>%
  pivot_wider(names_from = treatment, values_from = Media, names_prefix = "Media_")

tabla_resumen1 <- data_total %>%
  mutate(
    Diferencia_Media = Media_1 - Media_0,  
    .by = "Variable"  
  )

  # Crear tabla 
kable(tabla_resumen1, format = "latex", booktabs = TRUE, digits = 2) 

  # Prueba formal de diferencia en medias significativas
variables <- c("health_expenditures_base", "age_hh", "age_sp", 'educ_hh', "educ_sp", 
               "hospital_base", "poverty_index", "female_hh", "indigenous", 
               "hhsize", "dirtfloor", "bathroom", "land", "hospital_distance")

  # Crear un objeto donde guardar los resultados
resultados <- list()

  # Loop sobre las variables
for (var in variables) {
  formula <- as.formula(paste(var, "~ treatment"))
  modelo <- lm(formula, data = data1, subset = eligible == 1)
  resultados[[var]] <- summary(modelo)
}

resultados[]


#*******************************************************************************
# Pregunta 2
  # Porcentaje de incumplimiento

incumplimiento <- data1 %>%
  filter(eligible == 1, treatment == 0) %>%
  summarise(incumplimiento = mean(enrolled)) %>%
  pull(incumplimiento)

porcentaje_incumplimiento = incumplimiento * 100
porcentaje_incumplimiento


data_control <- data1 %>%
  filter(eligible == 1, treatment == 0) %>%
  group_by(enrolled) %>%
  summarise(
    Health_Expenditures = mean(health_expenditures_base, na.rm = TRUE),
    Age_HH = mean(age_hh, na.rm = TRUE),
    Age_SP = mean(age_sp, na.rm = TRUE),
    Educ_HH = mean(educ_hh, na.rm = TRUE),
    Educ_SP = mean(educ_sp, na.rm = TRUE),
    Hospital_Base = mean(hospital_base, na.rm = TRUE),
    Poverty_Index = mean(poverty_index, na.rm = TRUE),
    Female_HH = mean(female_hh, na.rm = TRUE),
    Indigenous = mean(indigenous, na.rm = TRUE),
    HH_Size = mean(hhsize, na.rm = TRUE),
    Dirtfloor = mean(dirtfloor, na.rm = TRUE),
    Bathroom = mean(bathroom, na.rm = TRUE),
    Land = mean(land, na.rm = TRUE),
    Hospital_Distance = mean(hospital_distance, na.rm = TRUE),
    n = n(),
    .groups = "drop"  # <- Agregado para que no te dé warning
  ) %>%
  pivot_longer(cols = -enrolled, names_to = "Variable", values_to = "Media") %>%
  pivot_wider(names_from = enrolled, values_from = Media, names_prefix = "Media_enrolled_")

  # Restar los promedios para cada variable
tabla_resumen2 <- data_control %>%
  mutate(Diferencia_Media = Media_enrolled_1 - Media_enrolled_0)

  # Crear tabla 
kable(tabla_resumen2, format = "latex", booktabs = TRUE, digits = 2) 

  # Prueba formal de diferencia en medias significativas
comparaciones <- list()

for (var in variables) {
  formula <- as.formula(paste(var, "~ enrolled"))
  modelo <- lm(formula, data = data1 %>% filter(eligible == 1, treatment == 0))
  comparaciones[[var]] <- summary(modelo)
}

  # Ver un resultado, por ejemplo
comparaciones[]


#*******************************************************************************
# Pregunta 3
  # Comparar gasto en salud en el período post
gasto_post <- data1 %>%
  filter(eligible == 1) %>%
  group_by(treatment) %>%
  summarise(mean_gasto_salud = mean(health_expenditures_end, na.rm = TRUE), n = n())

gasto_post

##########################3333

# Primero filtrar solo los hogares elegibles
data_eligible <- subset(data1, eligible == 1)

N <- nrow(data_eligible)
N

p <- mean(data_eligible$treatment)
p

# s y c:
s <- mean(data_eligible$enrolled[data_eligible$treatment == 0])
s

c <- mean(data_eligible$enrolled[data_eligible$treatment == 1])
c

sigma <- sd(data_eligible$health_expenditures_base, na.rm = TRUE)
sigma

t_alpha <- 1.96   # Nivel de significancia del 5% (confianza 95%)
t_kappa <- 0.84   # Poder estadístico del 80%

  # Finalmente calculamos el MDE:
MDE <- (t_kappa + t_alpha) * sqrt((sigma^2 / (N * p * (1 - p)))) * (1 / (c - s))
MDE


#####################3

#*******************************************************************************
# Pregunta 4
  # a) Sin controles
modelo_a <- lm(health_expenditures_end ~ treatment, data = data1, subset = eligible == 1)
summary(modelo_a)

  # b) Incluyendo gasto rezagado
modelo_b <- lm(health_expenditures_end ~ treatment + health_expenditures_base, data = data1, subset = eligible == 1)
summary(modelo_b)

  # c) Gasto rezagado + edades
modelo_c <- lm(health_expenditures_end ~ treatment + health_expenditures_base + age_hh + age_sp, data = data1, subset = eligible == 1)
summary(modelo_c)
  
  # d) Agregando más controles
modelo_d <- lm(health_expenditures_end ~ treatment + health_expenditures_base + age_hh + age_sp + educ_hh +
                 educ_sp + female_hh + indigenous + hhsize + dirtfloor + bathroom + land + poverty_index + hospital_distance,
               data = data1, subset = eligible == 1)
summary(modelo_d)

stargazer(modelo_a, modelo_b, modelo_c, modelo_d, type = "text", title = "Impacto del programa en gasto de salud", digits = 2)

