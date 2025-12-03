# ###########################################################
  # Trabajo 2 - Informática para Economistas
# ###########################################################
  
data <- "G:/Trabajo 2/Data/enaho01a-2020-500.dta"

library(tidyverse)
library(haven)

enaho <- read_dta(data)

# Pregunta a
data_filtrada <- enaho %>%
  select(p208a, p523, p524a1, ubigeo) %>%
  drop_na()

# Pregunta b
logsalario <- data_filtrada %>%
  mutate(
    salario_mensual = case_when(
      p523 == 1 ~ p524a1 * 30, # Diario
      p523 == 2 ~ p524a1 * 4,  # Semanal
      p523 == 3 ~ p524a1 * 2,  # Quincenal
      p523 == 4 ~ p524a1 * 1,  # Mensual
      TRUE ~ NA_real_
    ),
    log_salario = log(salario_mensual)
  )

# Pregunta c
prom_ubigeo <- logsalario %>%
  group_by(ubigeo) %>%
  summarise(
    salario_promedio = mean(salario_mensual, na.rm = TRUE),
    edad_promedio = mean(p208a, na.rm = TRUE)
  )

# Pregunta d
library(data.table)

enaho_data <- as.data.table(read_dta(data))

# Pregunta d.a
enaho_data_filtrada <- enaho_data[!is.na(p208a) & !is.na(p523) & !is.na(p524a1) & !is.na(ubigeo),
                              .(p208a, p523, p524a1, ubigeo)]

# Pregunta d.b
enaho_data_filtrada[, salario_mensual := fifelse(p523 == 1, p524a1 * 30,
                                               fifelse(p523 == 2, p524a1 * 4,
                                                       fifelse(p523 == 3, p524a1 * 2,
                                                               fifelse(p523 == 4, p524a1 * 1, NA_real_))))]

enaho_data_filtrada[, log_salario := log(salario_mensual)]


# Pregunta d.c
e_data_promedio_ubigeo <- enaho_data_filtrada[, .(
  salario_promedio = mean(salario_mensual, na.rm = TRUE),
  edad_promedio = mean(p208a, na.rm = TRUE)
), by = ubigeo]


