# ---------------------------------------------------------------------------- #
# Paso 0: Limpiar espacio de trabajo ----
# ---------------------------------------------------------------------------- #

rm(list = ls())


# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 1: Cargar paquetes necesarios ----
# ---------------------------------------------------------------------------- #

# Instalar el paquete pacman que permite instalar y/o cargar los paquetes necesarios
if (!require("pacman")) install.packages("pacman", repos = 'http://cran.us.r-project.org')

# Instalar o cargar los paquetes necesarios
pacman::p_load("dplyr", "readxl", "ggplot2", "knitr", "modelsummary",
               "equatiomatic", "magrittr", "furrr", "papeR", "purrr")

options(bitmapType="cairo")

# Agregar funciones propias
p_report <- function(x){
  
  # create an object "e" which contains x, the p value you are reporting,
  # rounded to 3 decimal places
  
  e <- round(x, digits = 3)
  
  # the next two lines of code prints "< .001" if x is indeed less than .001
  
  if (x < 0.001)
    print(paste0("<", " ", ".001"))
  
  # if x is greater than .001, the code below prints the object "e"
  # with an "=" sign, and with the preceeding zero removed
  
  else
    print(
      paste0("=",
             " ",
             sub("^(-?)0.", "\\1.", sprintf("%.3f",e))))
  
}

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 2: Lectura de datos ----
# ---------------------------------------------------------------------------- #

raw_data <- readxl::read_xlsx("./data/rocketfuel_data.xlsx")

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 3: Data wrangling ----
# ---------------------------------------------------------------------------- #

data <- raw_data %>%
  # Clases de grupo de usuarios
  dplyr::mutate(user_group = factor(test, levels = 0:1, labels = c("Control", "Expuesto"))) %>%
  # Conversión de usuarios
  dplyr::mutate(bought_bag = factor(converted , levels = 0:1, labels = c("No", "Si"))) %>%
  # Convertir numero de dia en dia de la semana
  dplyr::mutate(dia_mas_impresiones = factor(mode_impr_day, levels=1:7,
                                             labels=c("Lunes", "Martes", "Miércoles",
                                                      "Jueves", "Viernes", "Sábado", "Domingo"))) %>%
  # Convertir hora del día en factor
  dplyr::mutate(hora_dia = factor(mode_impr_hour))

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 3: Análisis exploratorio ----
# ---------------------------------------------------------------------------- #

# Cantidad de usuarios
usuarios <- nrow(data)
# Cantidad de usuarios expuestos a la campaña
expuestos <- data %>%
  dplyr::filter(test == 1) %>%
  dplyr::pull(test)
# Cantidad de usuarios de control
control <- data %>%
  dplyr::filter(test == 0) %>%
  dplyr::pull(test)

# Resumen de variable cuantitativas
cuanti_summarise <- papeR::summarize(data %>%
                           dplyr::select(-user_id), type = "numeric")

# Distribucion de la cantidad de impresiones

test.labs <- c("Control", "Expuesto")
names(test.labs) <- c("0", "1")

densidad_impresiones <- ggplot2::ggplot(data = data, ggplot2::aes(x = tot_impr)) +
  ggplot2::geom_density() +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~test, labeller = labeller(test = test.labs)) +
  ggplot2::labs(x = 'Cantidad de impresiones (log)', y = "Cantidad de usuarios") +
  ggplot2::theme_bw()

conversion_dia_semana <- data %>%
  dplyr::group_by(dia_mas_impresiones) %>%
  dplyr::summarize(conversion_rate = mean(converted)) %>%
  dplyr::rename(dia = dia_mas_impresiones, tasa_conversion = conversion_rate)

conversion_dia_semana_grafico <- ggplot2::ggplot(data = conversion_dia_semana, ggplot2::aes(x = dia, y = tasa_conversion)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = "Día de la semana", y = "Tasa de conversión") +
  ggplot2::theme_bw()

conversion_hora_dia <- data %>%
  dplyr::group_by(mode_impr_hour) %>%
  dplyr::summarize(conversion_rate = mean(converted)) %>%
  dplyr::rename(hora = mode_impr_hour, tasa_conversion = conversion_rate)

conversion_hora_dia_grafico <- ggplot2::ggplot(data = conversion_hora_dia, ggplot2::aes(x = factor(hora), y = tasa_conversion)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = "Hora del día", y = "Tasa de conversión") +
  ggplot2::theme_bw()
# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Diferencia de medias ----
# ---------------------------------------------------------------------------- #

# Evaluación de la campaña
# Question 1 - Was the advertising campaign effective? 
# Did additional consumers convert as a result of the ad campaign?

# Se realiza un t-test para evaluar si la tasa de conversion es diferente
# entre los grupos expuestos a la campaña o no. 

diferencia_medias <- data %>%
  dplyr::select(converted, user_group) %>%
  t.test(converted~user_group, alternative = "less", var.equal = TRUE, data = .)

# Formula del modelo lineal
formula_modelo <- formula("converted~user_group")

# Diferencia de medias
diferencia_medias <- data %>%
  dplyr::select(converted, user_group) %>%
  t.test(formula_modelo, alternative = "less", var.equal = TRUE, data = .)


# Semilla para el remuestreo
set.seed(1234) 

# Para el remuestreo se utiliza el paquete rsample. Forma parte de la suite
# de tidyverse y permite utilizar la funcionalidad de los paquetes relacionados

# Se crean N remuestreos del dataset original
# TO DO: Elegir in R lo suficientemente grande, se usa 100 solo para pruebas
bootstrapped_samples_diferencia <- rsample::bootstraps(data, times = 100)


# Definición de función para realizar la prueba
t.test.run <- function(splits, ...) {
  # se `analysis` para extraer el data frame correspondiente a 
  # cada muestra
  t.test(..., alternative = "less", var.equal = TRUE, data = rsample::analysis(splits)) %>%
    broom::tidy() # tidy permite extraer los coeficientes ajustados 
}

# Se itera sobre cada una de los remuestreos para  realizar la prueba y 
# extraer los resultados
plan(multisession)
bootstrapped_samples_diferencia$model <- furrr::future_map(
  .x = bootstrapped_samples_diferencia$splits, 
  .f = t.test.run, formula_modelo)

# Extraer los coeficientes ajustados para cada muestra y convertir en un 
# data frame para poder graficar
t.test.results <- bootstrapped_samples_diferencia %>%
  dplyr::select(-splits) %>%
  # Apilar los tibbles en la variable model
  tidyr::unnest(model) %>%
  # Seleccionar las variables de interes
  dplyr::select(id, estimate, estimate1, estimate2, statistic,  p.value, parameter, conf.low, conf.high) 


histograma_coeficientes_diferencia <- ggplot2::ggplot(data = t.test.results, ggplot2::aes(x = estimate)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(data = t.test.results, aes(xintercept = mean(conf.low)), col = "red") + 
  ggplot2::geom_vline(data = t.test.results, aes(xintercept = mean(conf.high)), col = "red") + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Coeficiente") + ggplot2::ylab("Cantidad")


# No. of Converted users in Exposed Group
usuarios_expuestos_convertidos <- data %>%
  dplyr::filter(test == 1, converted == 1) %>%
  nrow(.)
# Percentage of converted users in Exposed Group
(usuarios_expuestos_convertidos/length(expuestos))*100
# No. of users in Control Group
# No. of Converted users in Control Group
usuarios_control_convertidos <- data %>%
  dplyr::filter(test == 0, converted == 1) %>%
  nrow(.)
usuarios_expuesto_convertidos_campaña <- data %>%
  dplyr::group_by(test) %>%
  dplyr::summarise(tasa_conversion = mean(converted)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(usuarios_extra = diff(tasa_conversion)*length(expuestos)) %>%
  dplyr::pull(usuarios_extra)
# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Modelo de regresión lineal PLM ----
# ---------------------------------------------------------------------------- #

# Formula del modelo lineal
formula_modelo <- formula("converted~user_group")

# Regresion por MCO
modelo_regresion_basico <- lm(formula_modelo, data = data) 

# Promediode los residuos == 0 
mean(modelo_regresion_basico$residuals)

# Regresion por MCO haciendo bootstrap para conocer la distribución de 
# los coeficientes del modelo

# Semilla para el remuestreo
set.seed(1234) 

# Para el remuestreo se utiliza el paquete rsample. Forma parte de la suite
# de tidyverse y permite utilizar la funcionalidad de los paquetes relacionados

# Se crean N remuestreos del dataset original
# TO DO: Elegir in R lo suficientemente grande, se usa 100 solo para pruebas
bootstrapped_samples_basico <- rsample::bootstraps(data, times = 100)


# Definición de función para ajustar el modelo lineal
lm_coefs <- function(splits, ...) {
  # se `analysis` para extraer el data frame correspondiente a 
  # cada muestra
  lm(..., data = rsample::analysis(splits)) %>%
    broom::tidy() # tidy permite extraer los coeficientes ajustados 
}

# Se itera sobre cada una de los remuestreos para el ajuste del modelo 
# lineal y la extracción de los coeficientes
plan(multisession)
bootstrapped_samples_basico$model <- furrr::future_map(
  .x = bootstrapped_samples_basico$splits, 
  .f = lm_coefs, formula_modelo)

# Extraer los coeficientes ajustados para cada muestra y convertir en un 
# data frame para poder graficar
lm_coef_basico <- bootstrapped_samples_basico %>%
  dplyr::select(-splits) %>%
  # Apilar los tibbles en la variable model
  tidyr::unnest(model) %>%
  # Seleccionar las variables de interes
  dplyr::select(id, term, estimate, std.error, statistic,  p.value) %>%
  dplyr::mutate(term = if_else(term == 'user_groupExpuesto', 'Expuesto', term)) %>%
  dplyr::mutate(term = factor(term, levels = c('(Intercept)', 'Expuesto')))

# Intervalos percentiles
p_ints_basico <- rsample::int_pctl(bootstrapped_samples_basico, model)%>%
  dplyr::mutate(term = if_else(term == 'user_groupExpuesto', 'Expuesto', term)) 

histograma_coeficientes_basico <- ggplot2::ggplot(data = lm_coef_basico, ggplot2::aes(x = estimate)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~term, scales = 'free') +
  ggplot2::geom_vline(data = p_ints_basico, aes(xintercept = .lower), col = "red") + 
  ggplot2::geom_vline(data = p_ints_basico, aes(xintercept = .upper), col = "red") + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Coeficiente") + ggplot2::ylab("Cantidad")

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Modelo de regresión logistica  ----
# ---------------------------------------------------------------------------- #

# Independiente de la condicion del producto

# Formula del modelo lineal
formula_modelo <- formula("converted~user_group")

# Regresion por MCO
modelo_regresion_logistica <- glm(converted~user_group , family=binomial, data = data)

# Semilla para el remuestreo
set.seed(1234) 

# Para el remuestreo se utiliza el paquete rsample. Forma parte de la suite
# de tidyverse y permite utilizar la funcionalidad de los paquetes relacionados

# Se crean N remuestreos del dataset original
# TO DO: Elegir in R lo suficientemente grande, se usa 100 solo para pruebas
plan(multisession)
bootstrapped_samples_logistico <- rsample::bootstraps(data, times = 100)


# Definición de función para ajustar el modelo lineal
glm_coefs <- function(splits, ...) {
  # se `analysis` para extraer el data frame correspondiente a 
  # cada muestra
  lm(..., data = rsample::analysis(splits)) %>%
    broom::tidy() # tidy permite extraer los coeficientes ajustados 
}

# Se itera sobre cada una de los remuestreos para el ajuste del modelo 
# lineal y la extracción de los coeficientes
plan(multisession)
bootstrapped_samples_logistico$model <- furrr::future_map(
  .x = bootstrapped_samples_basico$splits, 
  .f = lm_coefs, formula_modelo)

# Extraer los coeficientes ajustados para cada muestra y convertir en un 
# data frame para poder graficar
glm_coef_basico <- bootstrapped_samples_logistico %>%
  dplyr::select(-splits) %>%
  # Apilar los tibbles en la variable model
  tidyr::unnest(model) %>%
  # Seleccionar las variables de interes
  dplyr::select(id, term, estimate, std.error, statistic,  p.value) %>%
  dplyr::mutate(term = if_else(term == 'user_groupExpuesto', 'Expuesto', term)) %>%
  dplyr::mutate(term = factor(term, levels = c('(Intercept)', 'Expuesto')))

# Intervalos percentiles
p_ints_logistico <- rsample::int_pctl(bootstrapped_samples_logistico, model)%>%
  dplyr::mutate(term = if_else(term == 'user_groupExpuesto', 'Expuesto', term)) 

histograma_coeficientes_logistico <- ggplot2::ggplot(data = glm_coef_basico, ggplot2::aes(x = estimate)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~term, scales = 'free') +
  ggplot2::geom_vline(data = p_ints_basico, aes(xintercept = .lower), col = "red") + 
  ggplot2::geom_vline(data = p_ints_basico, aes(xintercept = .upper), col = "red") + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Coeficiente") + ggplot2::ylab("Cantidad")

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso : Rentabilidad de la campaña -----
# ---------------------------------------------------------------------------- #

# Renta para la firma
# La firma estima que el valor de un cliente convertido es de 40 USD. DATO
valor_cliente_convertido <- 40
ingresos_campana <- usuarios_expuesto_convertidos_campaña*valor_cliente_convertido

# Costos de la campana
# El costo por mil impresiones es de USD 9. DATO
cpm <- 9
costo_campana <- sum(data$tot_impr)/1000*cpm

# Costo de oportunidad
usuarios_control_convertidos_potenciales <- data %>%
  dplyr::group_by(test) %>%
  dplyr::summarise(tasa_conversion = mean(converted)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(usuarios_extra = diff(tasa_conversion)*length(control)) %>%
  dplyr::pull(usuarios_extra)

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso : Efectividad de la campaña ----
# ---------------------------------------------------------------------------- #

densidad_conversion_impresiones <- data %>%
  dplyr::filter(converted == 1) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = tot_impr)) +
  ggplot2::geom_density() +
  #ggplot2::geom_histogram(bins = 30L) +
  ggplot2::labs(x = "Impresiones totales", y = "Usuarios convertidos") +
  ggplot2::theme_bw()

densidad_conversion_impresiones_250 <- data %>%
  dplyr::filter(converted == 1) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = tot_impr, color = user_group)) +
  ggplot2::scale_color_brewer(name = "Grupo", palette = 'Set2') +
  ggplot2::geom_density() +
  #ggplot2::geom_histogram(bins = 30L) +
  ggplot2::labs(x = "Impresiones totales", y = "Usuarios convertidos") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom') +
  ggplot2::xlim(c(0, 250))

# Categorizacion de la variable impresiones
# Etiquetas de las clases
etiquetas_bins <- c("0-25", "26-50", "51-75", "76-100", "101-125", "126-150", "151-175", "176-200",
                    "201-225", "226-250", "251+")
# Quiebres de las clases
quiebres <- c(seq(0, 251, by = 25), Inf)

data <- data %>%
  dplyr::mutate(clases_impr = factor(cut(tot_impr, 
                                         breaks = quiebres, 
                                         labels = etiquetas_bins,
                                         include.lowest = T)))

# Resumen de metricas por clase de impresiones
resumen_clases <- data %>%
  dplyr::group_by(clases_impr, user_group) %>%
  dplyr::summarise(usuarios = n(), 
                   conversion = sum(converted), 
                   total_impresiones = sum(tot_impr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tasa_conversion = (conversion / usuarios) * 100,
                impresiones_conversion = total_impresiones / conversion)

# Histograma de conversiones totales
histograma_conversiones_totales <- ggplot2::ggplot() +
  ggplot2::geom_bar(data = resumen_clases, ggplot2::aes(x = clases_impr, weight = conversion), fill = "Steelblue") +
  ggplot2::geom_line(data = resumen_clases, aes(x = clases_impr, y = impresiones_conversion), size = 1, color = "black", group = 1) +
  ggplot2::facet_wrap(~user_group) +
  ggrepel::geom_label_repel(data = resumen_clases, aes(x = clases_impr, y = impresiones_conversion, label = round(impresiones_conversion, 0)), nudge_x = 0.35, size = 4) +
  ggplot2::scale_y_continuous(name = "Conversiones", sec.axis = sec_axis(~ . * 1, name = "Impresiones por conversión")) +
  ggplot2::labs(x = "Clases de impresiones") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))

# Tasa de conversion por clase de impresion
evolucion_tasa_conversion <- ggplot2::ggplot(resumen_clases, aes(x = clases_impr, y = tasa_conversion)) +
  ggplot2::geom_line(size = 1, color = "Steelblue", group = 1) +
  ggplot2::facet_wrap(~user_group) +
  ggrepel::geom_label_repel(aes(label = round(tasa_conversion, 1)), nudge_x = 0.35, size = 4) +
  ggplot2::labs(x = "Clases de impresiones", y = "Tasa de conversión") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))


# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso : Efecto del día de la semana/hora del día ----
# ---------------------------------------------------------------------------- #

resumen_clases_dia <- data %>%
  dplyr::group_by(dia_mas_impresiones, user_group) %>%
  dplyr::summarise(usuarios = n(), 
                   conversion = sum(converted), 
                   total_impresiones = sum(tot_impr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tasa_conversion = (conversion / usuarios) * 100,
                impresiones_conversion = total_impresiones / conversion)

efecto_dia_semana_tasa <-  ggplot2::ggplot(data = resumen_clases_dia, ggplot2::aes(x = dia_mas_impresiones, y = tasa_conversion, fill = user_group)) +
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::scale_fill_brewer(palette = 'Paired', name = "Grupo") +
  #ggplot2::geom_text(stat = "count", aes(label=after_stat(count)), vjust = "outward") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Días de la semana", y = 'Tasa de conversión') +
  ggplot2::theme(legend.position = 'bottom')

efecto_dia_semana_efectividad <-  ggplot2::ggplot(data = resumen_clases_dia, ggplot2::aes(x = dia_mas_impresiones, y = impresiones_conversion, fill = user_group)) +
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::scale_fill_brewer(palette = 'Paired', name = "Grupo") +
  #ggplot2::geom_text(stat = "count", aes(label=after_stat(count)), vjust = "outward") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Días de la semana", y = 'Impresiones por conversión') +
  ggplot2::theme(legend.position = 'bottom')

# Hora del día
resumen_clases_hora <- data %>%
  dplyr::filter(mode_impr_hour > 8) %>%
  dplyr::group_by(hora_dia, user_group) %>%
  dplyr::summarise(usuarios = n(), 
                   conversion = sum(converted), 
                   total_impresiones = sum(tot_impr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tasa_conversion = (conversion / usuarios) * 100,
                impresiones_conversion = total_impresiones / conversion,
                impresiones_conversion = if_else(conversion == 0, 0, impresiones_conversion))

efecto_hora_tasa <-  ggplot2::ggplot(data = resumen_clases_hora, ggplot2::aes(x = hora_dia, y = tasa_conversion, fill = user_group)) +
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::scale_fill_brewer(palette = 'Paired', name = "Grupo") +
  #ggplot2::geom_text(stat = "count", aes(label=after_stat(count)), vjust = "outward") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Días de la semana", y = 'Tasa de conversión') +
  ggplot2::theme(legend.position = 'bottom')

efecto_hora_efectividad <-  ggplot2::ggplot(data = resumen_clases_hora, ggplot2::aes(x = hora_dia, y = impresiones_conversion, fill = user_group)) +
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::scale_fill_brewer(palette = 'Paired', name = "Grupo") +
  #ggplot2::geom_text(stat = "count", aes(label=after_stat(count)), vjust = "outward") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Días de la semana", y = 'Impresiones por conversión') +
  ggplot2::theme(legend.position = 'bottom')

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso : Modelo de regresión ----
# ---------------------------------------------------------------------------- #

# Modelo con interacción 
ajuste_lineal_interaccion <- lm(converted~tot_impr+I(tot_impr^2) + 
      dia +
      hora +
      tot_impr*dia +
        dia*hora, 
    data = data %>%
      dplyr::rename(dia = dia_mas_impresiones, hora = hora_dia))

# Modelo sin interacción 
ajuste_lineal_basico <- lm(converted~tot_impr+I(tot_impr^2) + 
                      dia +
                      hora, 
                    data = data %>%
                      dplyr::rename(dia = dia_mas_impresiones, hora = hora_dia))


summary(ajuste_lineal_basico)

coeficiente_hora_dia <- broom::tidy(ajuste_lineal_basico) %>%
  dplyr::filter(stringr::str_detect(term, 'hora'), !stringr::str_detect(term, ':')) %>%
  dplyr::mutate(term = as.numeric(stringr::str_remove(term, "hora"))) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = factor(term), y = estimate, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Hora del día", y = "Coef.")

coeficiente_dia_semana <- broom::tidy(ajuste_lineal_basico) %>%
  dplyr::filter(stringr::str_detect(term, 'dia'), !stringr::str_detect(term, ':')) %>%
  dplyr::mutate(term = stringr::str_remove(term, "dia"),
                term = factor(term, levels = c("Martes", "Miércoles", "Jueves", 
                              "Viernes", "Sábado", "Domingo"))) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = factor(term), y = estimate, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed') +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Hora del día", y = "Coef.")

efecto_maringal_impresiones <- data %>%
  dplyr::mutate(efecto_impresiones = 0.00124 - (2*0.00000122)*tot_impr) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = tot_impr, y = efecto_impresiones)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Total de impresiones", y = "Efecto marginal")

  
 
# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Creación de intervalos de confianza de la media por tratamiento ----
# ---------------------------------------------------------------------------- #

# Funcion para el calculo de la media
meanfun <- function(data, i){
  d <- data[i]
  return(mean(d))   
}

# Definir semilla
set.seed(1234)

combinaciones <- data %>%
  dplyr::select(user_group) %>%
  dplyr::distinct() %>%
  dplyr::arrange(user_group) %>%
  dplyr::mutate(seed = runif(2, min = 1000, max = 9999))

bootstrap_media <- purrr::map(
  .x = combinaciones$user_group,
  .f = function(grupo) {
    
    
    # Semilla para cada iteracion
    combinaciones %>%
      dplyr::filter(user_group == grupo) %>%
      dplyr::pull(seed) %>%
      set.seed()
    
    # Seleccion y filtrado de la variable de interes
    converted <- data %>%
      dplyr::filter(user_group == grupo) %>%
      dplyr::pull(converted) 
    
    # Remuestreo de la media de ventas
    boot_media <- boot::boot(data = converted, statistic = meanfun, R = 1000)
    
    # Creacion de variable para nombrar la lista resultante
    nombre_lista <- paste("Grupo:" , grupo)
    # Crear objeto para guardar resultados
    resultados <- list(boot_media) 
    # Renombrar objeto resultado
    resultados %<>% purrr::set_names(nombre_lista)
    
  }
) %>% unlist(., recursive = FALSE) # Eliminar un nivel de la lista. 


bootstrap_media_conf <- purrr::map_dfr(
  .x = unique(names(bootstrap_media)),
  .f = function(combinacion) {
    
    boot_media <- bootstrap_media[[combinacion]]
    
    broom::tidy(boot_media, conf.int = T) %>%
      dplyr::mutate(grupo = stringr::word(combinacion, 2)) %>%
      dplyr::select(grupo, media = statistic, sesgo = bias, error = std.error,
                    conf.inf = conf.low, conf.sup = conf.high)
    
  }
)

intervalos_confianza_plataforma_momento <- ggplot(data = bootstrap_media_conf, ggplot2::aes(x = grupo, y = media)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_discrete("Grupo", labels = c("Control", "Expuesto")) +
  ggplot2::geom_errorbar(aes(ymin=conf.inf, ymax=conf.sup), width = .1) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Media")
# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# ---- Paso n: Generar reporte ----
# ---------------------------------------------------------------------------- #

# Generar informe en PDF
output.dir <- glue::glue("{getwd()}/output/")
rmarkdown::render(
  input = "./output/reporte.Rmd",
  output_file = "reporte.pdf",
  output_dir = output.dir
)

# -----------------------------------------------------------------------------
