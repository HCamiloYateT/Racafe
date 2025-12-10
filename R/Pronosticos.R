#' Imputar valores faltantes en una serie temporal
#'
#' Aplica distintos métodos de imputación sobre una serie temporal representada como
#' `tsibble`. Permite interpolar, reemplazar por estadísticas de tendencia central,
#' realizar rellenado hacia adelante o atrás, utilizar splines o asignar valores
#' constantes. Tras la imputación se revisa si persisten valores faltantes y se
#' realiza un rellenado bidireccional como paso de seguridad.
#'
#' @param ts_data Un objeto `tsibble` con las columnas `fecha` y `valor`.
#' @param metodo_imputacion Cadena que define el método de imputación. Valores
#'   válidos: "interpolacion", "media", "mediana", "percentil", "forward_fill",
#'   "backward_fill", "spline", "constante" o "cero".
#' @param valor_constante Valor numérico a utilizar cuando `metodo_imputacion`
#'   es "constante".
#' @param prob_percentil Probabilidad utilizada para calcular el percentil cuando
#'   se selecciona el método "percentil". Debe estar en (0, 1).
#'
#' @return El objeto `tsibble` con la columna `valor` imputada.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr fill
#' @importFrom stats median quantile
#' @importFrom tsibble is_tsibble index_var
#' @importFrom rlang sym
#' @export
aplicar_imputacion <- function(ts_data, metodo_imputacion, valor_constante = NULL, prob_percentil = 0.25) {
  x_num <- as.numeric(ts_data$fecha)

  imputado <- switch(
    metodo_imputacion,
    "interpolacion" = {
      ts_data %>%
        dplyr::mutate(valor = approx(x = x_num, y = valor, xout = x_num, rule = 2)$y)
    },
    "media" = {
      media_valor <- mean(ts_data$valor, na.rm = TRUE)
      ts_data %>%
        dplyr::mutate(valor = ifelse(is.na(valor), media_valor, valor))
    },
    "mediana" = {
      mediana_valor <- stats::median(ts_data$valor, na.rm = TRUE)
      ts_data %>%
        dplyr::mutate(valor = ifelse(is.na(valor), mediana_valor, valor))
    },
    "percentil" = {
      if (prob_percentil <= 0 || prob_percentil >= 1) {
        stop("prob_percentil debe estar en (0, 1).")
      }
      perc_valor <- as.numeric(stats::quantile(
        ts_data$valor,
        probs = prob_percentil,
        na.rm = TRUE,
        type = 7
      ))
      ts_data %>%
        dplyr::mutate(valor = ifelse(is.na(valor), perc_valor, valor))
    },
    "forward_fill" = {
      ts_data %>%
        tidyr::fill(valor, .direction = "down")
    },
    "backward_fill" = {
      ts_data %>%
        tidyr::fill(valor, .direction = "up")
    },
    "spline" = {
      if (sum(!is.na(ts_data$valor)) >= 4) {
        ts_data %>%
          dplyr::mutate(valor = spline(x = x_num[!is.na(valor)], y = valor[!is.na(valor)], xout = x_num)$y)
      } else {
        ts_data %>%
          dplyr::mutate(valor = approx(x = x_num, y = valor, xout = x_num, rule = 2)$y)
      }
    },
    "constante" = {
      if (is.null(valor_constante)) {
        stop("Para el método 'constante' debe especificar 'valor_constante'.")
      }
      ts_data %>%
        dplyr::mutate(valor = ifelse(is.na(valor), valor_constante, valor))
    },
    "cero" = {
      ts_data %>%
        dplyr::mutate(valor = ifelse(is.na(valor), 0, valor))
    },
    {
      ts_data
    }
  )

  if (any(is.na(imputado$valor))) {
    imputado <- imputado %>%
      tidyr::fill(valor, .direction = "downup")
  }

  cat("Imputación completada. Valores NA restantes:", sum(is.na(imputado$valor)), "\n")
  imputado
}

#' Extraer intervalos de pronóstico
#'
#' Obtiene los límites inferior y superior de los intervalos de confianza de un
#' objeto de pronóstico generado con `fable`. Si el nivel solicitado no está
#' disponible se calcula un intervalo aproximado a partir de la media y una
#' desviación estándar empírica.
#'
#' @param forecast_obj Objeto de pronóstico retornado por `fabletools::forecast()`.
#' @param nivel_conf Nivel de confianza (entre 0 y 1).
#'
#' @return Una lista con los elementos `inferior` y `superior`.
#'
#' @importFrom stats sd qnorm
#' @export
extraer_intervalos <- function(forecast_obj, nivel_conf) {
  nivel_str <- paste0(nivel_conf * 100, "%")

  if (!is.null(nivel_conf) && nivel_str %in% names(forecast_obj)) {
    intervalos <- forecast_obj[[nivel_str]]

    if (is.matrix(intervalos) && ncol(intervalos) == 2) {
      return(list(
        inferior = as.numeric(intervalos[, 1]),
        superior = as.numeric(intervalos[, 2])
      ))
    }

    if (is.numeric(intervalos)) {
      return(list(
        inferior = as.numeric(intervalos),
        superior = as.numeric(intervalos)
      ))
    }
  }

  mean_vec <- as.numeric(forecast_obj$.mean)
  sd_mean <- stats::sd(mean_vec, na.rm = TRUE)
  z_alpha <- stats::qnorm((1 + nivel_conf) / 2)

  list(
    inferior = mean_vec - z_alpha * sd_mean,
    superior = mean_vec + z_alpha * sd_mean
  )
}

#' Ejecutar un conjunto de métodos de pronóstico
#'
#' Genera pronósticos empleando diferentes métodos de la librería `forecast`,
#' `fable` y `e1071`. El resultado incluye tanto los valores
#' pronosticados como intervalos de confianza y métricas de evaluación calculadas
#' sobre una partición de prueba.
#'
#' @param train_data Datos de entrenamiento con columnas `fecha`, `fecha_ts` y `valor`.
#' @param test_data Datos de prueba con la misma estructura que `train_data`.
#' @param h_periods Número de períodos a pronosticar.
#' @param fechas_futuras Vector de fechas asociadas a los pronósticos futuros.
#' @param metodos Vector de métodos a ejecutar.
#' @param nivel_confianza Nivel de confianza para los intervalos.
#' @param nombre_columna Nombre de la columna que se está modelando.
#' @param frecuencia_ts Frecuencia de la serie temporal.
#'
#' @return Una lista con dos elementos: `pronosticos` (lista de data frames por
#'   método) y `metricas` (data frame con RMSE y MAPE por método).
#'
#' @importFrom stats ts sd predict embed qnorm
#' @importFrom utils tail
#' @importFrom dplyr mutate filter select arrange distinct summarise group_by n_distinct left_join
#' @importFrom tidyr fill pivot_longer
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom tsibble is_tsibble index_var
#' @importFrom rlang sym
#' @importFrom fabletools model forecast
#' @importFrom fable ARIMA ETS NNETAR MEAN NAIVE SNAIVE TSLM
#' @importFrom forecast ses hw tbats bats auto.arima arfima forecast
#' @importFrom e1071 svm
#' @export
ejecutar_pronosticos <- function(train_data, test_data, h_periods, fechas_futuras,
                                 metodos, nivel_confianza, nombre_columna,
                                 frecuencia_ts = 12) {

  resultados <- list()
  metricas <- data.frame()

  calc_metricas <- function(obs, pred) {
    if (length(obs) == 0 || length(pred) == 0) {
      return(c(RMSE = NA_real_, MAPE = NA_real_))
    }
    obs <- as.numeric(obs)
    pred <- as.numeric(pred)
    rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
    mape <- mean(abs((obs - pred) / obs) * 100, na.rm = TRUE)
    c(RMSE = rmse, MAPE = mape)
  }

  usa_forecast_pkg <- any(c("ses", "holtwinters", "tbats", "bats",
                            "arfima", "autoarima") %in% metodos)
  if (usa_forecast_pkg) {
    ts_train <- stats::ts(train_data$valor, frequency = frecuencia_ts)
  }

  if ("ses" %in% metodos) {
    tryCatch({
      forecast_ses <- forecast::ses(
        ts_train,
        h = h_periods,
        level = nivel_confianza * 100
      )

      if (nrow(test_data) > 0) {
        forecast_test_ses <- forecast::ses(
          ts_train,
          h = nrow(test_data),
          level = nivel_confianza * 100
        )
        m <- calc_metricas(test_data$valor, forecast_test_ses$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$ses <- data.frame(
        fecha = fechas_futuras,
        metodo = "SES",
        valor = as.numeric(forecast_ses$mean),
        limite_inferior = as.numeric(forecast_ses$lower[, 1]),
        limite_superior = as.numeric(forecast_ses$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(
        metricas,
        data.frame(metodo = "SES", RMSE = m["RMSE"], MAPE = m["MAPE"])
      )
      cat("SES completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en SES para", nombre_columna, ":", e$message)))
  }

  if ("holtwinters" %in% metodos) {
    tryCatch({
      forecast_hw <- forecast::hw(
        ts_train,
        seasonal = "additive",
        h = h_periods,
        level = nivel_confianza * 100
      )

      if (nrow(test_data) > 0) {
        forecast_test_hw <- forecast::hw(
          ts_train,
          seasonal = "additive",
          h = nrow(test_data),
          level = nivel_confianza * 100
        )
        m <- calc_metricas(test_data$valor, forecast_test_hw$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$holtwinters <- data.frame(
        fecha = fechas_futuras,
        metodo = "Holt-Winters",
        valor = as.numeric(forecast_hw$mean),
        limite_inferior = as.numeric(forecast_hw$lower[, 1]),
        limite_superior = as.numeric(forecast_hw$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(
        metricas,
        data.frame(metodo = "Holt-Winters", RMSE = m["RMSE"], MAPE = m["MAPE"])
      )
      cat("Holt-Winters completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en Holt-Winters para", nombre_columna, ":", e$message)))
  }

  if ("tbats" %in% metodos) {
    tryCatch({
      modelo_tbats <- forecast::tbats(ts_train)
      forecast_tbats <- forecast::forecast(modelo_tbats, h = h_periods, level = nivel_confianza * 100)

      if (nrow(test_data) > 0) {
        forecast_test_tbats <- forecast::forecast(modelo_tbats, h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_tbats$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$tbats <- data.frame(
        fecha = fechas_futuras,
        metodo = "TBATS",
        valor = as.numeric(forecast_tbats$mean),
        limite_inferior = as.numeric(forecast_tbats$lower[, 1]),
        limite_superior = as.numeric(forecast_tbats$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "TBATS", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("TBATS completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en TBATS para", nombre_columna, ":", e$message)))
  }

  if ("bats" %in% metodos) {
    tryCatch({
      modelo_bats <- forecast::bats(ts_train)
      forecast_bats <- forecast::forecast(modelo_bats, h = h_periods, level = nivel_confianza * 100)

      if (nrow(test_data) > 0) {
        forecast_test_bats <- forecast::forecast(modelo_bats, h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_bats$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$bats <- data.frame(
        fecha = fechas_futuras,
        metodo = "BATS",
        valor = as.numeric(forecast_bats$mean),
        limite_inferior = as.numeric(forecast_bats$lower[, 1]),
        limite_superior = as.numeric(forecast_bats$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "BATS", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("BATS completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en BATS para", nombre_columna, ":", e$message)))
  }

  if ("autoarima" %in% metodos) {
    tryCatch({
      modelo_autoarima <- forecast::auto.arima(ts_train)
      forecast_autoarima <- forecast::forecast(modelo_autoarima, h = h_periods, level = nivel_confianza * 100)

      if (nrow(test_data) > 0) {
        forecast_test_autoarima <- forecast::forecast(modelo_autoarima, h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_autoarima$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$autoarima <- data.frame(
        fecha = fechas_futuras,
        metodo = "AUTO.ARIMA",
        valor = as.numeric(forecast_autoarima$mean),
        limite_inferior = as.numeric(forecast_autoarima$lower[, 1]),
        limite_superior = as.numeric(forecast_autoarima$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "AUTO.ARIMA", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("AUTO.ARIMA completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en AUTO.ARIMA para", nombre_columna, ":", e$message)))
  }

  if ("arfima" %in% metodos) {
    tryCatch({
      modelo_arfima <- forecast::arfima(ts_train)
      forecast_arfima <- forecast::forecast(modelo_arfima, h = h_periods, level = nivel_confianza * 100)

      if (nrow(test_data) > 0) {
        forecast_test_arfima <- forecast::forecast(modelo_arfima, h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_arfima$mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$arfima <- data.frame(
        fecha = fechas_futuras,
        metodo = "ARFIMA",
        valor = as.numeric(forecast_arfima$mean),
        limite_inferior = as.numeric(forecast_arfima$lower[, 1]),
        limite_superior = as.numeric(forecast_arfima$upper[, 1]),
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "ARFIMA", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("ARFIMA completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en ARFIMA para", nombre_columna, ":", e$message)))
  }

  if ("arima" %in% metodos) {
    tryCatch({
      modelo_arima <- train_data %>% fabletools::model(fable::ARIMA(valor))
      forecast_arima <- modelo_arima %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_arima, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_arima <- modelo_arima %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_arima$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$arima <- data.frame(
        fecha = fechas_futuras,
        metodo = "ARIMA",
        valor = as.numeric(forecast_arima$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "ARIMA", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("ARIMA completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en ARIMA para", nombre_columna, ":", e$message)))
  }

  if ("ets" %in% metodos) {
    tryCatch({
      modelo_ets <- train_data %>% fabletools::model(fable::ETS(valor))
      forecast_ets <- modelo_ets %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_ets, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_ets <- modelo_ets %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_ets$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$ets <- data.frame(
        fecha = fechas_futuras,
        metodo = "ETS",
        valor = as.numeric(forecast_ets$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "ETS", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("ETS completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en ETS para", nombre_columna, ":", e$message)))
  }

  if ("nnetar" %in% metodos) {
    tryCatch({
      modelo_nnar <- train_data %>% fabletools::model(fable::NNETAR(valor))
      forecast_nnar <- modelo_nnar %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_nnar, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_nnar <- modelo_nnar %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_nnar$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$nnetar <- data.frame(
        fecha = fechas_futuras,
        metodo = "NNAR",
        valor = as.numeric(forecast_nnar$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "NNAR", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("NNAR completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en NNAR para", nombre_columna, ":", e$message)))
  }

  if ("mean" %in% metodos) {
    tryCatch({
      modelo_mean <- train_data %>% fabletools::model(fable::MEAN(valor))
      forecast_mean <- modelo_mean %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_mean, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_mean <- modelo_mean %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_mean$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$mean <- data.frame(
        fecha = fechas_futuras,
        metodo = "MEAN",
        valor = as.numeric(forecast_mean$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "MEAN", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("MEAN completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en MEAN para", nombre_columna, ":", e$message)))
  }

  if ("naive" %in% metodos) {
    tryCatch({
      modelo_naive <- train_data %>% fabletools::model(fable::NAIVE(valor))
      forecast_naive <- modelo_naive %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_naive, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_naive <- modelo_naive %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_naive$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$naive <- data.frame(
        fecha = fechas_futuras,
        metodo = "NAIVE",
        valor = as.numeric(forecast_naive$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "NAIVE", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("NAIVE completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en NAIVE para", nombre_columna, ":", e$message)))
  }

  if ("snaive" %in% metodos) {
    tryCatch({
      modelo_snaive <- train_data %>% fabletools::model(fable::SNAIVE(valor ~ lag("year")))
      forecast_snaive <- modelo_snaive %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_snaive, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_snaive <- modelo_snaive %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_snaive$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$snaive <- data.frame(
        fecha = fechas_futuras,
        metodo = "SNAIVE",
        valor = as.numeric(forecast_snaive$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "SNAIVE", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("SNAIVE completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en SNAIVE para", nombre_columna, ":", e$message)))
  }

  if ("tslm" %in% metodos) {
    tryCatch({
      modelo_tslm <- train_data %>% fabletools::model(fable::TSLM(valor ~ trend() + season()))
      forecast_tslm <- modelo_tslm %>% fabletools::forecast(h = h_periods, level = nivel_confianza * 100)
      intervalos <- extraer_intervalos(forecast_tslm, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_tslm <- modelo_tslm %>% fabletools::forecast(h = nrow(test_data))
        m <- calc_metricas(test_data$valor, forecast_test_tslm$.mean)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$tslm <- data.frame(
        fecha = fechas_futuras,
        metodo = "TSLM",
        valor = as.numeric(forecast_tslm$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "TSLM", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("TSLM completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en TSLM para", nombre_columna, ":", e$message)))
  }

  if ("svr" %in% metodos) {
    tryCatch({
      y <- as.numeric(train_data$valor)
      n <- length(y)
      max_lag <- min(12, n - 1)
      if (max_lag < 1) stop("No hay suficientes datos para SVR.")

      em <- stats::embed(y, max_lag + 1)
      y_reg <- em[, 1]
      X_reg <- em[, -1, drop = FALSE]

      svr_model <- e1071::svm(
        x = X_reg,
        y = y_reg,
        type = "eps-regression"
      )

      forecast_svr_iter <- function(model, serie_inicial, h, max_lag) {
        history <- as.numeric(serie_inicial)
        preds <- numeric(h)
        for (i in seq_len(h)) {
          lags <- rev(utils::tail(history, max_lag))
          x_new <- matrix(lags, nrow = 1)
          preds[i] <- stats::predict(model, x_new)
          history <- c(history, preds[i])
        }
        preds
      }

      pred_svr <- forecast_svr_iter(svr_model, y, h_periods, max_lag)

      fitted_in <- stats::predict(svr_model, X_reg)
      residuos <- y_reg - fitted_in
      sigma_hat <- stats::sd(residuos, na.rm = TRUE)
      z_alpha <- stats::qnorm((1 + nivel_confianza) / 2)
      lim_inf <- pred_svr - z_alpha * sigma_hat
      lim_sup <- pred_svr + z_alpha * sigma_hat

      if (nrow(test_data) > 0) {
        h_test <- nrow(test_data)
        pred_test <- forecast_svr_iter(svr_model, y, h_test, max_lag)
        m <- calc_metricas(test_data$valor, pred_test)
      } else {
        m <- c(RMSE = NA_real_, MAPE = NA_real_)
      }

      resultados$svr <- data.frame(
        fecha = fechas_futuras,
        metodo = "SVR",
        valor = as.numeric(pred_svr),
        limite_inferior = as.numeric(lim_inf),
        limite_superior = as.numeric(lim_sup),
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas,
                        data.frame(metodo = "SVR", RMSE = m["RMSE"], MAPE = m["MAPE"]))
      cat("SVR completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en SVR para", nombre_columna, ":", e$message)))
  }

  list(pronosticos = resultados, metricas = metricas)
}

#' Pronosticar series temporales desde un data frame
#'
#' Prepara un data frame con fechas y columnas numéricas para generar pronósticos
#' de múltiples métodos. Realiza imputación de valores faltantes, particiona los
#' datos en entrenamiento y prueba, ejecuta los métodos solicitados y consolida
#' los resultados junto con las métricas obtenidas.
#'
#' @param df Data frame con la información original.
#' @param fecha_col Nombre de la columna de fechas.
#' @param valor_cols Vector de columnas numéricas a pronosticar. Si es `NULL`
#'   se detectan automáticamente.
#' @param nivel_confianza Nivel de confianza para los intervalos.
#' @param periodos_pronostico Número de períodos a pronosticar. Si es `NULL` se
#'   estima automáticamente.
#' @param metodos Métodos a utilizar (ver `ejecutar_pronosticos`).
#' @param metodo_imputacion Estrategia de imputación para valores faltantes.
#' @param incluir_historicos Si `TRUE` adjunta los valores históricos imputados.
#' @param tipo_frecuencia Frecuencia de la serie: "diaria", "semanal",
#'   "mensual" o "anual".
#' @param frecuencia_ts Frecuencia numérica personalizada para objetos `ts`.
#'
#' @return Un data frame con los resultados de pronóstico y atributos con
#'   metadatos utilizados durante el proceso.
#'
#' @importFrom tsibble as_tsibble fill_gaps yearmonth yearweek index_var is_tsibble
#' @importFrom lubridate year interval floor_date time_length
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr select mutate arrange left_join summarise across filter distinct bind_rows group_by n
#' @importFrom tidyr pivot_longer
#' @export
Pronosticar <- function(df, fecha_col = "fecha", valor_cols = NULL, nivel_confianza = 0.95,
                        periodos_pronostico = NULL,
                        metodos = c("arima", "ets", "mean", "naive", "snaive", "tslm", "ses",
                                    "holtwinters", "tbats", "bats", "autoarima", "arfima",
                                    "svr"),
                        metodo_imputacion = "interpolacion", incluir_historicos = TRUE,
                        tipo_frecuencia = c("diaria", "semanal", "mensual", "anual"),
                        frecuencia_ts = NULL) {

  if (!fecha_col %in% names(df)) {
    stop("La columna de fecha especificada no existe en el dataframe")
  }

  tipo_frecuencia <- match.arg(tipo_frecuencia)

  if (is.null(frecuencia_ts)) {
    frecuencia_ts_int <- switch(
      tipo_frecuencia,
      "diaria" = 365,
      "semanal" = 52,
      "mensual" = 12,
      "anual" = 1
    )
  } else {
    frecuencia_ts_int <- frecuencia_ts
  }

  if (is.null(valor_cols)) {
    valor_cols <- names(df)[sapply(df, is.numeric) & names(df) != fecha_col]
    if (length(valor_cols) == 0) {
      stop("No se encontraron columnas numéricas para pronosticar")
    }
    cat("Columnas detectadas automáticamente:",
        paste(valor_cols, collapse = ", "), "\n")
  }

  if (!all(valor_cols %in% names(df))) {
    columnas_faltantes <- valor_cols[!valor_cols %in% names(df)]
    stop(paste("Las siguientes columnas no existen:",
               paste(columnas_faltantes, collapse = ", ")))
  }

  df_clean <- df %>%
    dplyr::select(fecha = dplyr::all_of(fecha_col),
                  dplyr::all_of(valor_cols)) %>%
    dplyr::mutate(fecha = as.Date(fecha)) %>%
    dplyr::arrange(fecha)

  fecha_min <- min(df_clean$fecha, na.rm = TRUE)
  fecha_max <- max(df_clean$fecha, na.rm = TRUE)

  by_seq <- switch(
    tipo_frecuencia,
    "diaria" = "day",
    "semanal" = "week",
    "mensual" = "month",
    "anual" = "year"
  )

  fechas_completas <- tibble::tibble(
    fecha = seq(from = fecha_min, to = fecha_max, by = by_seq)
  )

  df_clean <- fechas_completas %>%
    dplyr::left_join(df_clean, by = "fecha")

  na_por_columna <- df_clean %>%
    dplyr::select(-fecha) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~ sum(is.na(.)))) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "columna",
                        values_to = "na_count")

  if (any(na_por_columna$na_count > 0)) {
    cat("Valores NA encontrados por columna:\n")
    print(na_por_columna %>% dplyr::filter(na_count > 0))
  }

  cat("Serie configurada como",
      toupper(tipo_frecuencia),
      "(frecuencia ts =", frecuencia_ts_int, ")\n")

  resultados_todas_columnas <- list()
  metricas_todas_columnas <- data.frame()
  ts_imputados <- list()

  for (col_actual in valor_cols) {
    cat("\n=== PROCESANDO COLUMNA:", col_actual, "===\n")

    df_col <- df_clean %>%
      dplyr::select(fecha, valor = dplyr::all_of(col_actual))

    n_validos <- sum(!is.na(df_col$valor))
    if (n_validos < 10) {
      warning(paste("Columna", col_actual,
                    "tiene muy pocos datos válidos. Se omite."))
      next
    }

    df_col_ts <- df_col %>%
      dplyr::arrange(fecha) %>%
      dplyr::distinct(fecha, .keep_all = TRUE)

    if (tipo_frecuencia == "mensual") {
      ts_data <- df_col_ts %>%
        dplyr::mutate(fecha_ts = tsibble::yearmonth(fecha)) %>%
        dplyr::select(fecha, fecha_ts, valor) %>%
        dplyr::distinct(fecha_ts, .keep_all = TRUE) %>%
        tsibble::as_tsibble(index = fecha_ts) %>%
        tsibble::fill_gaps()
    } else if (tipo_frecuencia == "semanal") {
      ts_data <- df_col_ts %>%
        dplyr::mutate(fecha_ts = tsibble::yearweek(fecha)) %>%
        dplyr::select(fecha, fecha_ts, valor) %>%
        dplyr::distinct(fecha_ts, .keep_all = TRUE) %>%
        tsibble::as_tsibble(index = fecha_ts) %>%
        tsibble::fill_gaps()
    } else if (tipo_frecuencia == "diaria") {
      ts_data <- df_col_ts %>%
        dplyr::mutate(fecha_ts = fecha) %>%
        dplyr::select(fecha, fecha_ts, valor) %>%
        dplyr::distinct(fecha_ts, .keep_all = TRUE) %>%
        tsibble::as_tsibble(index = fecha_ts) %>%
        tsibble::fill_gaps()
    } else if (tipo_frecuencia == "anual") {
      ts_data <- df_col_ts %>%
        dplyr::mutate(
          fecha_ts = tsibble::yearmonth(lubridate::floor_date(fecha, "year"))
        ) %>%
        dplyr::select(fecha, fecha_ts, valor) %>%
        dplyr::distinct(fecha_ts, .keep_all = TRUE) %>%
        tsibble::as_tsibble(index = fecha_ts) %>%
        tsibble::fill_gaps()
    }

    if (any(is.na(ts_data$valor))) {
      cat("Aplicando imputación para", col_actual, "...\n")

      ts_data_imputado <- aplicar_imputacion(ts_data, metodo_imputacion)

      if (!tsibble::is_tsibble(ts_data_imputado)) {
        index_col <- tsibble::index_var(ts_data)
        ts_data <- ts_data_imputado %>%
          dplyr::mutate(fecha = as.Date(fecha_ts)) %>%
          dplyr::arrange(!!rlang::sym(index_col)) %>%
          dplyr::distinct(!!rlang::sym(index_col), .keep_all = TRUE) %>%
          tsibble::as_tsibble(index = !!rlang::sym(index_col))
      } else {
        ts_data <- ts_data_imputado
      }

      if (!"fecha" %in% names(ts_data)) {
        ts_data <- ts_data %>%
          dplyr::mutate(fecha = as.Date(fecha_ts))
      }
    }

    if (!"fecha" %in% names(ts_data)) {
      ts_data <- ts_data %>%
        dplyr::mutate(fecha = as.Date(fecha_ts))
    }

    ts_imputados[[col_actual]] <- ts_data %>%
      dplyr::select(fecha, valor) %>%
      tibble::as_tibble()

    n_total <- nrow(ts_data)
    n_train <- floor(n_total * 0.8)

    train_data <- ts_data[1:n_train, ]
    test_data <- ts_data[(n_train + 1):n_total, ]

    fecha_max_col <- max(df_col$fecha, na.rm = TRUE)
    if (is.null(periodos_pronostico)) {
      año_siguiente <- lubridate::year(fecha_max_col) + 1
      fecha_objetivo <- as.Date(paste0(año_siguiente, "-12-01"))
      meses_hasta_obj <- floor(lubridate::time_length(
        lubridate::interval(fecha_max_col, fecha_objetivo),
        unit = "month"
      ))
      h_periods <- max(1, meses_hasta_obj)
      cat("Pronosticando", h_periods,
          "pasos hasta diciembre", año_siguiente,
          "para", col_actual, "\n")
    } else {
      h_periods <- periodos_pronostico
      cat("Pronosticando", h_periods, "pasos especificados para",
          col_actual, "\n")
    }

    ultima_fecha_ts <- max(ts_data$fecha_ts)
    fechas_futuras_idx <- seq(ultima_fecha_ts + 1,
                              length.out = h_periods,
                              by = 1)
    fechas_futuras <- as.Date(fechas_futuras_idx)

    resultados_col <- ejecutar_pronosticos(
      train_data, test_data, h_periods, fechas_futuras,
      metodos, nivel_confianza, col_actual,
      frecuencia_ts = frecuencia_ts_int
    )

    if (length(resultados_col$pronosticos) > 0) {
      resultados_todas_columnas[[col_actual]] <- resultados_col$pronosticos

      metricas_col <- resultados_col$metricas %>%
        dplyr::mutate(columna = col_actual)
      metricas_todas_columnas <- rbind(metricas_todas_columnas, metricas_col)
    }
  }

  if (length(resultados_todas_columnas) == 0) {
    stop("No se pudo generar ningún pronóstico para ninguna columna")
  }

  df_pronosticos_final <- purrr::map_dfr(
    names(resultados_todas_columnas),
    function(col_name) {
      purrr::map_dfr(resultados_todas_columnas[[col_name]],
                     function(resultado_metodo) {
                       resultado_metodo %>%
                         dplyr::mutate(columna = col_name)
                     })
    }
  )

  if (incluir_historicos) {
    combinaciones_metodo_columna <- df_pronosticos_final %>%
      dplyr::distinct(metodo, columna)

    datos_historicos_expandidos <- purrr::map_dfr(
      seq_len(nrow(combinaciones_metodo_columna)),
      function(i) {
        metodo_actual <- combinaciones_metodo_columna$metodo[i]
        columna_actual <- combinaciones_metodo_columna$columna[i]

        base_hist <- ts_imputados[[columna_actual]] %>%
          dplyr::filter(!is.na(valor))

        base_hist %>%
          dplyr::mutate(
            metodo = metodo_actual,
            columna = columna_actual,
            limite_inferior = NA_real_,
            limite_superior = NA_real_,
            tipo = "Histórico"
          )
      }
    )

    df_final <- dplyr::bind_rows(datos_historicos_expandidos,
                                 df_pronosticos_final) %>%
      dplyr::arrange(columna, metodo, fecha)
  } else {
    df_final <- df_pronosticos_final
  }

  attr(df_final, "metricas") <- metricas_todas_columnas
  attr(df_final, "nivel_confianza") <- nivel_confianza
  attr(df_final, "columnas_pronosticadas") <- valor_cols
  attr(df_final, "datos_originales") <- nrow(df_clean)
  attr(df_final, "periodos_pronostico") <- h_periods
  attr(df_final, "datos_historicos") <- df_clean
  attr(df_final, "metodo_imputacion") <- metodo_imputacion
  attr(df_final, "incluye_historicos") <- incluir_historicos
  attr(df_final, "frecuencia") <- tipo_frecuencia

  cat("\n=== RESUMEN FINAL ===\n")
  cat("Columnas procesadas:", length(valor_cols), "\n")
  cat("Métodos exitosos por columna:\n")
  resumen_metodos <- df_pronosticos_final %>%
    dplyr::filter(tipo == "Pronóstico") %>%
    dplyr::group_by(columna) %>%
    dplyr::summarise(metodos_exitosos = dplyr::n_distinct(metodo),
                     .groups = "drop")
  print(resumen_metodos)

  df_final
}

#' Mostrar métricas de evaluación de pronósticos
#'
#' Presenta en consola las métricas calculadas durante el proceso de pronóstico
#' y muestra resúmenes por columna o para una columna en particular.
#'
#' @param resultado_pronostico Objeto retornado por `Pronosticar()`.
#' @param columna Nombre de la columna a consultar. Si es `NULL` se muestran
#'   todas las columnas.
#'
#' @return Invisiblemente retorna `NULL` tras imprimir la información.
#'
#' @export
PronMetricas <- function(resultado_pronostico, columna = NULL) {
  metricas <- attr(resultado_pronostico, "metricas")

  if (is.null(metricas)) {
    cat("No se encontraron métricas en el resultado del pronóstico\n")
    return(invisible(NULL))
  }

  cat("=== MÉTRICAS DE EVALUACIÓN ===\n")

  if (!is.null(columna)) {
    metricas_filtradas <- metricas %>%
      dplyr::filter(columna == !!columna)
    if (nrow(metricas_filtradas) == 0) {
      cat("No se encontraron métricas para la columna:", columna, "\n")
      return(invisible(NULL))
    }
    cat("Columna:", columna, "\n")
    print(metricas_filtradas %>% dplyr::select(-columna))
  } else {
    print(metricas)

    cat("\n=== RESUMEN POR COLUMNA ===\n")
    resumen_columnas <- metricas %>%
      dplyr::group_by(columna) %>%
      dplyr::summarise(
        metodos_evaluados = dplyr::n(),
        mejor_rmse = min(RMSE, na.rm = TRUE),
        mejor_mape = min(MAPE, na.rm = TRUE),
        metodo_mejor_rmse = metodo[which.min(RMSE)],
        metodo_mejor_mape = metodo[which.min(MAPE)],
        .groups = "drop"
      )
    print(resumen_columnas)
  }

  cat("\nNivel de confianza:",
      attr(resultado_pronostico, "nivel_confianza") * 100, "%\n")
  cat("Columnas pronosticadas:",
      paste(attr(resultado_pronostico, "columnas_pronosticadas"),
            collapse = ", "), "\n")
  cat("Datos originales:",
      attr(resultado_pronostico, "datos_originales"), "\n")
  cat("Períodos pronosticados:",
      attr(resultado_pronostico, "periodos_pronostico"), "\n")

  invisible(NULL)
}

#' Seleccionar el mejor método de pronóstico
#'
#' Evalúa las métricas generadas por `Pronosticar()` para cada columna y
#' selecciona el método con mejor desempeño según un criterio configurable. En
#' caso de no encontrar métodos válidos se utiliza un método de respaldo.
#'
#' @param resultado_pronostico Resultado de `Pronosticar()`.
#' @param columna Columna específica a analizar. Si es `NULL` se procesan todas.
#' @param criterio_principal Criterio para ordenar los modelos: "compuesto",
#'   "rmse" o "mape".
#' @param peso_rmse Peso del RMSE en la puntuación compuesta.
#' @param peso_mape Peso del MAPE en la puntuación compuesta.
#' @param peso_intervalo Peso de la amplitud del intervalo en la puntuación.
#' @param excluir_metodos Métodos a excluir del análisis.
#' @param mostrar_analisis Si `TRUE` imprime mensajes informativos.
#' @param fallback_method Método utilizado en caso de no contar con métricas válidas.
#'
#' @return Una lista con la información del mejor modelo para cada columna.
#'
#' @export
PronSeleccionar <- function(resultado_pronostico, columna = NULL,
                            criterio_principal = "compuesto",
                            peso_rmse = 0.5, peso_mape = 0.3, peso_intervalo = 0.2,
                            excluir_metodos = c("SNAIVE"),
                            mostrar_analisis = TRUE,
                            fallback_method = "MEAN") {

  metricas <- attr(resultado_pronostico, "metricas")

  if (is.null(metricas)) {
    stop("No se encontraron métricas en el resultado del pronóstico")
  }

  if (!is.null(columna)) {
    if (!columna %in% metricas$columna) {
      stop(paste("La columna", columna, "no existe en los resultados"))
    }
    metricas <- metricas %>% dplyr::filter(columna == !!columna)
    resultado_pronostico <- resultado_pronostico %>% dplyr::filter(columna == !!columna)
    columnas_a_procesar <- columna
  } else {
    columnas_a_procesar <- unique(metricas$columna)
  }

  selecciones_por_columna <- list()

  for (col_actual in columnas_a_procesar) {
    cat("\n=== SELECCIÓN PARA COLUMNA:", col_actual, "===\n")

    tryCatch({
      metricas_col <- metricas %>%
        dplyr::filter(columna == col_actual,
                      !metodo %in% excluir_metodos) %>%
        dplyr::filter(!is.na(RMSE), !is.na(MAPE))

      if (nrow(metricas_col) == 0) {
        warning(paste("No hay métodos válidos para la columna",
                      col_actual, "- usando", fallback_method))
        metricas_mean <- metricas %>%
          dplyr::filter(columna == col_actual, metodo == fallback_method)
        datos_mejor_modelo <- resultado_pronostico %>%
          dplyr::filter(metodo == fallback_method, columna == col_actual)

        if (nrow(datos_mejor_modelo) == 0) {
          any_row <- resultado_pronostico %>%
            dplyr::filter(columna == col_actual) %>%
            dplyr::slice(1)
          if (nrow(any_row) == 0) {
            datos_mejor_modelo <- resultado_pronostico[0, , drop = FALSE]
          } else {
            any_row$metodo <- fallback_method
            datos_mejor_modelo <- any_row
          }
        }

        attr(datos_mejor_modelo, "metricas") <- if (nrow(metricas_mean) > 0) metricas_mean else NULL
        attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
        attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
        attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
        attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
        attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
        attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
        attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

        seleccion_columna <- list(
          columna = col_actual,
          mejor_modelo = fallback_method,
          metricas_mejor = if (nrow(metricas_mean) > 0) metricas_mean else tibble::tibble(),
          ranking_completo = metricas %>% dplyr::filter(columna == col_actual),
          criterio_usado = paste("Fallback:", fallback_method),
          datos_mejor_modelo = datos_mejor_modelo
        )

        selecciones_por_columna[[col_actual]] <- seleccion_columna

        if (mostrar_analisis) {
          cat("Se seleccionó fallback", fallback_method,
              "para", col_actual, "\n")
        }
        next
      }

      amplitud_intervalos <- resultado_pronostico %>%
        dplyr::filter(tipo == "Pronóstico",
                      columna == col_actual,
                      !metodo %in% excluir_metodos) %>%
        dplyr::filter(!is.na(limite_inferior),
                      !is.na(limite_superior)) %>%
        dplyr::group_by(metodo) %>%
        dplyr::summarise(
          amplitud_promedio = mean(limite_superior - limite_inferior,
                                   na.rm = TRUE),
          amplitud_relativa = mean((limite_superior - limite_inferior) /
                                     abs(valor),
                                   na.rm = TRUE),
          .groups = "drop"
        )

      metricas_completas <- metricas_col %>%
        dplyr::left_join(amplitud_intervalos, by = "metodo")

      rmse_min <- min(metricas_completas$RMSE, na.rm = TRUE)
      rmse_max <- max(metricas_completas$RMSE, na.rm = TRUE)
      mape_min <- min(metricas_completas$MAPE, na.rm = TRUE)
      mape_max <- max(metricas_completas$MAPE, na.rm = TRUE)
      amp_min <- min(metricas_completas$amplitud_relativa, na.rm = TRUE)
      amp_max <- max(metricas_completas$amplitud_relativa, na.rm = TRUE)

      metricas_normalizadas <- metricas_completas %>%
        dplyr::mutate(
          rmse_norm = if (rmse_max > rmse_min)
            (RMSE - rmse_min) / (rmse_max - rmse_min) else 0,
          mape_norm = if (mape_max > mape_min)
            (MAPE - mape_min) / (mape_max - mape_min) else 0,
          amplitud_norm = if (amp_max > amp_min)
            abs(amplitud_relativa - stats::median(amplitud_relativa,
                                                  na.rm = TRUE)) /
            (amp_max - amp_min) else 0
        ) %>%
        dplyr::mutate(
          rmse_norm = ifelse(is.nan(rmse_norm), 0, rmse_norm),
          mape_norm = ifelse(is.nan(mape_norm), 0, mape_norm),
          amplitud_norm = ifelse(is.nan(amplitud_norm), 0, amplitud_norm)
        )

      metricas_puntuacion <- metricas_normalizadas %>%
        dplyr::mutate(
          puntuacion_compuesta = peso_rmse * rmse_norm +
            peso_mape * mape_norm +
            peso_intervalo * amplitud_norm
        ) %>%
        dplyr::arrange(puntuacion_compuesta)

      if (criterio_principal == "rmse") {
        mejor_modelo <- metricas_puntuacion %>%
          dplyr::arrange(RMSE) %>%
          dplyr::slice(1)
        criterio_usado <- "RMSE mínimo"
      } else if (criterio_principal == "mape") {
        mejor_modelo <- metricas_puntuacion %>%
          dplyr::arrange(MAPE) %>%
          dplyr::slice(1)
        criterio_usado <- "MAPE mínimo"
      } else {
        mejor_modelo <- metricas_puntuacion %>%
          dplyr::slice(1)
        criterio_usado <- "Puntuación compuesta"
      }

      mejor_metodo_nombre <- mejor_modelo$metodo
      datos_mejor_modelo <- resultado_pronostico %>%
        dplyr::filter(metodo == mejor_metodo_nombre,
                      columna == col_actual)

      attr(datos_mejor_modelo, "metricas") <- metricas %>%
        dplyr::filter(metodo == mejor_metodo_nombre,
                      columna == col_actual)
      attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
      attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
      attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
      attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
      attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
      attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
      attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

      seleccion_columna <- list(
        columna = col_actual,
        mejor_modelo = mejor_metodo_nombre,
        metricas_mejor = mejor_modelo,
        ranking_completo = metricas_puntuacion,
        criterio_usado = criterio_usado,
        datos_mejor_modelo = datos_mejor_modelo
      )

      selecciones_por_columna[[col_actual]] <- seleccion_columna

      if (mostrar_analisis) {
        cat("Mejor modelo para", col_actual, ":", mejor_metodo_nombre, "\n")
        cat("RMSE:", round(mejor_modelo$RMSE, 4),
            "| MAPE:", round(mejor_modelo$MAPE, 4), "%\n")
      }

    }, error = function(e) {
      warning(paste("Error al procesar columna", col_actual, ":", e$message,
                    "- usando", fallback_method, "como fallback"))
      metricas_mean <- metricas %>%
        dplyr::filter(columna == col_actual, metodo == fallback_method)
      datos_mejor_modelo <- resultado_pronostico %>%
        dplyr::filter(metodo == fallback_method, columna == col_actual)

      if (nrow(datos_mejor_modelo) == 0) {
        any_row <- resultado_pronostico %>%
          dplyr::filter(columna == col_actual) %>%
          dplyr::slice(1)
        if (nrow(any_row) == 0) {
          datos_mejor_modelo <- resultado_pronostico[0, , drop = FALSE]
        } else {
          any_row$metodo <- fallback_method
          datos_mejor_modelo <- any_row
        }
      }

      attr(datos_mejor_modelo, "metricas") <- if (nrow(metricas_mean) > 0) metricas_mean else NULL
      attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
      attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
      attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
      attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
      attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
      attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
      attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

      seleccion_columna <- list(
        columna = col_actual,
        mejor_modelo = fallback_method,
        metricas_mejor = if (nrow(metricas_mean) > 0) metricas_mean else tibble::tibble(),
        ranking_completo = metricas %>% dplyr::filter(columna == col_actual),
        criterio_usado = paste("Fallback por error:", fallback_method),
        datos_mejor_modelo = datos_mejor_modelo
      )

      selecciones_por_columna[[col_actual]] <- seleccion_columna
    })
  }

  if (length(selecciones_por_columna) == 1) {
    return(selecciones_por_columna[[1]])
  }

  selecciones_por_columna
}

#' Visualizar la serie seleccionada con intervalos de confianza
#'
#' Construye un gráfico interactivo con `plotly` que muestra los datos históricos
#' y los pronósticos del modelo seleccionado mediante `PronSeleccionar()`.
#'
#' @param seleccion Resultado de `PronSeleccionar()` para una columna o lista
#'   de columnas.
#' @param columna Nombre de la columna a graficar cuando `seleccion` contiene
#'   múltiples columnas.
#'
#' @return Un objeto `plotly` listo para renderizarse.
#'
#' @importFrom plotly plot_ly add_ribbons add_lines add_trace layout config
#' @importFrom stringr str_to_title
#' @export
PronSerie <- function(seleccion, columna = NULL) {
  if (is.list(seleccion) && !is.null(names(seleccion)) &&
      all(c("columna", "mejor_modelo", "datos_mejor_modelo") %in% names(seleccion))) {
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    metricas <- seleccion$metricas_mejor
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    metricas <- seleccion_col$metricas_mejor
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  historicos <- datos_mejor %>% dplyr::filter(tipo == "Histórico")
  pronosticos <- datos_mejor %>% dplyr::filter(tipo == "Pronóstico")

  pronosticos_clean <- pronosticos %>%
    dplyr::filter(!is.na(limite_inferior), !is.na(limite_superior)) %>%
    dplyr::mutate(
      hover_text = paste0("Límite inferior: ", round(limite_inferior, 2),
                          "<br>Límite superior: ", round(limite_superior, 2))
    )

  subtitulo <- paste0("RMSE: ", round(metricas$RMSE, 2),
                      " | MAPE: ",
                      format(round(metricas$MAPE, 2),
                             big.mark = ",", decimal.mark = "."),
                      " %")

  if (!is.null(metricas$MAPE) && !is.na(metricas$MAPE) && metricas$MAPE > 50) {
    subtitulo <- paste0(subtitulo, "<br>",
                        FormatearTexto("Nota: El modelo de pronóstico no convergió adecuadamente para los datos",
                                       color = "#C0392B", tamano_pct = 0.65))
  }

  titulo_principal <- paste0(
    "<b>Serie Pronosticada ",
    stringr::str_to_title(col_actual),
    " - Método: ", mejor_metodo, "</b><br>",
    "<span style='font-size:14px; color:#7F8C8D'>",
    subtitulo, "</span>"
  )

  plotly::plot_ly() %>%
    plotly::add_ribbons(
      data = pronosticos_clean,
      x = ~fecha, ymin = ~limite_inferior, ymax = ~limite_superior,
      fillcolor = "rgba(231, 76, 60, 0.3)",
      line = list(color = "transparent"),
      name = "Intervalo de Confianza",
      text = ~hover_text,
      hovertemplate = paste0("<b>Intervalo de Confianza</b><br>",
                             "%{text}",
                             "<extra></extra>")
    ) %>%
    plotly::add_lines(
      data = historicos, x = ~fecha, y = ~valor,
      line = list(color = "#2C3E50", width = 2),
      name = "Histórico",
      hovertemplate = paste0("<b>Datos Históricos</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::add_trace(
      data = pronosticos, x = ~fecha, y = ~valor,
      type = "scatter", mode = "lines+markers",
      line = list(color = "#E74C3C", width = 2),
      marker = list(color = "#E74C3C", size = 5, symbol = "diamond"),
      name = "Pronóstico",
      hovertemplate = paste0("<b>Punto de Pronóstico</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::layout(
      title = list(
        text = titulo_principal,
        font = list(size = 18, color = "#2C3E50"),
        x = 0.5
      ),
      xaxis = list(
        title = list(text = "Fecha", font = list(size = 14)),
        showgrid = TRUE, gridcolor = "#E8E8E8", gridwidth = 1
      ),
      yaxis = list(
        title = list(text = col_actual, font = list(size = 14)),
        showgrid = TRUE, gridcolor = "#E8E8E8", gridwidth = 1
      ),
      hovermode = "x unified", showlegend = TRUE,
      legend = list(
        orientation = "h", x = 0.5, y = -0.2,
        xanchor = "center", yanchor = "top"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(t = 100)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

#' Comparar patrones mensuales por año
#'
#' Genera un gráfico tipo spaghetti por año con colores distintivos para los
#' datos históricos y, opcionalmente, los pronósticos del modelo seleccionado.
#'
#' @param seleccion Resultado de `PronSeleccionar()` para una columna o lista.
#' @param columna Columna específica a mostrar cuando se entregan múltiples.
#' @param incluir_pronosticos Si `TRUE` los valores pronosticados se incluyen en
#'   el gráfico.
#'
#' @return Un objeto `plotly` con la comparación mensual.
#'
#' @importFrom plotly plot_ly add_trace layout config
#' @importFrom lubridate month year
#' @export
PronMensual <- function(seleccion, columna = NULL, incluir_pronosticos = TRUE) {
  if (is.list(seleccion) && !is.null(names(seleccion)) &&
      all(c("columna", "mejor_modelo", "datos_mejor_modelo") %in% names(seleccion))) {
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    metricas <- seleccion$metricas_mejor
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    metricas <- seleccion_col$metricas_mejor
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  datos_preparados <- datos_mejor %>%
    dplyr::mutate(
      mes = format(fecha, "%B"),
      mes_num = lubridate::month(fecha),
      año = lubridate::year(fecha),
      mes_abrev = format(fecha, "%b")
    ) %>%
    dplyr::arrange(fecha)

  datos_grafico <- if (incluir_pronosticos) datos_preparados
  else datos_preparados %>% dplyr::filter(tipo == "Histórico")

  años_unicos <- sort(unique(datos_grafico$año))
  año_actual <- as.numeric(format(Sys.Date(), "%Y"))

  asignar_color_año <- function(años) {
    azules_verdes <- c(
      "rgba(31, 78, 121, 0.7)",
      "rgba(0, 128, 128, 0.6)",
      "rgba(0, 168, 150, 0.55)",
      "rgba(255, 193, 7, 0.5)",
      "rgba(255, 87, 34, 0.5)",
      "rgba(156, 39, 176, 0.6)",
      "rgba(63, 81, 181, 0.6)",
      "rgba(0, 188, 212, 0.5)",
      "rgba(139, 195, 74, 0.5)",
      "rgba(205, 220, 57, 0.5)",
      "rgba(255, 202, 40, 0.5)"
    )

    rojos <- c("#8B0000", "#B22222")
    negro <- c("#292526", "#3E3839")

    colores_asignados <- character(length(años))

    años_anteriores <- años[años < año_actual]
    años_actuales <- años[años == año_actual]
    años_futuros <- años[años > año_actual]

    if (length(años_anteriores) > 0) {
      n_anteriores <- length(años_anteriores)
      colores_anteriores <- azules_verdes[1:min(n_anteriores, length(azules_verdes))]
      colores_asignados[años %in% años_anteriores] <- colores_anteriores
    }

    if (length(años_actuales) > 0) {
      colores_asignados[años %in% años_actuales] <- rojos[1]
    }

    if (length(años_futuros) > 0) {
      colores_asignados[años %in% años_futuros] <- negro[1]
    }

    stats::setNames(colores_asignados, as.character(años))
  }

  colores_asignados <- asignar_color_año(años_unicos)

  meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

  p <- plotly::plot_ly()

  for (año in años_unicos) {
    datos_año <- datos_grafico %>% dplyr::filter(año == !!año)

    if (any(datos_año$tipo == "Pronóstico")) {
      estilo_linea <- list(color = colores_asignados[as.character(año)],
                           width = 3, dash = "dash")
      estilo_marcador <- list(color = colores_asignados[as.character(año)],
                              size = 5, symbol = "diamond")
      nombre_año <- paste(año, "(Pronóstico)")
    } else {
      estilo_linea <- list(color = colores_asignados[as.character(año)],
                           width = 2)
      estilo_marcador <- list(color = colores_asignados[as.character(año)],
                              size = 4, symbol = "circle")
      nombre_año <- as.character(año)
    }

    p <- p %>%
      plotly::add_trace(
        data = datos_año,
        x = ~mes_num, y = ~valor,
        type = "scatter", mode = "lines+markers",
        line = estilo_linea,
        marker = estilo_marcador,
        name = nombre_año,
        customdata = ~mes,
        text = ~format(fecha, "%Y-%m-%d"),
        hovertemplate = paste("<b>", nombre_año, ": </b><br>",
                              "Valor: %{y:.2f}<br>",
                              "<extra></extra>")
      )
  }

  subtitulo <- paste0("RMSE: ", round(metricas$RMSE, 2),
                      " | MAPE: ",
                      format(round(metricas$MAPE, 2),
                             big.mark = ",", decimal.mark = "."),
                      " %")

  if (!is.null(metricas$MAPE) && !is.na(metricas$MAPE) && metricas$MAPE > 50) {
    subtitulo <- paste0(subtitulo, "<br>",
                        FormatearTexto("Nota: El modelo de pronóstico no convergió adecuadamente para los datos",
                                       color = "#C0392B", tamano_pct = 0.65))
  }

  titulo_principal <- paste0(
    "<b>Comparación Mensual por Año - ",
    col_actual, " - Método: ", mejor_metodo, "</b><br>",
    "<span style='font-size:14px; color:#7F8C8D'>",
    subtitulo, "</span>"
  )

  p %>%
    plotly::layout(
      title = list(
        text = titulo_principal,
        font = list(size = 18, color = "#2C3E50"),
        x = 0.5
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 1:12,
        ticktext = meses_esp
      ),
      yaxis = list(title = col_actual),
      hovermode = "x unified",
      showlegend = TRUE,
      legend = list(
        orientation = "h", x = 0.5, y = -0.1,
        xanchor = "center", yanchor = "top"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(t = 100)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

#' Analizar el patrón estacional histórico
#'
#' Calcula estadísticas mensuales de la serie histórica y las muestra en un
#' gráfico interactivo que combina promedio, mediana y bandas de desviación
#' estándar.
#'
#' @param seleccion Resultado de `PronSeleccionar()` para una columna o lista.
#' @param columna Columna específica a analizar cuando existen múltiples.
#'
#' @return Un objeto `plotly` con el patrón mensual histórico.
#'
#' @importFrom plotly plot_ly add_lines add_ribbons add_markers layout config
#' @importFrom lubridate month year
#' @importFrom stats median sd
#' @export
PronPatronMes <- function(seleccion, columna = NULL) {
  if (is.list(seleccion) && !is.null(names(seleccion)) &&
      all(c("columna", "mejor_modelo", "datos_mejor_modelo") %in% names(seleccion))) {
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    metricas <- seleccion$metricas_mejor
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    metricas <- seleccion_col$metricas_mejor
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  historicos <- datos_mejor %>%
    dplyr::filter(tipo == "Histórico") %>%
    dplyr::mutate(
      mes = format(fecha, "%B"),
      mes_num = lubridate::month(fecha),
      año = lubridate::year(fecha)
    )

  stats_mensuales <- historicos %>%
    dplyr::group_by(mes, mes_num) %>%
    dplyr::summarise(
      promedio = mean(valor, na.rm = TRUE),
      mediana = stats::median(valor, na.rm = TRUE),
      desviacion = stats::sd(valor, na.rm = TRUE),
      minimo = min(valor, na.rm = TRUE),
      maximo = max(valor, na.rm = TRUE),
      coef_variacion = stats::sd(valor, na.rm = TRUE) /
        mean(valor, na.rm = TRUE) * 100,
      n_observaciones = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mes_num)

  meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

  subtitulo <- ""
  if (!is.null(metricas$MAPE) && !is.na(metricas$MAPE) && metricas$MAPE > 50) {
    subtitulo <- paste0(subtitulo, "<br>",
                        FormatearTexto("Nota: El modelo de pronóstico no convergió adecuadamente para los datos",
                                       color = "#C0392B", tamano_pct = 0.65))
  }

  grafico <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = stats_mensuales, x = ~mes_num, y = ~promedio,
      line = list(color = "#2C3E50", width = 4),
      name = "Promedio", customdata = ~mes,
      hovertemplate = paste0("<b>Promedio</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::add_ribbons(
      data = stats_mensuales,
      x = ~mes_num,
      ymin = ~(promedio - desviacion),
      ymax = ~(promedio + desviacion),
      fillcolor = "rgba(44, 62, 80, 0.2)",
      line = list(color = "transparent"),
      name = "± 1 Desv. Estándar",
      customdata = ~mes,
      hovertemplate = paste0("<b>Banda de Desviación</b><br>",
                             "Promedio: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::add_lines(
      data = stats_mensuales, x = ~mes_num, y = ~mediana,
      line = list(color = "#E74C3C", width = 2, dash = "dot"),
      name = "Mediana", customdata = ~mes,
      hovertemplate = paste0("<b>Mediana</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::add_markers(
      data = stats_mensuales, x = ~mes_num, y = ~maximo,
      marker = list(color = "#27AE60", size = 8, symbol = "triangle-up"),
      name = "Máximo", customdata = ~mes,
      hovertemplate = paste0("<b>Máximo</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::add_markers(
      data = stats_mensuales, x = ~mes_num, y = ~minimo,
      marker = list(color = "#E67E22", size = 8, symbol = "triangle-down"),
      name = "Mínimo", customdata = ~mes,
      hovertemplate = paste0("<b>Mínimo</b><br>",
                             "Valor: %{y:.2f}<br>",
                             "<extra></extra>")
    ) %>%
    plotly::layout(
      title = list(
        text = paste("<b>Patrón Estacional Histórico -",
                      col_actual, "- Método:", mejor_metodo, "</b>",
                      subtitulo),
        font = list(size = 18, color = "#2C3E50"),
        x = 0.5
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 1:12,
        ticktext = meses_esp
      ),
      yaxis = list(title = col_actual),
      hovermode = "x unified",
      showlegend = TRUE,
      legend = list(
        orientation = "h", x = 0.5, y = -0.1,
        xanchor = "center", yanchor = "top"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(t = 100)
    ) %>%
    plotly::config(displayModeBar = FALSE)

  cat("=== ESTADÍSTICAS MENSUALES PARA", col_actual, "===\n")
  print(stats_mensuales)

  grafico
}
