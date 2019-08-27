#'@title Request to the API, depending on the action provided
#'@description Prepare the data to send it 'OpenBlender' API. This function is not used by users.
#'@param json_parametros Request parameters
#'@param url Url selected
#'@return List of observations obtained with \link{dameRespuestaLlamado}.
#'@keywords internal
get_observations <- function(json_parametros, url) {
  action <- "API_getSampleObservationsFromDataset"
  start <- Sys.time()
  if (hasName(json_parametros, "test_call") && (json_parametros$test_call == 1 || json_parametros$test_call == "on")) {
    test_call <- 1
  } else {
    test_call <- FALSE
  }
  if (test_call == 1) {
    message("This is a TEST CALL, set \"test_call\" : \"off\" or remove to execute service.")
    data <- list(action = action, json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    df_resp <- respuesta$sample
    t_universo <- 0
  } else {
    json_parametros$tamano_bin <- 50
    json_parametros$skip <- 0
    message("Downloading...")
    data <- list(action = action, json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    t_universo <- respuesta$universe_size
    stop <- Sys.time()
    segundos <- ceiling(stop - start)
    tam_pedazo <- as.integer(round(600 / as.integer(segundos), digits = 0))
    nums_pedazos <- ceiling(t_universo / tam_pedazo)
    if (nums_pedazos <= 0) {
      nums_pedazos <- 1
    }
    df_resp <- NULL
    for (i in seq(0, nums_pedazos, by = 1)) {
      json_parametros$tamano_bin <- tam_pedazo
      json_parametros$skip <- tam_pedazo * i
      data <- list(action = action, json = json_parametros)
      respuesta <- dameRespuestaLlamado(url, data)
      df <- respuesta$sample
      if (is.null(df_resp)) {
        df_resp <- df
      } else {
        df_resp <- rbind(df_resp, df)
      }
      avance <- round(((i) / nums_pedazos) * 100, digits = 2)
      if (avance >= 100) {
        message(paste(avance, "% completed."))
      } else {
        message(paste(avance, "%"))
      }
    }
    if (hasName(json_parametros, "sample_size")) {
      if (as.integer(json_parametros$sample_size) < nrow(df_resp)) {
        df_resp <- df_resp[-sample(nrow(df_resp), (nrow(df_resp) - as.integer(json_parametros$sample_size))), ]
      }
    }
  }
  df_resp <- df_resp[order(-as.integer(df_resp$timestamp)), ]
  respuesta <- list(universe_size = t_universo, sample = df_resp)
  return(respuesta)
}
