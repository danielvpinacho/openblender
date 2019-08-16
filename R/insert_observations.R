insert_observations <<- function(json_parametros, url) {
  action <- "API_insertObservationsFromDataFrame"
  if (hasName(json_parametros, "test_call") && (json_parametros$test_call == 1 || json_parametros$test_call == "on")) {
    test_call <- 1
  } else {
    test_call <- FALSE
  }
  if (test_call == 1) {
    print("This is a TEST CALL, set \"test_call\" : \"off\" or remove to execute service.")
  }
  if (hasName(json_parametros, "dataframe")) {
    nom_obs <- "dataframe"
  } else {
    nom_obs <- "observations"
  }
  obj <- comprobarJSONaDF(json_parametros[nom_obs])
  if (!obj$valido) {
    return(obj$msj)
  }
  n_filas <- nrow(obj$df_nuevo)
  print(head(obj$df_nuevo))
  tam_pedazo_ini <- 1000
  json_particion <- json_parametros
  if (n_filas > tam_pedazo_ini) {
    print("Uploading...")
    print("0%")
    start <- Sys.time()
    json_particion[nom_obs] <- toJSON(obj$df_nuevo[1:tam_pedazo_ini, ], dataframe = "columns")
    data <- list(action = action, json = json_particion)
    respuesta <- dameRespuestaLlamado(url, data)
    stop <- Sys.time()
    segundos <- as.integer(ceiling(stop - start))
    tam_pedazo <- as.integer(round(600 / segundos, digits = 0))
    json_particion <- json_parametros
    for (i in seq(tam_pedazo_ini, n_filas, by = tam_pedazo)) {
      if ((n_filas - i) < tam_pedazo) {
        tam_pedazo <- (n_filas - i)
      }
      df_nuevo <- obj$df_nuevo[(i + 1):(i + tam_pedazo), ]
      json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
      data <- list(action = action, json = json_particion)
      respuesta <- dameRespuestaLlamado(url, data)
      #Imprimir avance
      avance <- round(((i + tam_pedazo) / n_filas) * 100, digits = 2)
      if (avance >= 100) {
        print("100%")
        print("Wrapping Up..")
      } else {
        print(paste(avance, "%"))
        Sys.sleep(2)
      }
    }
  } else {
    json_parametros[nom_obs] <- toJSON(obj$df_nuevo, dataframe = "columns")
    data <- list(action = action, json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
  }
  return(respuesta)
}