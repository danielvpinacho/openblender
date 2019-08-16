create_dataset <<- function(json_parametros, url) {
  action <- "API_createDataset"
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
  tam_pedazo_ini <- 1000
  insert_observations <- TRUE
  json_particion <- json_parametros
  if (hasName(json_parametros, "insert_observations")) {
    if (json_parametros$insert_observations == 1 || json_parametros$insert_observations == "on") {
      insert_observations <- TRUE
    } else {
      insert_observations <- FALSE
    }
  }
  if (hasName(json_parametros, "test_call") && (json_parametros$test_call == 1 || json_parametros$test_call == "on")) {
    test_call <- 1
  } else {
    test_call <- FALSE
  }
  if (test_call == 1) {
    print("This is a TEST CALL, set \"test_call\" : \"off\" or remove to execute service.")
  }
  respuesta0 <- NULL
  #Creación del dataset
  if (!test_call && (n_filas > tam_pedazo_ini)) {
    if (insert_observations) {
      start <- Sys.time()
      json_particion[nom_obs] <- toJSON(obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ], dataframe = "columns")
      json_particion_molde <- json_particion
      json_particion_molde$insert_observations <- 0
      data <- list(action = action, json = json_particion_molde)
      respuesta <- dameRespuestaLlamado(url, data)
      if (!hasName(respuesta, "id_dataset")) {
        return(respuesta)
      }
      respuesta0 <- respuesta
      json_particion$id_dataset <- respuesta$id_dataset
      print(paste("Dataset created succesfully, id:", json_particion$id_dataset))
      print("Starting upload..")
      stop <- Sys.time()
      segundos <- as.integer(ceiling(stop - start))
      tam_pedazo <- as.integer(round((600 / segundos), digits = 0))
      action <- "API_insertObservationsFromDataFrame"
      rownames(obj$df_nuevo) <- 1:n_filas
      for (i in seq(0, n_filas, by = tam_pedazo)) {
        if ((n_filas - i) < tam_pedazo) {
          tam_pedazo <- (n_filas - i)
        }
        df_nuevo <- obj$df_nuevo[(i + 1) : (i + tam_pedazo), ]
        json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
        data <- list(action = action, json = json_particion)
        respuesta <- dameRespuestaLlamado(url, data)
        #Imprimir avance
        avance <- round(((i + tam_pedazo) / n_filas) * 100, digits = 2)
        if (avance > 100) {
          print("100%")
          print("Wrapping Up..")
        } else {
          print(paste(avance, "%"))
          Sys.sleep(2)
        }
      }
    } else {
      df_nuevo <- obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ]
      rownames(df_nuevo) <- 0:(nrow(df_nuevo) - 1)
      json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
      data <- list(action = action, json = json_particion)
      respuesta <- dameRespuestaLlamado(url, data)
      return(respuesta)
    }
  } else {
    if (n_filas > tam_pedazo_ini) {
      tam_pedazo_ini <- tam_pedazo_ini
    } else {
      tam_pedazo_ini <- n_filas
    }
    df_nuevo <- obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ]
    json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
    data <- list(action = action, json = json_particion)
    respuesta <- dameRespuestaLlamado(url, data)
    return(respuesta)
  }
  return(respuesta0)
}
