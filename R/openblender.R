library(httr)
library(jsonlite)

dameRespuestaLlamado <- function(url, data) {
  data$json <- toJSON(data$json, auto_unbox = TRUE)
  resp <- POST(url=url, body=data, encode="form")
  if (content(resp)$status == 'error') {
    print(paste("ERROR:",content(resp)))
    return(FALSE)
  } else {
    cont <- content(resp)
    if(hasName(cont, 'sample')) {
      sample <- toJSON(cont$sample, dataframe = "rows")
      cont$sample <- fromJSON(sample)
    }
    return(cont)
  }
}

call <- function(action, json_parametros) {
  respuesta <- tryCatch({
    if(hasName(json_parametros, 'oblender') && json_parametros$oblender == 1) {
      url <- 'http://3.16.237.62:8080/bronce'
    } else {
      url <- 'http://52.8.156.139/oro/'
    }
    switch(action,
           API_createDataset={ respuesta <- api_createDataset(json_parametros, url) },
           API_insertObservations={ respuesta <- api_insertObservationsFromDataFrame(json_parametros, url) },
           API_getObservationsFromDataset={ respuesta <- api_getSampleObservationsFromDataset(json_parametros, url) },
           API_powerModel={ respuesta <- api_powerModel(json_parametros, url) },
           {
             data <- list(action=action, json=json_parametros)
             respuesta <- dameRespuestaLlamado(url, data)
           }
           )
    return(respuesta)
  }, error = function(e) {
    if(hasName(json_parametros, 'oblender') && json_parametros['oblender'] == 1) {
      print("err 1")
      print("internal error")
      print(e)
    } else {
      print(list(status="internal error openblender", msg=e))
    }
    return(list(status="internal error", msg=e))
  })
  return(respuesta)
}

api_createDataset <- function(json_parametros, url) {
  action <- 'API_createDataset'
  if(hasName(json_parametros, 'dataframe')) { nom_obs <- "dataframe" } else { nom_obs <- "observations" }
  obj <- comprobarJSONaDF(json_parametros[nom_obs])
  if(!obj$valido) { return(obj$msj) }
  n_filas <- nrow(obj$df_nuevo)
  tam_pedazo_ini <- 1000
  insert_observations <- TRUE
  json_particion <- json_parametros
  if(hasName(json_parametros, 'insert_observations')) {
    if(json_parametros$insert_observations == 1 || json_parametros$insert_observations == 'on') {
      insert_observations <- TRUE
    } else {
      insert_observations <- FALSE
    }
  }
  if(hasName(json_parametros, 'test_call') && (json_parametros$test_call == 1 || json_parametros$test_call == 'on')) {
    test_call <- 1
  } else {
    test_call <- FALSE
  }
  if(test_call == 1) { print('This is a TEST CALL, set "test_call" : "off" or remove to execute service.') }
  respuesta0 <- NULL
  #Creación del dataset
  if(!test_call&&(n_filas > tam_pedazo_ini)) {
    if(insert_observations) {
      start <- Sys.time()
      json_particion[nom_obs] <- toJSON(obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ], dataframe = "columns")
      json_particion_molde <- json_particion
      json_particion_molde$insert_observations <- 0
      data <- list( action=action, json=json_particion_molde)
      respuesta <- dameRespuestaLlamado(url, data)
      if (!hasName(respuesta, "id_dataset")) { return(respuesta) }
      respuesta0 <- respuesta
      json_particion$id_dataset <- respuesta$id_dataset
      print(paste("Dataset created succesfully, id:", json_particion$id_dataset))
      print("Starting upload..")
      stop <- Sys.time()
      segundos <- as.integer(ceiling(stop - start))
      tam_pedazo <- as.integer(round((600 / segundos), digits = 0))
      action <- 'API_insertObservationsFromDataFrame'
      rownames(obj$df_nuevo) <- 1:n_filas
      for (i in seq(0, n_filas, by=tam_pedazo)) {
        if((n_filas - i) < tam_pedazo) { tam_pedazo <- (n_filas - i) }
        df_nuevo <- obj$df_nuevo[(i+1):(i+tam_pedazo), ]
        json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
        data <- list(action=action, json=json_particion)
        respuesta <- dameRespuestaLlamado(url, data)
        #Imprimir avance
        avance <- round(((i + tam_pedazo) / n_filas) * 100, digits = 2)
        if(avance > 100) {
          print("100%")
          print("Wrapping Up..")
        } else {
          print(paste(avance, "%"))
          Sys.sleep(2)
        }
      }
    } else {
      df_nuevo <- obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ]
      rownames(df_nuevo) <- 0:(nrow(df_nuevo)-1)
      json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
      data <- list(action=action, json=json_particion)
      respuesta <- dameRespuestaLlamado(url, data)
      return(respuesta)
    }
  } else {
    if(n_filas > tam_pedazo_ini) { tam_pedazo_ini <- tam_pedazo_ini } else { tam_pedazo_ini <- n_filas}
    df_nuevo <- obj$df_nuevo[sample(nrow(obj$df_nuevo), tam_pedazo_ini), ]
    json_particion[nom_obs] <- toJSON(df_nuevo, dataframe="columns")
    data <- list(action=action, json=json_particion)
    respuesta <- dameRespuestaLlamado(url, data)
    return(respuesta)
  }
  return(respuesta0)
}

api_insertObservationsFromDataFrame <- function(json_parametros, url) {
  action <- 'API_insertObservationsFromDataFrame'
  if(hasName(json_parametros, 'test_call') && (json_parametros$test_call == 1 || json_parametros$test_call == 'on')) { test_call <- 1 } else { test_call <- FALSE }
  if(test_call == 1) { print('This is a TEST CALL, set "test_call" : "off" or remove to execute service.') }
  if(hasName(json_parametros, 'dataframe')) { nom_obs <- "dataframe" } else { nom_obs <- "observations" }
  obj <- comprobarJSONaDF(json_parametros[nom_obs])
  if(!obj$valido) { return(obj$msj) }
  n_filas <- nrow(obj$df_nuevo)
  n_columnas <- ncol(obj$df_nuevo)
  print(head(obj$df_nuevo))
  tam_pedazo_ini <- 1000
  json_particion <- json_parametros
  if(n_filas > tam_pedazo_ini) {
    print("Uploading...")
    print("0%")
    start <- Sys.time()
    json_particion[nom_obs] <- toJSON(obj$df_nuevo[1:tam_pedazo_ini, ], dataframe = "columns")
    data <- list(action=action, json=json_particion)
    respuesta <- dameRespuestaLlamado(url, data)
    stop <- Sys.time()
    segundos <- as.integer(ceiling(stop - start))
    tam_pedazo <- as.integer(round(600 / segundos, digits = 0))
    json_particion <- json_parametros
    for(i in seq(tam_pedazo_ini, n_filas, by=tam_pedazo)) {
      if((n_filas - i) < tam_pedazo) { tam_pedazo <- (n_filas - i) }
      print(paste("from",(i+1), "to", (i+tam_pedazo)))
      df_nuevo <- obj$df_nuevo[(i+1):(i+tam_pedazo), ]
      json_particion[nom_obs] <- toJSON(df_nuevo, dataframe = "columns")
      data <- list(action=action, json=json_particion)
      respuesta <- dameRespuestaLlamado(url, data)
      #print(respuesta)
      #Imprimir avance
      avance <- round(((i + tam_pedazo) / n_filas) * 100, digits = 2)
      if(avance >= 100) {
        print("100%")
        print("Wrapping Up..")
      } else {
        print(paste(avance, "%"))
        Sys.sleep(2)
      } 
    }
  } else {
    json_parametros[nom_obs] <- toJSON(obj$df_nuevo, dataframe = "columns")
    data <- list(action=action, json=json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
  }
  return(respuesta)
}

api_getSampleObservationsFromDataset <- function(json_parametros, url) {
  action <- 'API_getSampleObservationsFromDataset'
  start <- Sys.time()
  if(hasName(json_parametros, 'test_call') && (json_parametros$test_call == 1 || json_parametros$test_call == 'on')) { test_call <- 1 } else { test_call <- FALSE }
  if(test_call == 1) {
    print('This is a TEST CALL, set "test_call" : "off" or remove to execute service.')
    data <- list(action=action, json=json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    print(respuesta)
    df_resp <- respuesta$sample
    head(df_resp)
    #rownames(df_resp) <- 0:nrow(df_resp)-1
    t_universo <- 0
  } else {
    json_parametros$tamano_bin <- 50
    json_parametros$skip <- 0
    print("Downloading...")
    data <- list(action=action, json=json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    t_universo <- respuesta$universe_size
    #print(paste("t universo", t_universo))
    stop <- Sys.time()
    segundos <- ceiling(stop - start)
    tam_pedazo <- as.integer(round(600 / as.integer(segundos), digits = 0))
    nums_pedazos <- ceiling(t_universo / tam_pedazo)
    #print(paste("num pedazos:", nums_pedazos))
    if(nums_pedazos <= 0) { nums_pedazos <- 1 }
    df_resp <- NULL
    for (i in seq(0, nums_pedazos, by=1)) {
      json_parametros$tamano_bin <- tam_pedazo
      json_parametros$skip <- tam_pedazo * i
      data <- list(action=action, json=json_parametros)
      respuesta <- dameRespuestaLlamado(url, data)
      df <- respuesta$sample
      if(is.null(df_resp)) {
        df_resp <- df
      } else {
        df_resp <- rbind(df_resp, df)
        #rownames(df_resp) <- 1:nrow(df_resp)
      }
      avance <- round(((i) / nums_pedazos) * 100, digits = 2)
      if (avance >= 100) {
        print(paste(avance, "% completed."))
      } else {
        print(paste(avance, "%"))
      }
    }
    if (hasName(json_parametros, 'sample_size')) {
      if (as.integer(json_parametros$sample_size) < nrow(df_resp)) {
        df_resp <- df_resp[-sample(nrow(df_resp), (nrow(df_resp) - as.integer(json_parametros$sample_size))), ]
      }
    }
  }
  print(typeof(df_resp$timestamp[[1]]))
  df_resp <- df_resp[order(-as.integer(df_resp$timestamp)), ]
  respuesta <- list(universe_size=t_universo, sample=df_resp)
  return(respuesta)
}

api_powerModel <- function(json_parametros, url) {
  action <- 'API_powerModel'
  data <- list(action=action, json=json_parametros)
  respuesta <- dameRespuestaLlamado(url, data)
  return(respuesta)
}

comprobarJSONaDF <- function(df_json) {
  obj <- list(valido=TRUE, msj="Success", df_nuevo=NULL)
  if(hasName(df_json, 'dataframe')) { ind <- 'dataframe' } else { ind <- 'observations' }
  tryCatch({
    obj$df_nuevo <- fromJSON(toJSON(df_json[[ind]]))
  }, error = function(e) {
    print(paste("TYPE:", typeof(df_json)))
    obj$df_nuevo <- NULL
    obj$valido <- FALSE
    obj$msj <- paste("Error transforming json: ", e)
    print(obj$msj)
  })
  return(obj)
}

OpenBlender <- list(call=call)