dameRespuestaLlamado <<- function(url, data) {
  data$json <- toJSON(data$json, auto_unbox = TRUE)
  resp <- POST(url = url, body = data, encode = "form")
  if (content(resp)$status == "error") {
    print(paste("ERROR:", content(resp)))
    return(FALSE)
  } else {
    cont <- content(resp)
    if (hasName(cont, "sample")) {
      sample <- toJSON(cont$sample, dataframe = "rows")
      cont$sample <- fromJSON(sample)
    }
    return(cont)
  }
}

call <- function(action, json_parametros) {
  respuesta <- tryCatch({
    if (hasName(json_parametros, "oblender") && json_parametros$oblender == 1) {
      url <- "http://3.16.237.62:8080/bronce"
    } else {
      url <- "http://52.8.156.139/oro/"
    }
    switch(action,
           API_createDataset = {
             respuesta <- create_dataset(json_parametros, url)
           },
           API_insertObservations = {
             respuesta <- insert_observations(json_parametros, url)
           },
           API_getObservationsFromDataset = {
             respuesta <- get_observations(json_parametros, url)
           },
           API_powerModel = {
             respuesta <- power_model(json_parametros, url)
           }
           , {
             data <- list(action = action, json = json_parametros)
             respuesta <- dameRespuestaLlamado(url, data)
            }
           )
    return(respuesta)
  },
  error = function(e) {
    if (hasName(json_parametros, "oblender") && json_parametros$oblender == 1) {
      print("err 1")
      print("internal error")
      print(e)
    } else {
      print(list(status = "internal error openblender", msg = e))
    }
    return(list(status = "internal error", msg = e))
  })
  return(respuesta)
}

power_model <<- function(json_parametros, url) {
  action <- "API_powerModel"
  data <- list(action = action, json = json_parametros)
  respuesta <- dameRespuestaLlamado(url, data)
  return(respuesta)
}

comprobarJSONaDF <<- function(df_json) {
  obj <- list(valido = TRUE, msj = "Success", df_nuevo = NULL)
  if (hasName(df_json, "dataframe")) {
    ind <- "dataframe"
  } else {
    ind <- "observations"
  }
  tryCatch({
    obj$df_nuevo <- fromJSON(toJSON(df_json[[ind]]))
  },
  error = function(e) {
    print(paste("TYPE:", typeof(df_json)))
    obj$df_nuevo <- NULL
    obj$valido <- FALSE
    obj$msj <- paste("Error transforming json: ", e)
    print(obj$msj)
  })
  return(obj)
}
