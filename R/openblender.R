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

#'@title Make HTTP request to \href{http://openblender.io}{openblender.io} service
#'@description Function made to call OpenBlender API services.
#'@param action Task you're requesting
#'@param parameters Request options
#'@return The OpenBlender service response, which depends on the action and parameters provided.
#'@examples
#'##CREATE A DATASET
#'df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
#'action <- "API_createDataset"
#'parameters <- list(
#'token = "YOUR_TOKEN",
#'id_user = "YOUR_USER_ID",
#'name = "dataset name",
#'descriptipon = "Provide a description here",
#'visibility = "public",
#'tags = list("topic", "tag"),
#'insert_observations = "on",
#'dataframe = df
#')
#'response <- openblender::call(action, parameters)
#'
#'##INSERT OBSERVATIONS
#'df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
#'action <- "API_insertObservations"
#'parameters <- list(
#'token = "YOUR_TOKEN",
#'id_user = "YOUR_USER_ID",
#'id_dataset = "DATASET_ID",
#'notification = "on",
#'observations = df
#')
#'response <- openblender::call(action, parameters)
#'
#'##GET OBSERVATIONS
#'action <- "API_getObservationsFromDataset"
#'parameters <- list(
#'token = "YOUR_TOKEN",
#'id_user = "YOUR_USER_ID",
#'id_dataset = "DATASET_ID"
#')
#'response <- openblender::call(action, parameters)
call <- function(action, parameters) {
  respuesta <- tryCatch({
    if (hasName(parameters, "oblender") && parameters$oblender == 1) {
      url <- "http://3.16.237.62:8080/bronce"
    } else {
      url <- "http://52.8.156.139/oro/"
    }
    switch(action,
           API_createDataset = {
             respuesta <- create_dataset(parameters, url)
           },
           API_insertObservations = {
             respuesta <- insert_observations(parameters, url)
           },
           API_getObservationsFromDataset = {
             respuesta <- get_observations(parameters, url)
           },
           API_powerModel = {
             respuesta <- power_model(parameters, url)
           }
           , {
             data <- list(action = action, json = parameters)
             respuesta <- dameRespuestaLlamado(url, data)
            }
           )
    return(respuesta)
  },
  error = function(e) {
    if (hasName(parameters, "oblender") && parameters$oblender == 1) {
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
