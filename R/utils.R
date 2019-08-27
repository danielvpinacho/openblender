#'@title HTTP request
#'@description Make HTTP requests to 'OpenBlender' API services. This function is not used by users.
#'@param url URL selected
#'@param data Request data
#'@return A list from 'OpenBlender' API (response).
#'@keywords internal
dameRespuestaLlamado <- function(url, data) {
  data$json <- toJSON(data$json, auto_unbox = TRUE)
  resp <- POST(url = url, body = data, encode = "form")
  if (hasName(content(resp), "status") && content(resp)$status == "error") {
    message(content(resp))
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
#'@title Request to the API, depending on the action provided
#'@description Prepare the data to send it 'OpenBlender' API. This function is not used by users.
#'@param json_parametros Request parameters
#'@param url Url selected
#'@return A list obtained from \link{dameRespuestaLlamado}.
#'@keywords internal
power_model <- function(json_parametros, url) {
  action <- "API_powerModel"
  data <- list(action = action, json = json_parametros)
  respuesta <- dameRespuestaLlamado(url, data)
  return(respuesta)
}

#'@title Verify JSON
#'@description Check if a dataframe can be transformed into JSON with no errors. This function is not accessible for users.
#'@param df_json Dataframe to verify
#'@return Dataframe verified or an error message.
#'@keywords internal
comprobarJSONaDF <- function(df_json) {
  obj <- list(valido = TRUE, msj = "Success", df_nuevo = NULL)
  if (hasName(df_json, "dataframe")) {
    ind <- "dataframe"
  } else {
    ind <- "observations"
  }
  tryCatch({
    obj$df_nuevo <- fromJSON(toJSON(df_json[[ind]]))
    if(length(obj$df_nuevo) == 0) {
      obj$valido <- FALSE
    }
  },
  error = function(e) {
    obj$df_nuevo <- NULL
    obj$valido <- FALSE
    obj$msj <- paste("Error transforming json: ", e)
    warning(obj$msj)
  })
  return(obj)
}
