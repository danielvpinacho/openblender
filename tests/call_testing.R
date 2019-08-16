# TESTING #
############################################################
######################Create Dataset########################
df <- read.csv(file = "climas2018.csv", header = TRUE, sep = ",")
head(df)
nrow(df)
action <- "API_createDataset"
parameters <- list(
  token = "1AKnDhP09wa2yGRia5z1SGTxAUQAiV",
  id_user = "5d07bed33e59b27a93e43be8",
  name = "climas2018",
  descriptipon = "testing R library for OpenBlender",
  visibility = "public",
  tags = list("weather", "climate"),
  insert_observations = "on",
  dataframe = df
)
response <- openblender::call(action, parameters)
response
############################################################
#####################Insert observations####################
df <- read.csv(file = "promotion_succecss_ratio.csv", header = TRUE, sep = ",")
head(df)
nrow(df)
action <- "API_insertObservations"
parameters <- list(
  #oblender = 1,
  token = "67dTvR8KVRM044s9ldE94IgFc5HF3q",
  id_user = "5d48b439275b3f05db0feee2",
  id_dataset = "5d5342f28ca3b8a0b2c674de",
  notification = "on",
  observations = df
)
response <- openblender::call(action, parameters)
response

############################################################
#####################Obtain observations####################
#with daniel.pinacho
action <- "API_getObservationsFromDataset"
parameters <- list(
  test_call = "off",
  token = "1AKnDhP09wa2yGRia5z1SGTxAUQAiV",
  id_user = "5d07bed33e59b27a93e43be8",
  id_dataset = "5d5440729516292d0375f2a8",
  force_as_categorical = list("dia", "lluvia"),
  target_threshold = list(feature = "tmax", success_thr_over = 23.93)
)

dfr <- openblender::call(action, parameters)$sample
head(dfr)
nrow(dfr)
head(dfr)

#with danielvpinacho
action <- "API_getObservationsFromDataset"
parameters <- list(
  test_call = "off",
  token = "1AKnDhP09wa2yGRia5z1SGTxAUQAiV",
  id_user = "5d07bed33e59b27a93e43be8",
  id_dataset = "5d5440729516292d0375f2a8"
)

dfr <- openblender::call(action, parameters)$sample
nrow(dfr)
#head(dfr)
