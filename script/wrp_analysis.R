required.packages <- c("data.table", "rstudioapi", "arrow", "jsonlite")
lapply(required.packages, require, character.only=T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")