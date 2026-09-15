required.packages <- c("data.table", "rstudioapi", "arrow", "jsonlite")
lapply(required.packages, require, character.only=T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("src/WRP_wave4_for_candidate")

#Load
wrp4 <- data.table(read_parquet("WRP_wave4_for_candidate.parquet"))
schema <- fromJSON("WRP_wave4_for_candidate.schema.json")

#Set missing values to placeholder
wrp4[is.na(wrp4)] <- 9999

#Parse column schema as R types
schema_col <- rbindlist(lapply(schema$columns, function(x) as.list(x[c('label', 'type')])))
schema_col <- cbind(var = names(schema$columns), schema_col)
schema_col[, r_type := "numeric"]
schema_col[type == "string", r_type := "character"]

#Age is incorrectly schema as string
schema_col[var == "Age", r_type := "numeric"]

#Coerce raw data to class
vars <- schema_col$var
wrp4[, (vars) := Map(as, .SD, schema_col$r_type), .SDcols = vars]      

#Validate against constrained vars
schema_labs <- rbindlist(lapply(schema$columns, function(x) data.table(x['value_labels'])))
schema_labs <- data.table(var = names(schema$columns), labs = schema_labs)

schema_labs <- schema_labs[labs.V1 != "NULL"]
schema_labs <- setNames(lapply(schema_labs$labs.V1, names), schema_labs$var)

#Disallow ages below 15 and above 99
schema_labs <- c(schema_labs, Age = list(15:99))

#Check if constrained responses validate 
constrained_vars <- names(schema_labs)
wrp4[, (constrained_vars) := Map(function(var, labs) fifelse(var %in% c(labs, 9999), var, NA), .SD, schema_labs), .SDcols = constrained_vars]

#Remove the Manics references
wrp4[wrp4 == "If You Tolerate This Your Children Will Be Next"] <- NA

#Unvalidated response rows
wrp4_fail <- wrp4[!complete.cases(wrp4)]

#Cleaned data, return missing values
wrp4_validate <- wrp4
wrp4_validate[wrp4_validate == 9999] <- NA

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
if(!dir.exists("clean_src")) dir.create("clean_src")
fwrite(wrp4_validate, "clean_src/wrp4_validate.csv")
fwrite(wrp4_fail, "clean_src/wrp4_fail.csv")
