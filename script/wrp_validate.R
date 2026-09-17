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

#Return list of responses not in schema, by var
constrained_vars <- names(schema_labs)
bad_vals <- rbindlist(lapply(constrained_vars, function(x) data.table(x, wrp4[[x]][!is.na(wrp4[[x]]) & !(wrp4[[x]] %in% c(schema_labs[[x]], 9999))])))

if(nrow(bad_vals) > 0){
  bad_vals_tab <- bad_vals[, .N, by = .(var = x, val = V2)][order(-N)]
  message(sum(bad_vals_tab$N), " bad values detected.")
}

### FIXES
#Remove the Manics references
wrp4[wrp4 == "If You Tolerate This Your Children Will Be Next"] <- NA

#Correct age in schema as string
schema_col[var == "Age", r_type := "numeric"]
wrp4[, Age := as.numeric(Age)]

#Replace non-allowed values with NA
wrp4[, (constrained_vars) := Map(function(var, labs) fifelse(var %in% c(labs, 9999), var, NA), .SD, schema_labs), .SDcols = constrained_vars]

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
if(exists("bad_vals_tab")) fwrite(bad_vals_tab, "clean_src/bad_vals.csv")
