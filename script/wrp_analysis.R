required.packages <- c("data.table", "rstudioapi", "arrow", "jsonlite", "lme4", "broom.mixed")
lapply(required.packages, require, character.only=T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

wrp4 <- fread("clean_src/wrp4_validate.csv")
schema <- fromJSON("src/WRP_wave4_for_candidate/WRP_wave4_for_candidate.schema.json")

schema_col <- rbindlist(lapply(schema$columns, function(x) x[c('label')]))
schema_col <- cbind(var = names(schema$columns), schema_col)

worry_var <- schema_col[grepl("^Worried", label)]$var
harm_var <- schema_col[grepl("^Experienced Harm", label)]$var
domain <- gsub("^Experienced Harm in Past Two Years: | [(]You/Someone You Know[)]", "", schema_col[grepl("^Experienced Harm", label)]$label)

length(worry_var) == length(harm_var)

#Checked, but we are making the assumption here that worry and harm are ordered the same
domain_map <- data.table(worry_var, harm_var, domain)

id_vars <- c(
  "COUNTRY_ISO3",
  "CountryIncomeLevel2025",
  "WGT",
  "Gender",
  "AgeGroups5",
  "Education", 
  "Urbanicity",
  "INCOME_5"
)

long_vars <- rbindlist(lapply(1:nrow(domain_map),
                            function(i) data.table(domain = domain_map$domain[i],
                                                   worry = wrp4[, get(domain_map$worry_var[i])],
                                                   harm = wrp4[, get(domain_map$harm_var[i])],
                                                   wrp4[, .SD, .SDcols = (id_vars)])))

check_cols <- c("Gender", "AgeGroups5", "Education", "Urbanicity", "INCOME_5", "CountryIncomeLevel2025", "worry", "harm")
long_vars[, (check_cols) := lapply(.SD, function(x) fifelse(x %in% c(9, 97, 98, 99), NA, x)), .SDcols = check_cols]

long_vars[, worry := 4-worry] #reverse worry scoring
long_vars[, harm := 1*(harm != 4)] #binary harm scoring

long_vars <- long_vars[complete.cases(long_vars)]

fac_cols <- c("Gender", "AgeGroups5", "Education", "Urbanicity", "INCOME_5", "CountryIncomeLevel2025")
long_vars[, (fac_cols) := lapply(.SD, as.factor), .SDcols = fac_cols]

#Worry domain model
domains <- domain_map$domain
fixed_effects_list <- list()
for(i in 1:length(domains)){
  dom <- domains[i]
  lmemodel <- lmer(worry ~ harm + Gender + AgeGroups5 + Education + Urbanicity + INCOME_5*CountryIncomeLevel2025 + (1 + harm | COUNTRY_ISO3),
                data = long_vars[domain == dom],
                weights = WGT)
  fe <- data.table(tidy(lmemodel, effects = "fixed"))
  fe[, domain := domains[i]]
  fixed_effects_list[[i]] <- fe
  message(dom, "...(", i, "/", length(domains), ")")
}

fixed_effects <- rbindlist(fixed_effects_list, fill = T)

if(!dir.exists("output")) dir.create("output")
fwrite(fixed_effects, "output/fe_modelout.csv")
