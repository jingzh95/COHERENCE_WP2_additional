################################################
## we extract the ATC & diagnostic code
## extract the num of users & patients at the 
## time of matching - earliest date
## then divided by the popuplation
################################################
setwd("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/to_export_add/")
NCDM_path <- "/tsd/p1380/data/durable/Nordic_CDM/Tables v5d - new data"
country_code <- "NO" # the country code is useless. we change format to integer below
library(data.table)
################################################
## extract the diagnostic code and ATC code
## from the "predictors"
################################################
load ("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Analysis/Permanent_datasets/tempal.RData")

pred <- names(tempal)
pred <- pred[pred %like% "_"]
diag_code <- pred[pred %like% "diag"]
diag_code <- substr(diag_code, 10, 12)
diag_code <- unique(diag_code)
diag_code <- diag_code[!diag_code==""]
atc <- pred[!pred %like% "diag"]
atc <- substr(atc, 5, 11)
atc <- unique(atc)
atc <- atc[!atc==""]

################################################
## get the temp17 which contain the date var100 
## of birth and date of matching var108
################################################
load ("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Permanent_datasets/case_control_risk_set_matching_formating.RData") # temp17
summary(temp17$var108) # If var108 is the date of matching, we use the earliest date
#  Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-02-24" "2020-12-14" "2021-04-11" "2021-04-17" "2021-11-08" "2021-12-31" 
## we use the min. date to extract the prevalence
date <- as.Date(min(temp17$var108))

################################################
## get the num of drug users for each ATC code
################################################
tablename <- "drug"
df <- fread(paste0(NCDM_path, "/", tablename, ".csv")) # 218223415 lines
df <- df[,.(person_id, disp_date, atc_code)]
df$person_id <- gsub(country_code, "", df$person_id)
df$person_id <- as.integer(df$person_id)
df$disp_date <- as.Date(df$disp_date , "%d%b%Y")
prev <- df[((disp_date <= date) & (atc_code %in% atc)), ]
rm(df); gc()
summary(prev$disp_date)

num_users <- aggregate(prev, 
                       person_id~atc_code, 
                       function(person_id) length(unique(person_id)))
setnames(num_users, c("person_id","atc_code"), 
         c("num_pats.","code"))
rm(prev)
gc()

################################################
## get the population at "2020-02-24"
################################################
tablename <- "person"
p <- fread(paste0(NCDM_path, "/", tablename, ".csv"))
p$person_id <- gsub(country_code, "", p$person_id)
p$person_id <- as.integer(p$person_id)
p$birth_date <- as.Date(p$birth_date , "%d%b%Y")
p$death_date <- as.Date(p$death_date , "%d%b%Y")
p <- p[,.(person_id, death_date)]
tablename <- "migration"
emi <- fread(paste0(NCDM_path, "/", tablename, ".csv"))
emi <- emi[,.(person_id, migr_date)]
emi$person_id <- gsub(country_code, "", emi$person_id)
emi$person_id <- as.integer(emi$person_id)
emi$migr_date <- as.Date(emi$migr_date , "%d%b%Y")
pop <- merge(p, emi, by = "person_id", all.x = TRUE)

# keep people who don't have death date
# OR have death date later than the obs date
# OR have migr date later than the obs date

pop <- pop[is.na(death_date)|
             death_date> date|
             migr_date > date]
pop_size <- length(unique(pop$person_id)) # 5583791
rm(pop, p, emi)
gc()

################################################
## get the prevalence of drug use
################################################
num_users$prevalence <- 
  round(num_users$num_pats./pop_size, 2)

################################################
## get the prevalence of patients of diag code
################################################
tablename <- "diagnoses"
diag <- fread(paste0(NCDM_path, "/", tablename, ".csv"))
diag <- diag[,.(hosp_id, code)]
diag <- diag[code %in% diag_code,]
tablename <- "hospital"
hosp <- fread(paste0(NCDM_path, "/", tablename, ".csv"))
hosp <- hosp[,.(person_id, adm_date, hosp_id)]
hosp$person_id <- gsub(country_code, "", hosp$person_id)
hosp$person_id <- as.integer(hosp$person_id)
hosp$adm_date <- as.Date(hosp$adm_date , "%d%b%Y")
# hosp$disc_date <- as.Date(hosp$disc_date , "%d%b%Y")
hosp <- hosp[adm_date<=date,]
pats <- merge(diag, hosp, by = "hosp_id", all.x = TRUE)
num_pats <- aggregate(pats, 
                       person_id~code, 
                       function(person_id) length(unique(person_id)))
setnames(num_pats, "person_id", "num_pats.")
num_pats$prevalence <- round(num_pats$num_pats./pop_size, 2)
################################################
## put the prevalence together
################################################

prevalence <- rbind(num_users, num_pats)

fwrite(prevalence, "prevalence.csv")



