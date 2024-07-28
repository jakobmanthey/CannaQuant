# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  CANNAQUANT
# CODE AUTHOR:    JM
# DATE STARTED:   240322

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

# input and output path
path <- paste0("data/")

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( tidyr )
library( stringr )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  1) CZECHIA
# -------------------------------------------------------

filename <- paste0(path,"Czechia/CQ czech.xlsx")

input1 <- data.table(openxlsx::read.xlsx(filename))
dim(input1) # 925 * 60


##  2) GERMANY
# -------------------------------------------------------

filename <- paste0(path,"Germany/CQ germany.xlsx")

input2 <- data.table(openxlsx::read.xlsx(filename))
dim(input2) # 836 * 60


##  3) SLOVAKIA
# -------------------------------------------------------

filename <- paste0(path,"Slovakia/CQ slovakia.xlsx")

input3 <- data.table(openxlsx::read.xlsx(filename))
dim(input3) # 636 * 58


##  4) SPAIN
# -------------------------------------------------------

filename <- paste0(path,"Spain/CQ spain.xlsx")

input4 <- data.table(openxlsx::read.xlsx(filename))
dim(input4) # 458 * 59


##  5) SWITZERLAND
# -------------------------------------------------------

filename <- paste0(path,"Switzerland/CQ switzerland.xlsx")

input5 <- data.table(openxlsx::read.xlsx(filename))
dim(input5) # 298 * 57



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

##  1) Czechia
#   .............................................

cz <- copy(input1)

# keep relevant variables:
str(cz)

cz <- cz[,.(country = "Czechia",
            studyID,male,education,education_cz,age,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# get educ:
cz[, table(education, useNA = "always")] # mostly missing
cz[, table(education, education_cz, useNA = "always")] # mostly missing
cz[, educ := education]
cz$education <- cz$education_cz <- NULL

# keep only if complete in sex, age, edu
cz[!complete.cases(cz[,.(male,age,educ)])]  # remove 892 rows
cz[complete.cases(cz[,.(male,age,educ)])]  # keep 33 rows

# keep only if complete in sex and age
cz[!complete.cases(cz[,.(male,age)])]  # remove 627 rows
cz[complete.cases(cz[,.(male,age)])]  # keep 298 rows
cz <- cz[complete.cases(cz[,.(male,age)])]  # keep 298 rows


##  2) Germany
#   .............................................

de <- copy(input2)

# keep relevant variables:
str(de)

de <- de[,.(country = "Germany",
                studyID,male,educ,age,
                frequency,freq_v1,freq_v2,
                pref_type,
                thc_flower,thc_resin,thc_other,thc_avg,
                cast_tot,highrisk,
                cw_tot_flowers,cw_tot_resin,
                total_flower,size_flower,n_flower_weekday,n_flower_weekend,
                total_resin,total_cannabis,total_thc)]


# keep only if complete in sex, age, edu
de[!complete.cases(de[,.(male,age,educ)])]  # remove 389 rows
de[complete.cases(de[,.(male,age,educ)])]  # keep 447 rows
de <- de[complete.cases(de[,.(male,age,educ)])]  # keep 447 rows


##  3) Slovakia
#   .............................................

sv <- copy(input3)

# keep relevant variables:
str(sv)

sv <- sv[,.(country = "Slovakia",
            studyID,male,education,education_ca,age,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# get educ:
sv[, table(education, useNA = "always")] # mostly missing
sv[, table(education, education_ca, useNA = "always")] # information contained in "education_ca" --> needs to be recoded
sv[, educ := education]
sv$education <- sv$education_ca <- NULL

# keep only if complete in sex, age, edu
sv[!complete.cases(sv[,.(male,age,educ)])]  # remove 597 rows
sv[complete.cases(sv[,.(male,age,educ)])]  # keep 40 rows

# keep only if complete in sex and age
sv[!complete.cases(sv[,.(male,age)])]  # remove 360 rows
sv[complete.cases(sv[,.(male,age)])]  # keep 276 rows
sv <- sv[complete.cases(sv[,.(male,age)])]  # keep 276 rows


##  4) Spain
#   .............................................

es <- copy(input4)

# keep relevant variables:
str(es)

es <- es[,.(country = "Spain",
            studyID,male,education,education_es,education_ca,age,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# get educ:
es[, table(education, useNA = "always")] # mostly missing
es[, table(education_es, useNA = "always")] # all missing
es[, table(education_ca, useNA = "always")] # mostly missing
es[, educ := education]
es$education <- es$education_ca <- es$education_es <- NULL

# keep only if complete in sex, age, edu
es[!complete.cases(es[,.(male,age,educ)])]  # remove 448 rows
es[complete.cases(es[,.(male,age,educ)])]  # keep 10 rows

# keep only if complete in sex and age
es[!complete.cases(es[,.(male,age)])]  # remove 235 rows
es[complete.cases(es[,.(male,age)])]  # keep 223 rows
es <- es[complete.cases(es[,.(male,age)])]  # keep 223 rows


##  5) Switzerland
#   .............................................

ch <- copy(input5)

# keep relevant variables:
str(ch)

ch <- ch[,.(country = "Switzerland",
            studyID,male,educ,age,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# show educ
ch[, table(educ)]

# keep only if complete in sex, age, edu
ch[!complete.cases(ch[,.(male,age,educ)])]  # remove 137 rows
ch[complete.cases(ch[,.(male,age,educ)])]  # keep 161 rows
ch <- ch[complete.cases(ch[,.(male,age,educ)])]  # keep 161 rows

##  Y) COMBINE
#   .............................................

data <- rbind(cz,de,sv,es,ch)

data$country <- factor(data$country) 
                       #levels = c("Czechia","Germany","Slovakia","Spain","Switzerland"))

data[, iso := dplyr::recode(country,
                            "Czechia" = "CZ",
                            "Germany" = "DE",
                            "Slovakia" = "SK",
                            "Spain" = "ES",
                            "Switzerland" = "CH")]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) DEFINE VARIABLES
# ______________________________________________________________________________________________________________________

##  1) Sociodemographics
#   .............................................

# sex
data[, table(male, useNA = "always")] # 0 missing values
data[, sex := dplyr::recode(male, "1" = "men", "0" = "women")]
data[, table(sex, useNA = "always")] # 0 missing values
data$sex <- factor(data$sex, levels = c("men","women"))
data[, table(country,sex)]
data$male <- NULL

# agegroup
data[, table(age, useNA = "always")] # 0 missing values
data[, agegroup := ifelse(age < 25, "18-24",
                          ifelse(age < 35, "25-34",
                                 ifelse(age < 45, "35-44",
                                        ifelse(age < 55, "45-54",
                                               ifelse(age < 99, "55-73", "===")))))]
data[, table(age, agegroup)]
data[, table(country,agegroup, useNA = "always")]
data[, prop.table(table(country,agegroup),1)]
data$agegroup <- factor(data$agegroup, levels = c("18-24","25-34","35-44","45-54","55-73"))

# education (add remaining countries later)
data[, table(educ, country, useNA = "always")] # 0 missing values
data$edugroup <- NA_character_
#input1[, table(education_DE,educ)]
data[country == "Germany", edugroup := ifelse(educ < 3, "low", # Haupt/Real
                                              ifelse(educ < 8, "mid", # Fachhochschulreife/Abitur
                                                     ifelse(educ == 8, "high", "===")))] # Studium
#input2[, table(education,educ)]
data[country == "Switzerland", edugroup := ifelse(educ < 3, "low", # Basic or primary schooling | Secondary schooling
                                              ifelse(educ == 3, "mid", # Post-secondary vocational training/education
                                                     ifelse(educ == 4, "high", "===")))] # Higher education / university studies

data[, table(educ, edugroup, useNA = "always")]
data[, table(edugroup, useNA = "always")]
data$edugroup <- factor(data$edugroup, levels = c("low","mid","high"))

                                        
##  2) CAST
#   .............................................

data[, table(cast_tot, useNA = "always")]
data$highrisk <- factor(data$highrisk, labels = c("low","high"))
data[, table(cast_tot, highrisk, useNA = "always")]

##  3) Cannabis Frequency
#   .............................................

# version
data[, table(frequency,freq_v1)]
data[, table(frequency,freq_v2)]

# v1
data[, table(freq_v1, useNA = "always")] # 794 missing values

# v2
data[, table(freq_v2, useNA = "always")] # 711 missing values

# joint freq
data$freq <- NA_real_
data[, freq := ifelse(frequency == 1, 4.285 * freq_v1,freq_v2)]
data[, summary(freq)]
data[, freqgroup := ifelse(freq < 10, "1-9",
                           ifelse(freq < 20, "10-19",
                                  ifelse(freq < 29, "20-28","29-30")))]
data[, table(freq, freqgroup, useNA = "always")] # 20 NA
data$freqgroup <- factor(data$freqgroup, levels = c("1-9","10-19","20-28","29-30"))


##  4) Cannabis Amount - CWA
#   .............................................

data[, table(cw_tot_flowers, useNA = "always")] # 477 missings
data[, table(cw_tot_resin, useNA = "always")] # 1110 missings


##  5) Cannabis Amount - CQ
#   .............................................

data[, table(pref_type, useNA = "always")] # other (0), flower (1), resin (2)
data[, pref_type := dplyr::recode(pref_type, 
                                  "0" = "other",
                                  "1" = "flower",
                                  "2" = "resin")]

# flower
data[, table(total_flower, useNA = "always")] # 956 NA
data[, table(is.na(total_flower))] # 449 NA, 956 valid

data[, table(is.na(total_flower), frequency,useNA = "always")]
data[, table(is.na(total_flower), n_flower_weekday,useNA = "always")]
data[, table(is.na(total_flower), n_flower_weekend,useNA = "always")]
data[, table(is.na(total_flower), size_flower, useNA = "always")]

data[, table(total_resin, useNA = "always")] # 1338 NA
data[, table(total_cannabis, useNA = "always")] # 889 NA

##  6) THC
#   .............................................

data[, table(thc_avg, useNA = "always")] # 870 NA
data$thc_avg <- round(data$thc_avg,3)
data[, table(total_thc, useNA = "always")] # 1106 NA
data$total_thc <- round(data$total_thc,3)

data[, thc_label := dplyr::recode(thc_avg,
                                  .missing = "unknown",
                                  "0.025" = "0-4.9%",
                                  "0.075" = "5-9.9%",
                                  "0.125" = "10-14.9%",
                                  "0.175" = "15-19.9%",
                                  "0.225" = "20-24.9%",
                                  "0.275" = "25-29.9%",
                                  "0.3" = "30%+")]
data[, table(thc_label, thc_avg, useNA = "always")]
data$thc_label <- factor(data$thc_label, levels = c("0-4.9%","5-9.9%","10-14.9%","15-19.9%","20-24.9%","25-29.9%","30%+","unknown"))

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURE
# ______________________________________________________________________________________________________________________

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) SAVE
# ______________________________________________________________________________________________________________________

out <- data[,.(iso,country,studyID,
               sex,age,agegroup,edugroup,
               freqgroup,
               pref_type,
               thc_avg,thc_label,
               cast_tot,highrisk,
               cw_tot_flowers,cw_tot_resin,
               total_flower,total_resin,total_cannabis,total_thc)]

write.csv(out, paste0(path,"all_countries_cleaned_data_",Sys.Date(),".csv"), row.names = F)
saveRDS(out, file = paste0(path,"all_countries_cleaned_data_",Sys.Date(),".rds"))
