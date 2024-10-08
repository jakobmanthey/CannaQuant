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
dim(input1) # 436 * 57  # 925 before October 24
input1$country <- "Czechia"


##  2) GERMANY
# -------------------------------------------------------

filename <- paste0(path,"Germany/CQ germany.xlsx")

input2 <- data.table(openxlsx::read.xlsx(filename))
dim(input2) # 836 * 58
input2$country <- "Germany"


##  3) NETHERLANDS
# -------------------------------------------------------

filename <- paste0(path,"Netherlands/CQ netherlands.xlsx")

input3 <- data.table(openxlsx::read.xlsx(filename))
dim(input3) # 51 * 57
input3$country <- "Netherlands"


##  4) PORTUGAL
# -------------------------------------------------------

filename <- paste0(path,"Portugal/CQ portugal.xlsx")

input4 <- data.table(openxlsx::read.xlsx(filename))
dim(input4) # 293 * 56
input4$country <- "Portugal"


##  5) SLOVAKIA
# -------------------------------------------------------

filename <- paste0(path,"Slovakia/CQ slovakia.xlsx")

input5 <- data.table(openxlsx::read.xlsx(filename))
dim(input5) # 636 * 58
input5$country <- "Slovakia"


##  6) SPAIN
# -------------------------------------------------------

filename <- paste0(path,"Spain/CQ spain.xlsx")

input6 <- data.table(openxlsx::read.xlsx(filename))
dim(input6) # 458 * 57
input6$country <- "Spain"


##  7) SWEDEN
# -------------------------------------------------------

filename <- paste0(path,"Sweden/CQ sweden.xlsx")

input7 <- data.table(openxlsx::read.xlsx(filename))
dim(input7) # 173 * 57 # 298 before October 24
input7$country <- "Sweden"


##  8) SWITZERLAND
# -------------------------------------------------------

filename <- paste0(path,"Switzerland/CQ switzerland.xlsx")

input8 <- data.table(openxlsx::read.xlsx(filename))
dim(input8) # 173 * 57 # 298 before October 24
input8$country <- "Switzerland"


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

cz <- cz[,.(country,studyID,male,age,
            education = educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
cz$resp_total <- nrow(cz)

# get educ:
cz[, table(education, useNA = "always")] # mostly missing
cz[, educ := education]
cz$education <-  NULL

# keep only if complete in sex, age, edu
cz[!complete.cases(cz[,.(male,age,educ)])]  # remove 403 rows
cz[complete.cases(cz[,.(male,age,educ)])]  # keep 33 rows

# keep only if complete in sex and age
cz[!complete.cases(cz[,.(male,age)])]  # remove 138 rows
cz[complete.cases(cz[,.(male,age)])]  # keep 298 rows
cz <- cz[complete.cases(cz[,.(male,age)])]  # keep 298 rows


##  2) Germany
#   .............................................

de <- copy(input2)

# keep relevant variables:
str(de)

de <- de[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
de$resp_total <- nrow(de)

# keep only if complete in sex, age, edu
de[!complete.cases(de[,.(male,age,educ)])]  # remove 389 rows
de[complete.cases(de[,.(male,age,educ)])]  # keep 447 rows
#de <- de[complete.cases(de[,.(male,age,educ)])]  # keep 447 rows

# keep only if complete in sex and age
de[!complete.cases(de[,.(male,age)])]  # remove 383 rows
de[complete.cases(de[,.(male,age)])]  # keep 453 rows
de <- de[complete.cases(de[,.(male,age)])]  # keep 453 rows



##  3) Netherlands
#   .............................................

nl <- copy(input3)

# keep relevant variables:
str(nl)

nl <- nl[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
nl$resp_total <- nrow(nl)

# keep only if complete in sex, age, edu
nl[!complete.cases(nl[,.(male,age,educ)])]  # remove 29 rows
nl[complete.cases(nl[,.(male,age,educ)])]  # keep 22 rows

# keep only if complete in sex and age
nl[!complete.cases(nl[,.(male,age)])]  # remove 22 rows
nl[complete.cases(nl[,.(male,age)])]  # keep 22 rows
nl <- nl[complete.cases(nl[,.(male,age)])]  # keep 22 rows


##  4) Portugal
#   .............................................

pt <- copy(input4)

# keep relevant variables:
str(pt)

pt <- pt[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
pt$resp_total <- nrow(pt)

# keep only if complete in sex, age, edu
pt[!complete.cases(pt[,.(male,age,educ)])]  # remove 260 rows
pt[complete.cases(pt[,.(male,age,educ)])]  # keep 33 rows

# keep only if complete in sex and age
pt[!complete.cases(pt[,.(male,age)])]  # remove 260 rows
pt[complete.cases(pt[,.(male,age)])]  # keep 33 rows
pt <- pt[complete.cases(pt[,.(male,age)])]  # keep 33 rows


##  5) Slovakia
#   .............................................

sv <- copy(input5)

# keep relevant variables:
str(sv)

sv <- sv[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
sv$resp_total <- nrow(sv)

# keep only if complete in sex, age, edu
sv[!complete.cases(sv[,.(male,age,educ)])]  # remove 360 rows
sv[complete.cases(sv[,.(male,age,educ)])]  # keep 276 rows

# keep only if complete in sex and age
sv[!complete.cases(sv[,.(male,age)])]  # remove 360 rows
sv[complete.cases(sv[,.(male,age)])]  # keep 276 rows
sv <- sv[complete.cases(sv[,.(male,age)])]  # keep 276 rows


##  6) Spain
#   .............................................

es <- copy(input6)

# keep relevant variables:
str(es)

es <- es[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
es$resp_total <- nrow(es)

# keep only if complete in sex, age, edu
es[!complete.cases(es[,.(male,age,educ)])]  # remove 433 rows
es[complete.cases(es[,.(male,age,educ)])]  # keep 25 rows

# keep only if complete in sex and age
es[!complete.cases(es[,.(male,age)])]  # remove 235 rows
es[complete.cases(es[,.(male,age)])]  # keep 223 rows
es <- es[complete.cases(es[,.(male,age)])]  # keep 223 rows


##  7) Sweden
#   .............................................

sw <- copy(input7)

# keep relevant variables:
str(sw)

sw <- sw[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
sw$resp_total <- nrow(sw)

# keep only if complete in sex, age, edu
sw[!complete.cases(sw[,.(male,age,educ)])]  # remove 105 rows
sw[complete.cases(sw[,.(male,age,educ)])]  # keep 64 rows

# keep only if complete in sex and age
sw[!complete.cases(sw[,.(male,age)])]  # remove 105 rows
sw[complete.cases(sw[,.(male,age)])]  # keep 64 rows
sw <- sw[complete.cases(sw[,.(male,age)])]  # keep 64 rows


##  8) Switzerland
#   .............................................

ch <- copy(input8)

# keep relevant variables:
str(ch)

ch <- ch[,.(country,studyID,male,age,
            educ,
            frequency,freq_v1,freq_v2,
            pref_type,
            thc_flower,thc_resin,thc_other,thc_avg,
            cast_tot,highrisk,
            cw_tot_flowers,cw_tot_resin,
            total_flower,size_flower,n_flower_weekday,n_flower_weekend,
            total_resin,total_cannabis,total_thc)]

# add total number of responses
ch$resp_total <- nrow(ch)

# show educ
ch[, table(educ)]

# keep only if complete in sex, age, edu
ch[!complete.cases(ch[,.(male,age,educ)])]  # remove 137 rows
ch[complete.cases(ch[,.(male,age,educ)])]  # keep 161 rows
#ch <- ch[complete.cases(ch[,.(male,age,educ)])]  # keep 161 rows

# keep only if complete in sex and age
ch[!complete.cases(ch[,.(male,age)])]  # remove 12 rows
ch[complete.cases(ch[,.(male,age)])]  # keep 161 rows
ch <- ch[complete.cases(ch[,.(male,age)])]  # keep 161 rows



##  9) COMBINE
#   .............................................

data <- rbind(cz,de,nl,pt,sv,es,sw,ch)

data$country <- factor(data$country) 
                       #levels = c("Czechia","Germany","Slovakia","Spain","Switzerland"))

data[, iso := dplyr::recode(country,
                            "Czechia" = "CZ",
                            "Germany" = "DE",
                            "Netherlands" = "NL",
                            "Portugal" = "PT",
                            "Slovakia" = "SK",
                            "Spain" = "ES",
                            "Sweden" = "SE",
                            "Switzerland" = "CH")]
data[, table(iso,country)]

# edu.complete
data[,edu.complete := !is.na(educ)]
data[edu.complete == T, table(iso,country)]
data[, resp_sexage := .N, by = iso]
data[, resp_sexageedu := sum(edu.complete), by = iso]
unique(data[,.(iso,resp_total,resp_sexage,resp_sexageedu)])



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
#data[, table(educ, country, useNA = "always")] # 0 missing values
#data$edugroup <- NA_character_
#input1[, table(education_DE,educ)]
#data[country == "Germany", edugroup := ifelse(educ < 3, "low", # Haupt/Real
#                                             ifelse(educ < 8, "mid", # Fachhochschulreife/Abitur
#                                                     ifelse(educ == 8, "high", "===")))] # Studium
#input2[, table(education,educ)]
#data[country == "Switzerland", edugroup := ifelse(educ < 3, "low", # Basic or primary schooling | Secondary schooling
#                                              ifelse(educ == 3, "mid", # Post-secondary vocational training/education
#                                                     ifelse(educ == 4, "high", "===")))] # Higher education / university studies

#data[, table(educ, edugroup, useNA = "always")]
#data[, table(edugroup, useNA = "always")]
#data$edugroup <- factor(data$edugroup, levels = c("low","mid","high"))

                                        
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
data[, table(freq_v1, useNA = "always")] # 868 missing values

# v2
data[, table(freq_v2, useNA = "always")] # 770 missing values

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

data[, table(cw_tot_flowers, useNA = "always")] # 543 missings
data[, table(cw_tot_resin, useNA = "always")] # 1185 missings


##  5) Cannabis Amount - CQ
#   .............................................

data[, table(pref_type, useNA = "always")] # other (0), flower (1), resin (2)
data[, pref_type := dplyr::recode(pref_type, 
                                  "0" = "other",
                                  "1" = "flower",
                                  "2" = "resin")]

# flower
data[, table(total_flower, useNA = "always")] # 277 NA
data[, table(is.na(total_flower))] # 277 NA, 1253 valid

data[, table(is.na(total_flower), frequency,useNA = "always")]
data[, table(is.na(total_flower), n_flower_weekday,useNA = "always")]
data[, table(is.na(total_flower), n_flower_weekend,useNA = "always")]
data[, table(is.na(total_flower), size_flower, useNA = "always")]

data[, table(total_resin, useNA = "always")] # 1338 NA
data[, table(total_cannabis, useNA = "always")] # 889 NA

##  6) THC
#   .............................................

data[, table(thc_avg, useNA = "always")] # 423 NA
data$thc_avg <- round(data$thc_avg,3)
data[, table(total_thc, useNA = "always")] # 497 NA
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
               sex,age,agegroup,
               educ,edu.complete,
               freqgroup,
               pref_type,
               thc_avg,thc_label,
               cast_tot,highrisk,
               cw_tot_flowers,cw_tot_resin,
               total_flower,total_resin,total_cannabis,total_thc,
               resp_total,resp_sexage,resp_sexageedu)]

write.csv(out, paste0(path,"all_countries_cleaned_data_",Sys.Date(),".csv"), row.names = F)
saveRDS(out, file = paste0(path,"all_countries_cleaned_data_",Sys.Date(),".rds"))
