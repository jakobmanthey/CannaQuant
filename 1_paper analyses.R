# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  CANNAQUANT
# CODE AUTHOR:    JM
# DATE STARTED:   241209

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
library( lme4 )
library( ggthemes )
library( cowplot )
#library( tidyr )
#library( stringr )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)
green_shades_3 <- colorRampPalette(c("#DAF2D0", "#12501A"))(3)
green_shades_23 <- colorRampPalette(c("#DAF2D0", "#12501A"))(23)
colors_2 <- c("#FFC300", "#FF5733")
colors_6 <- c("#DAF7A6","#FFC300","#FF5733","#C70039","#900C3F","#581845")

# DATE last version of prepared data
DATE <- "2025-01-28"


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  1) READ IN DATA
# -------------------------------------------------------

filename <- paste0("data/all_countries_cleaned_data_",DATE,".rds")
input <- readRDS(file = filename)
dim(input) # 3635 * 27

data <- copy(input)

##  data cleaning to form analytical sample
nrow(data) # 3635
data <- data[!is.na(sex) & !is.na(agegroup)]
nrow(data) # 1534

data[, sample1 := !is.na(freqgroup) & !is.na(total_cannabis)]
nrow(data[sample1 == T]) # 1277

data <- data[sample1 == T]

nrow(data[edu.complete == T & !is.na(freqgroup) & !is.na(total_cannabis)]) # 911
data[, sample2 := edu.complete == T & !is.na(freqgroup)]


##  2) ...
# -------------------------------------------------------



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) COMPARE CWA and PRODUCT-SPECIFIC ESTIMATES
# ______________________________________________________________________________________________________________________

##  1) FLOWER (mostly for Supplementary Information 4)
# -------------------------------------------------------

comp.flower <- data[!is.na(cw_tot_flowers) & !is.na(total_flower),.(iso,sex,agegroup,edugroup,freqgroup,thc_label,cast_tot,highrisk,
                                                                    cw = cw_tot_flowers, cq = total_flower)]
nrow(comp.flower) # 786
comp.flower[is.na(freqgroup)]
comp.flower[,diff := cw-cq]
comp.flower[,rel := cw/cq-1]

# averages:
comp.flower[, .(mean = mean(cw),
                median = median(cw),
                p25 = quantile(cw, 0.25),
                p75 = quantile(cw, 0.75))]
comp.flower[, .(mean = mean(cq),
                median = median(cq),
                p25 = quantile(cq, 0.25),
                p75 = quantile(cq, 0.75))]

# median and 0 difference
comp.flower[, .(median = median(diff),
                p25 = quantile(diff, 0.25),
                p75 = quantile(diff, 0.75))]
comp.flower[diff == 0]

# intra-class correlation
psych::ICC(comp.flower[,.(cw,cq)]) # single: 0.23 / average rater: 0.37

# remove outliers:
comp.flower[abs(diff) > 30] # n=102
comp.flower[, keep := !abs(diff) > 30] 
hist(comp.flower[keep == T]$diff) # approximately normal with long tails
nrow(comp.flower[keep == F]) / nrow(comp.flower) # 14% outliers

# SUBSAMPLE without OUTLIERS: differences and variation country, freq, cast
nrow(comp.flower[keep == T]) # 684
comp.flower[keep == T, .(median = median(diff),
                         p25 = quantile(diff, 0.25),
                         p75 = quantile(diff, 0.75))]
psych::ICC(comp.flower[keep == T,.(cw,cq)]) # single: 0.91 / average rater: 0.94
comp.flower[keep == T, median(diff), by = iso]
comp.flower[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.flower[keep == T, median(diff), by = cast_tot][order(cast_tot)]

summary(lm(diff ~ iso + freqgroup + cast_tot, comp.flower[keep == T]))

# OUTLIER analyses: differences and variation country, freq, cast
summary(glm(keep==F ~ iso + sex + agegroup + edugroup + freqgroup + cast_tot, comp.flower, family = "binomial"))
# freqgroup: exp(1.09032) = 2.975
# CAST: exp(0.14038) = 1.15
# repeat without education
summary(glm(keep==F ~ iso + sex + agegroup + freqgroup + cast_tot, comp.flower, family = "binomial")) # Germany additional 

comp.flower[keep == T, median(diff), by = iso]
comp.flower[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.flower[keep == T, median(diff), by = cast_tot][order(cast_tot)]


##  2) RESIN (mostly for Supplementary Information 4)
# -------------------------------------------------------

comp.resin <- data[!is.na(cw_tot_resin) & !is.na(total_resin),.(iso,sex,agegroup,edugroup,freqgroup,thc_label,cast_tot,highrisk,
                                                                    cw = cw_tot_resin, cq = total_resin)]
nrow(comp.resin) # 129
comp.resin[is.na(freqgroup)]
comp.resin[,diff := cw-cq]
comp.resin[,rel := cw/cq-1]

# averages:
comp.resin[, .(mean = mean(cw),
                median = median(cw),
                p25 = quantile(cw, 0.25),
                p75 = quantile(cw, 0.75))]
comp.resin[, .(mean = mean(cq),
                median = median(cq),
                p25 = quantile(cq, 0.25),
                p75 = quantile(cq, 0.75))]

# median and 0 difference
comp.resin[, .(median = median(diff),
                p25 = quantile(diff, 0.25),
                p75 = quantile(diff, 0.75))]
comp.resin[diff == 0]

# intra-class correlation
psych::ICC(comp.resin[,.(cw,cq)]) # single: 0.08 / average rater: 0.14

# remove outliers:
comp.resin[abs(diff) > 30] # n=30
comp.resin[, keep := !abs(diff) > 30] 
hist(comp.resin[keep == T]$diff) # approximately normal
nrow(comp.resin[keep == F]) / nrow(comp.resin) # 23% outliers

# SUBSAMPLE without OUTLIERS: differences and variation country, freq, cast
nrow(comp.resin[keep == T]) # 99
comp.resin[keep == T, .(median = median(diff),
                         p25 = quantile(diff, 0.25),
                         p75 = quantile(diff, 0.75))]
psych::ICC(comp.resin[keep == T,.(cw,cq)]) # single: 0.87 / average rater: 0.93
comp.resin[keep == T, median(diff), by = iso]
comp.resin[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.resin[keep == T, median(diff), by = cast_tot][order(cast_tot)]

summary(lm(diff ~ iso + freqgroup + cast_tot, comp.resin[keep == T]))

# OUTLIER analyses: differences and variation country, freq, cast
summary(glm(keep==F ~ iso + sex + agegroup + edugroup + freqgroup + cast_tot, comp.resin, family = "binomial")) # none
# sex-women: exp(-2.2044) = 0.11
# freqgroup: exp(3.7646) = 43.15

# repeat without education
summary(glm(keep==F ~ iso + sex + agegroup + freqgroup + cast_tot, comp.resin, family = "binomial")) # only high-freq
# freqgroup: exp(2.3) = 9.97

comp.resin[keep == T, median(diff), by = iso]
comp.resin[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.resin[keep == T, median(diff), by = cast_tot][order(cast_tot)]






# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) PRIMARY ANALYSES
# ______________________________________________________________________________________________________________________

data[sample1 == T, mean(cast_tot)] # 7.6
data[sample1 == T, var(cast_tot)] # 16.5 --> overdispersion

##  1) CORRELATION
# -------------------------------------------------------

nrow(input[complete.cases(total_cannabis,cast_tot)]) # 1380
input[complete.cases(total_cannabis,cast_tot), cor(total_cannabis,cast_tot)] # .306
nrow(data[sample1 == T]) # 1277
data[sample1 == T, cor(total_cannabis,cast_tot)] # .287

##  2) REGRESSION MAIN ANALYSES
# -------------------------------------------------------

# 0: baseline
#pa_mod0 <- glmer(cast_tot ~ sex + agegroup + (1|iso), data[sample1 == T], family = "poisson")
pa_mod0 <- glmer.nb(cast_tot ~ sex + agegroup + (1|iso), data[sample1 == T])
summary(pa_mod0)
car::vif(pa_mod0) # all below 1.1

# 1a: frequency
#pa_mod1a <- glmer(cast_tot ~ sex + agegroup + freqgroup + (1|iso), data[sample1 == T], family = "poisson")
pa_mod1a <- glmer.nb(cast_tot ~ sex + agegroup + freqgroup + (1|iso), data[sample1 == T])
summary(pa_mod1a)
car::vif(pa_mod1a) # all below 1.1

# 1b: quantity (total)
#pa_mod1b <- glmer(cast_tot ~ sex + agegroup + total_cannabis + (1|iso), data[sample1 == T], family = "poisson")
#pa_mod1b <- glmer(cast_tot ~ sex + agegroup + log(total_cannabis) + (1|iso), data[sample1 == T], family = "poisson")
pa_mod1b <- glmer.nb(cast_tot ~ sex + agegroup + log(total_cannabis) + (1|iso), data[sample1 == T])
summary(pa_mod1b)
car::vif(pa_mod1b) # all below 1.1

# 2: frequency and quantity (per use day)
#pa_mod2 <- glmer(cast_tot ~ sex + agegroup + freqgroup + total_cannabis_useday + (1|iso), data[sample1 == T], family = "poisson")
pa_mod2 <- glmer.nb(cast_tot ~ sex + agegroup + freqgroup + total_cannabis_useday + (1|iso), data[sample1 == T])
summary(pa_mod2) # as expected: sig for total_cannabis_useday
anova(pa_mod2, pa_mod1a) # reduced AIC and BIC, sign. improvement
car::vif(pa_mod2) # all below 1.2


##  3) REGRESSION SENS ANALYSIS (WITH EDUCATION)
# -------------------------------------------------------

# 0: baseline
#pa_sa1_mod0 <- glmer(cast_tot ~ sex + agegroup + edugroup + (1|iso), data[sample2 == T], family = "poisson")
pa_sa1_mod0 <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + (1|iso), data[sample2 == T])
summary(pa_sa1_mod0)
car::vif(pa_sa1_mod0) # all below 1.2

# 1a: frequency
#pa_sa1_mod1a <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + (1|iso), data[sample2 == T], family = "poisson")
pa_sa1_mod1a <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + (1|iso), data[sample2 == T])
summary(pa_sa1_mod1a)
car::vif(pa_sa1_mod1a) # all below 1.2

# 1b: quantity (total)
#pa_sa1_mod1b <- glmer(cast_tot ~ sex + agegroup + edugroup + total_cannabis + (1|iso), data[sample2 == T], family = "poisson")
#pa_sa1_mod1b <- glmer(cast_tot ~ sex + agegroup + edugroup + log(total_cannabis) + (1|iso), data[sample2 == T], family = "poisson")
pa_sa1_mod1b <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + log(total_cannabis) + (1|iso), data[sample2 == T])
summary(pa_sa1_mod1b)
car::vif(pa_sa1_mod1b) # all below 1.2

# 2: frequency and quantity (per use day)
#pa_sa1_mod2 <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + total_cannabis_useday + (1|iso), data[sample2 == T], family = "poisson")
pa_sa1_mod2 <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + total_cannabis_useday + (1|iso), data[sample2 == T])
summary(pa_sa1_mod2) # as expected: sig for total_cannabis_useday
anova(pa_sa1_mod2, pa_sa1_mod1a) # reduced AIC and BIC, sign. improvement
car::vif(pa_sa1_mod2) # all below 1.2





# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) SECONDARY ANALYSES
# ______________________________________________________________________________________________________________________

##  1) CORRELATION
# -------------------------------------------------------

nrow(input[complete.cases(thc_units_total,cast_tot)]) # 986
input[complete.cases(thc_units_total,cast_tot), cor(thc_units_total,cast_tot)] # .305
nrow(data[sample1 == T & thc_units_total != 0]) # 921
data[sample1 == T & thc_units_total != 0, cor(thc_units_total,cast_tot)] # .280


##  2) SAMPLE 1 restricted to people with THC info - 921 people
# -------------------------------------------------------

# 0: baseline
#sa_mod0 <- glmer(cast_tot ~ sex + agegroup + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
sa_mod0 <- glmer.nb(cast_tot ~ sex + agegroup + (1|iso), data[sample1 == T & thc_units_total != 0])
summary(sa_mod0)
car::vif(sa_mod0) # all below 1.1

# 1a: frequency
#sa_mod1a <- glmer(cast_tot ~ sex + agegroup + freqgroup + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
sa_mod1a <- glmer.nb(cast_tot ~ sex + agegroup + freqgroup + (1|iso), data[sample1 == T & thc_units_total != 0])
summary(sa_mod1a)
car::vif(sa_mod1a) # all below 1.2

# 1b: THC units (total)
#sa_mod1b <- glmer(cast_tot ~ sex + agegroup + thc_units_total + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
#sa_mod1b <- glmer(cast_tot ~ sex + agegroup + log(thc_units_total) + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
sa_mod1b <- glmer.nb(cast_tot ~ sex + agegroup + log(thc_units_total) + (1|iso), data[sample1 == T & thc_units_total != 0])
summary(sa_mod1b)
car::vif(sa_mod1b) # all below 1.1

# 2: frequency and THC units (per use day)
sa_mod2 <- glmer(cast_tot ~ sex + agegroup + freqgroup + log(thc_units_useday) + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
sa_mod2 <- glmer.nb(cast_tot ~ sex + agegroup + freqgroup + log(thc_units_useday) + (1|iso), data[sample1 == T & thc_units_total != 0])
summary(sa_mod2) # as expected: sig for log(thc_units_useday)
anova(sa_mod2, sa_mod1a) # reduced AIC and BIC, sign. improvement
car::vif(sa_mod2) # all below 1.2

# 3: frequency and grams per day and avg THC
#sa_mod3 <- glmer(cast_tot ~ sex + agegroup + freqgroup + log(total_cannabis_useday) + thc_avg + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson") # THC as continuous covariate
#sa_mod3 <- glmer(cast_tot ~ sex + agegroup + freqgroup + log(total_cannabis_useday) + as.factor(thc_avg) + (1|iso), data[sample1 == T & thc_units_total != 0], family = "poisson")
sa_mod3 <- glmer.nb(cast_tot ~ sex + agegroup + freqgroup + log(total_cannabis_useday) + as.factor(thc_avg) + (1|iso), data[sample1 == T & thc_units_total != 0])
summary(sa_mod3) # THC not sig
anova(sa_mod3, sa_mod2) # no sign. improvement!
car::vif(sa_mod3) # all below 1.2


data[sample1 == T & thc_units_total != 0, prop.table(table(freqgroup,thc_label),2)]
data[sample1 == T & thc_units_total != 0, sum(thc_avg>=.2)/.N, by = (freqgroup == "29-30")]
# "Use of products above 20% THC content was more frequently reported by respondents with daily use patterns (38.2% vs. 25.4%)."


##  3) SAMPLE 2 - 659 people (SENS ANALYSIS 1)
# -------------------------------------------------------

# 0: baseline
#sa_sa2_mod0 <- glmer(cast_tot ~ sex + agegroup + edugroup + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
sa_sa2_mod0 <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + (1|iso), data[sample2 == T & thc_units_total != 0])
summary(sa_sa2_mod0)
car::vif(sa_sa2_mod0) # all below 1.2

# 1a: frequency
#sa_sa2_mod1a <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
sa_sa2_mod1a <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + (1|iso), data[sample2 == T & thc_units_total != 0])
summary(sa_sa2_mod1a)
car::vif(sa_sa2_mod1a) # all below 1.3

# 1b: THC units (total)
#sa_sa2_mod1b <- glmer(cast_tot ~ sex + agegroup + edugroup + thc_units_total + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
#sa_sa2_mod1b <- glmer(cast_tot ~ sex + agegroup + edugroup + log(thc_units_total) + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
sa_sa2_mod1b <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + log(thc_units_total) + (1|iso), data[sample2 == T & thc_units_total != 0])
summary(sa_sa2_mod1b)
car::vif(sa_sa2_mod1b) # all below 1.2

# 2: frequency and THC units (per use day)
#sa_sa2_mod2 <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(thc_units_useday) + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
sa_sa2_mod2 <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(thc_units_useday) + (1|iso), data[sample2 == T & thc_units_total != 0])
summary(sa_sa2_mod2) # as expected: sig for log(thc_units_useday)
anova(sa_sa2_mod2, sa_sa2_mod1a) # reduced AIC and BIC, sign. improvement
car::vif(sa_sa2_mod2) # all below 1.3

# 3: frequency and grams per day and avg THC
#sa_sa2_mod3 <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(total_cannabis_useday) + as.factor(thc_avg) + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
sa_sa2_mod3 <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(total_cannabis_useday) + as.factor(thc_avg) + (1|iso), data[sample2 == T & thc_units_total != 0])
summary(sa_sa2_mod3) # THC is not sig
#comp <- glmer(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(total_cannabis_useday) + (1|iso), data[sample2 == T & thc_units_total != 0], family = "poisson")
comp <- glmer.nb(cast_tot ~ sex + agegroup + edugroup + freqgroup + log(total_cannabis_useday) + (1|iso), data[sample2 == T & thc_units_total != 0])
anova(sa_sa2_mod3, comp) # no sign. improvement
car::vif(sa_sa2_mod3) # all below 1.4





# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) TABLES
# ______________________________________________________________________________________________________________________

##  1) TABLE 1. Primary analyses model output
# -------------------------------------------------------

sjPlot::tab_model(pa_mod0,pa_mod1a,pa_mod1b,pa_mod2,
                  dv.labels = c("Model 0: Baseline", "Model 1a: Frequency only", "Model 1b: Total use quantities only",
                                "Model 2: Frequency and quantities on use days"),
                  file = paste0("tabs/",Sys.Date(),"_TAB 1_primary analyses output.html"))

##  2) TABLE 2. Secondary analyses model output
# -------------------------------------------------------

sjPlot::tab_model(sa_mod0,sa_mod1a,sa_mod1b,sa_mod2,sa_mod3,
                  dv.labels = c("Model 0: Baseline", "Model 1a: Frequency only", "Model 1b: Total THC units only", 
                                "Model 2: Frequency and THC units on use days", "Model 3: Frequency, quantities on use days, and average THC concentration"),
                  file = paste0("tabs/",Sys.Date(),"_TAB 2_secondary analyses output.html"))




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) SUPPLEMENTARY TABLES
# ______________________________________________________________________________________________________________________

##  1) SUPPLEMENTARY TABLE 1 - CannaQuant components
# -------------------------------------------------------

##  2) SUPPLEMENTARY TABLE 2 - information on data collection
# -------------------------------------------------------

##  3) SUPPLEMENTARY TABLE 3 - descriptives
# -------------------------------------------------------

suptab3 <- data[sample1 == T, .(.N,
                                share_men = sum(sex == "men")/.N,
                                share_women = sum(sex == "women")/.N,
                                share_other = sum(sex == "other")/.N,
                                age_mean = format(mean(age),digits = 3, nsmall = 1),
                                age_IQR = paste0(format(quantile(age,0.25),digits = 2),"-",format(quantile(age,0.75),digits = 2)),
                                edu_low = sum(edugroup == "low", na.rm = T)/sum(!is.na(edugroup)),
                                edu_mid = sum(edugroup == "mid", na.rm = T)/sum(!is.na(edugroup)),
                                edu_high = sum(edugroup == "high", na.rm = T)/sum(!is.na(edugroup)),
                                freq_1 = sum(freqgroup == "1-9",na.rm = T)/sum(!is.na(freqgroup)),
                                freq_2 = sum(freqgroup == "10-19",na.rm = T)/sum(!is.na(freqgroup)),
                                freq_3 = sum(freqgroup == "20-28",na.rm = T)/sum(!is.na(freqgroup)),
                                freq_4 = sum(freqgroup == "29-30",na.rm = T)/sum(!is.na(freqgroup)),
                                pref_flower = sum(pref_type == "flower")/.N,
                                pref_resin = sum(pref_type == "resin")/.N,
                                total_cannabis_mean = format(mean(total_cannabis),digits = 3, nsmall = 1),
                                total_cannabis_IQR = paste0(format(quantile(total_cannabis,0.25),digits = 2),"-",format(quantile(total_cannabis,0.75),digits = 2)),
                                total_thc_mean = format(mean(total_thc, na.rm = T),digits = 3, nsmall = 1),
                                total_thc_IQR = paste0(format(quantile(total_thc,0.25,na.rm = T),digits = 2),"-",format(quantile(total_thc,0.75, na.rm = T),digits = 2)),
                                cast_risk = sum(highrisk == "high")/.N),
                by = country]

add <- data[sample1 == T, .(country = "Total",.N,
                            share_men = sum(sex == "men")/.N,
                            share_women = sum(sex == "women")/.N,
                            share_other = sum(sex == "other")/.N,
                            age_mean = format(mean(age),digits = 3, nsmall = 1),
                            age_IQR = paste0(format(quantile(age,0.25),digits = 2),"-",format(quantile(age,0.75),digits = 2)),
                            edu_low = sum(edugroup == "low", na.rm = T)/sum(!is.na(edugroup)),
                            edu_mid = sum(edugroup == "mid", na.rm = T)/sum(!is.na(edugroup)),
                            edu_high = sum(edugroup == "high", na.rm = T)/sum(!is.na(edugroup)),
                            freq_1 = sum(freqgroup == "1-9",na.rm = T)/sum(!is.na(freqgroup)),
                            freq_2 = sum(freqgroup == "10-19",na.rm = T)/sum(!is.na(freqgroup)),
                            freq_3 = sum(freqgroup == "20-28",na.rm = T)/sum(!is.na(freqgroup)),
                            freq_4 = sum(freqgroup == "29-30",na.rm = T)/sum(!is.na(freqgroup)),
                            pref_flower = sum(pref_type == "flower")/.N,
                            pref_resin = sum(pref_type == "resin")/.N,
                            total_cannabis_mean = format(mean(total_cannabis),digits = 3, nsmall = 1),
                            total_cannabis_IQR = paste0(format(quantile(total_cannabis,0.25),digits = 2),"-",format(quantile(total_cannabis,0.75),digits = 2)),
                            total_thc_mean = format(mean(total_thc, na.rm = T),digits = 3, nsmall = 1),
                            total_thc_IQR = paste0(format(quantile(total_thc,0.25,na.rm = T),digits = 2),"-",format(quantile(total_thc,0.75, na.rm = T),digits = 2)),
                            cast_risk = sum(highrisk == "high")/.N)]

suptab3 <- rbind(suptab3, add)
suptab3$country <- factor(suptab3$country) 

suptab3 <- suptab3[,.(country,
                      N,
                      share_men = paste0(format(share_men*100,digits = 3),"%"),
                      share_women = paste0(format(share_women*100,digits = 3),"%"),
                      share_other = paste0(format(share_other*100,digits = 3),"%"),
                      age = paste0(age_mean,"(",age_IQR,")"),
                      edu_low = paste0(format(edu_low*100,digits = 3),"%"),
                      edu_mid = paste0(format(edu_mid*100,digits = 3),"%"),
                      edu_high = paste0(format(edu_high*100,digits = 3),"%"),
                      pref_flower = paste0(format(pref_flower*100,digits = 3),"%"),
                      pref_resin = paste0(format(pref_resin*100,digits = 3),"%"),
                      freq_1 = paste0(format(freq_1*100,digits = 3),"%"),
                      freq_2 = paste0(format(freq_2*100,digits = 3),"%"),
                      freq_3 = paste0(format(freq_3*100,digits = 3),"%"),
                      freq_4 = paste0(format(freq_4*100,digits = 3),"%"),
                      amount = paste0(total_cannabis_mean,"(",total_cannabis_IQR,")"),
                      thc = paste0(total_thc_mean,"(",total_thc_IQR,")"),
                      cast_risk = paste0(format(cast_risk*100,digits = 3),"%"))]

write.csv(suptab3, paste0("tabs/",Sys.Date(),"_SUP TAB 3_descriptives.csv"), row.names = F)
rm(suptab3, add)


##  4) SUPPLEMENTARY TABLE 4 - SENS ANALYSIS of Primary analyses model output
# -------------------------------------------------------

sjPlot::tab_model(pa_sa1_mod0,pa_sa1_mod1a,pa_sa1_mod1b,pa_sa1_mod2,
                  dv.labels = c("Model 0: Baseline", "Model 1a: Frequency only", "Model 1b: Total use quantities only",
                                "Model 2: Frequency and quantities on use days"),
                  file = paste0("tabs/",Sys.Date(),"_SUP TAB 4_sensitivity analyses of primary analyses output.html"))

##  5) SUPPLEMENTARY TABLE 5 - SENS ANALYSIS of Secondary analyses model output
# -------------------------------------------------------

sjPlot::tab_model(sa_sa2_mod0,sa_sa2_mod1a,sa_sa2_mod1b,sa_sa2_mod2,sa_sa2_mod3,
                  dv.labels = c("Model 0: Baseline", "Model 1a: Frequency only", "Model 1b: Total THC units only", 
                                "Model 2: Frequency and THC units on use days", "Model 3: Frequency, quantities on use days, and average THC concentration"),
                  file = paste0("tabs/",Sys.Date(),"_SUP TAB 5_sensitivity analyses of secondary analyses output.html"))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) FIGURES
# ______________________________________________________________________________________________________________________


##  1) Visual aid
# -------------------------------------------------------

# ...

##  2) Predicted vs observed CAST - primary analyses
# -------------------------------------------------------

pdat <- copy(data[sample1 == T,.(iso,sex,agegroup,total_cannabis = round(total_cannabis,1),observed = cast_tot)])
pdat$predicted <- predict(pa_mod1b, type = "response")
q <- quantile(pdat$total_cannabis,probs = seq(from = 0, to = 1, by = 0.05))
#q[1] <- q[1]-0.0001
pdat[, quant := cut(total_cannabis, q, include.lowest = T)]
pdat[is.na(quant)]

pdat$quant <- as.factor(pdat$quant)
levels(pdat$quant) <- gsub("\\[|\\]","",levels(pdat$quant))
levels(pdat$quant) <- gsub("\\(",">",levels(pdat$quant))
levels(pdat$quant) <- gsub("\\,"," to <=",levels(pdat$quant))

pdat <- melt(pdat, id.vars = c("iso","sex","agegroup","quant"), measure.vars = c("observed","predicted"))

ggplot(pdat, aes(x = quant, y = value, fill = variable)) +
  ggtitle("Primary analyses: Observed and predicted CAST score",
          "Prediction based on sex, age group and total amount of cannabis used (Model 1b)") +
  #geom_point(aes(group = variable), alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("5% groupings of cannabis use quantities in the past 30 days (in g)") + 
  scale_y_continuous("CAST score") +
  scale_fill_manual("", values = colors_2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.8),
        legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_1_CAST predicted by quantity.png"),
       width = 10, height = 7)
ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_1_CAST predicted by quantity.pdf"),
       width = 10, height = 7)



##  5) Predicted vs observed CAST - secondary analyses
# -------------------------------------------------------

pdat <- copy(data[sample1 == T & thc_units_total != 0,.(iso,sex,agegroup,thc_units_total = round(thc_units_total,1),observed = cast_tot)])
pdat$predicted <- predict(sa_mod1b, type = "response")
q <- quantile(pdat$thc_units_total,probs = seq(from = 0, to = 1, by = 0.05))
pdat[, units := cut(thc_units_total, q, include.lowest = T, dig.lab = 6)]
pdat[is.na(units)]

pdat$units <- as.factor(pdat$units)
levels(pdat$units) <- gsub("\\[|\\]","",levels(pdat$units))
levels(pdat$units) <- gsub("\\(",">",levels(pdat$units))
levels(pdat$units) <- gsub("\\,"," to <=",levels(pdat$units))

pdat <- melt(pdat, id.vars = c("iso","sex","agegroup","units"), measure.vars = c("observed","predicted"))

ggplot(pdat, aes(x = units, y = value, fill = variable)) +
  ggtitle("Secondary analyses: Observed and predicted CAST score",
          "Prediction based on sex, age group and number of THC units (Model 1b)") +
  #geom_point(aes(group = variable), alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("5% groupings of the number THC units in the past 30 days (1 unit = 5mg THC)") + 
  scale_y_continuous("CAST score") +
  scale_fill_manual("", values = colors_2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.8),
        legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_2_CAST predicted by THC unit.png"),
       width = 10, height = 6)
ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_2_CAST predicted by THC unit.pdf"),
       width = 10, height = 6)



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) SUPPLEMENTARY FIGURES
# ______________________________________________________________________________________________________________________

##  1) Visual aid
# -------------------------------------------------------


##  2) Flow of questionnaire
# -------------------------------------------------------


##  2) Differences of flower quantities by method
# -------------------------------------------------------

pdat <- copy(comp.flower[keep == T])
pdat$cast_tot <- factor(pdat$cast_tot)

p1 <- ggplot(pdat, aes(x = cw, y = cq)) + 
  ggtitle("A: scatter plot of flower use quantities") +
  geom_point() + 
  scale_x_continuous("crude weight method") + 
  scale_y_continuous("CannaQuant method")

p2 <- ggplot(pdat, aes(x = iso, y = diff)) +
  ggtitle("B: difference by country") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("Country") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

p3 <- ggplot(pdat, aes(x = freqgroup, y = diff)) + 
  ggtitle("C: difference by use frequency") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("Number of cannabis use days") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

p4 <- ggplot(pdat, aes(x = cast_tot, y = diff)) + 
  ggtitle("D: difference by CAST") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("CAST score") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

plot_grid(p1, p2, p3, p4, nrow = 2)

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_3_quantity comparison flower.png"),
       width = 15, height = 12)
rm(pdat, p1, p2, p3, p4)


##  4) Differences of resin quantities by method
# -------------------------------------------------------

pdat <- copy(comp.resin[keep == T])
pdat$cast_tot <- factor(pdat$cast_tot)

p1 <- ggplot(pdat, aes(x = cw, y = cq)) + 
  ggtitle("A: scatter plot of resin use quantities") +
  geom_point() + 
  scale_x_continuous("crude weight method") + 
  scale_y_continuous("CannaQuant method")

p2 <- ggplot(pdat, aes(x = iso, y = diff)) +
  ggtitle("B: difference by country") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("Country") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

p3 <- ggplot(pdat, aes(x = freqgroup, y = diff)) + 
  ggtitle("C: difference by use frequency") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("Number of cannabis use days") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

p4 <- ggplot(pdat, aes(x = cast_tot, y = diff)) + 
  ggtitle("D: difference by CAST") +
  geom_jitter(alpha = 0.4) + 
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete("CAST score") + 
  scale_y_continuous("Absolute Difference: crude weight - CannaQuant")

plot_grid(p1, p2, p3, p4, nrow = 2)

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_4_quantity comparison resin.png"),
       width = 15, height = 12)
rm(pdat, p1, p2, p3, p4)



##  5) RELATIONSHIP BETWEEN FREQUENCY AND QUANTITY
# -------------------------------------------------------

pdat <- copy(data[sample1 == T,.(country, freqgroup,total_cannabis_useday)])
q <- quantile(pdat$total_cannabis_useday,probs = seq(from = 0, to = 1, by = 0.20))
pdat[, quantgroup := cut(total_cannabis_useday, q, include.lowest = T)]
pdat[is.na(quantgroup)]
pdat$quantgroup <- as.factor(pdat$quantgroup)
levels(pdat$quantgroup) <- gsub("\\[|\\]","",levels(pdat$quantgroup))
levels(pdat$quantgroup) <- gsub("\\(",">",levels(pdat$quantgroup))
levels(pdat$quantgroup) <- gsub("\\,"," to <=",levels(pdat$quantgroup))

p1 <- ggplot(pdat, aes(x = country, y = total_cannabis_useday, fill = freqgroup)) + 
  geom_jitter(alpha = 0.4) + 
  geom_boxplot() + 
  scale_fill_manual("Number of use days",values = colors_6[1:4]) +
  scale_x_discrete("") +
  scale_y_continuous("Amount of cannabis used per use day") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

pdat[, table(freqgroup,quantgroup)]
pdat[, prop.table(table(freqgroup,quantgroup), 1)] # 
pdat[, N := .N, by = freqgroup]
pdat <- unique(pdat[, .(prop = .N/N), by = .(freqgroup,quantgroup)])

p2 <- ggplot(pdat, aes(x = freqgroup, y = prop, fill = quantgroup)) + 
  geom_col() + 
  scale_fill_manual("Quintiles of amount of cannabis used per use day (in g)", values = colors_6[1:5]) +
  scale_x_discrete("Number of use days in the past 30 days") +
  scale_y_continuous("%", labels = scales::percent) + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

plot_grid(p1, p2, ncol = 2)

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_5_frequency and quantity relationship.png"),
       width = 18, height = 8)
rm(pdat, p1, p2)



##  6) RELATIONSHIP BETWEEN FREQUENCY AND THC UNITS
# -------------------------------------------------------

pdat <- copy(data[sample1 == T & thc_units_useday != 0,.(country, freqgroup,thc_units_useday)])
q <- quantile(pdat$thc_units_useday,probs = seq(from = 0, to = 1, by = 0.20))
pdat[, unitgroup := cut(thc_units_useday, q, include.lowest = T)]
pdat[is.na(unitgroup)]
pdat$unitgroup <- as.factor(pdat$unitgroup)
levels(pdat$unitgroup) <- gsub("\\[|\\]","",levels(pdat$unitgroup))
levels(pdat$unitgroup) <- gsub("\\(",">",levels(pdat$unitgroup))
levels(pdat$unitgroup) <- gsub("\\,"," to <=",levels(pdat$unitgroup))

p1 <- ggplot(pdat, aes(x = country, y = thc_units_useday, fill = freqgroup)) + 
  geom_jitter(alpha = 0.4) + 
  geom_boxplot() + 
  scale_fill_manual("Number of use days",values = colors_6[1:4]) +
  scale_x_discrete("") +
  scale_y_continuous("Number of THC units per use day") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

pdat[, table(freqgroup,unitgroup)]
pdat[, prop.table(table(freqgroup,unitgroup), 1)] # 
pdat[, N := .N, by = freqgroup]
pdat <- unique(pdat[, .(prop = .N/N), by = .(freqgroup,unitgroup)])

p2 <- ggplot(pdat, aes(x = freqgroup, y = prop, fill = unitgroup)) + 
  geom_col() + 
  scale_fill_manual("Quintiles of number of THC units per use day (1 unit = 5mg THC)", values = colors_6[1:5]) +
  scale_x_discrete("Number of use days in the past 30 days") +
  scale_y_continuous("%", labels = scales::percent) + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1))

plot_grid(p1, p2, ncol = 2)

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_6_frequency and THC units relationship.png"),
       width = 18, height = 8)
rm(pdat, p1, p2)

data[!is.na(thc_avg), sum(thc_avg >=0.2)/.N, by = .(freqgroup == "29-30")]



