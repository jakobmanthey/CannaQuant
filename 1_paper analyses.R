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

# DATE last version of prepared data
DATE <- "2024-12-09"


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
dim(input) # 1530 * 23

data <- copy(input)

##  sample restrictions
unique(data[,.(country,resp_total)])[, sum(resp_total)] # 3541
unique(data[,.(country,resp_sexage)])[, sum(resp_sexage)] # 1530
nrow(data) # 1530
3541-1530 # 2011  
nrow(data[!is.na(freqgroup)]) # 1422 --> first analytical sample
3541-1422 # 2119 
data[, sample1 := !is.na(freqgroup)]
data <- data[sample1 == T]

nrow(data[edu.complete == T & !is.na(freqgroup)]) # 995
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
nrow(comp.flower) # 831
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
psych::ICC(comp.flower[,.(cw,cq)]) # single: 0.25 / average rater: 0.4

# remove outliers:
comp.flower[abs(diff) > 30] # n=120
comp.flower[, keep := !abs(diff) > 30] 
hist(comp.flower[keep == T]$diff) # approximately normal with long tails
nrow(comp.flower[keep == F]) / nrow(comp.flower) # 14% outliers

# SUBSAMPLE without OUTLIERS: differences and variation country, freq, cast
nrow(comp.flower[keep == T]) # 711
comp.flower[keep == T, .(median = median(diff),
                         p25 = quantile(diff, 0.25),
                         p75 = quantile(diff, 0.75))]
psych::ICC(comp.flower[keep == T,.(cw,cq)]) # single: 0.91 / average rater: 0.95
comp.flower[keep == T, median(diff), by = iso]
comp.flower[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.flower[keep == T, median(diff), by = cast_tot][order(cast_tot)]

summary(lm(diff ~ iso + freqgroup + cast_tot, comp.flower[keep == T]))

# OUTLIER analyses: differences and variation country, freq, cast
summary(glm(keep==F ~ iso + sex + agegroup + edugroup + freqgroup + cast_tot, comp.flower, family = "binomial"))
# freqgroup: exp(1.2055) = 3.34
# CAST: exp(0.11618) = 1.12
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
psych::ICC(comp.resin[,.(cw,cq)]) # single: 0.03 / average rater: 0.05

# remove outliers:
comp.resin[abs(diff) > 30] # n=32
comp.resin[, keep := !abs(diff) > 30] 
hist(comp.resin[keep == T]$diff) # approximately normal
nrow(comp.resin[keep == F]) / nrow(comp.resin) # 14% outliers

# SUBSAMPLE without OUTLIERS: differences and variation country, freq, cast
nrow(comp.resin[keep == T]) # 97
comp.resin[keep == T, .(median = median(diff),
                         p25 = quantile(diff, 0.25),
                         p75 = quantile(diff, 0.75))]
psych::ICC(comp.resin[keep == T,.(cw,cq)]) # single: 0.91 / average rater: 0.95
comp.resin[keep == T, median(diff), by = iso]
comp.resin[keep == T, median(diff), by = freqgroup][order(freqgroup)]
comp.resin[keep == T, median(diff), by = cast_tot][order(cast_tot)]

summary(lm(diff ~ iso + freqgroup + cast_tot, comp.resin[keep == T]))

# OUTLIER analyses: differences and variation country, freq, cast
summary(glm(keep==F ~ iso + sex + agegroup + edugroup + freqgroup + cast_tot, comp.resin, family = "binomial"))
# country-sweden: exp(3.1980) = 24.48
# sex-women: exp(-2.2044) = 0.11
# freqgroup: exp(3.7646) = 43.15

# repeat without education
summary(glm(keep==F ~ iso + sex + agegroup + freqgroup + cast_tot, comp.resin, family = "binomial")) # Germany additional 

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

##  1) Baseline Model
# -------------------------------------------------------

# 0: baseline
pa_mod0 <- glmer(cast_tot ~ sex + agegroup + (1|iso), data[sample1 == T], family = "poisson")
summary(pa_mod0)

# 1a: frequency
pa_mod1a <- glmer(cast_tot ~ sex + agegroup + freqgroup + (1|iso), data[sample1 == T], family = "poisson")
summary(pa_mod1a)

# 1b: quantity (total)
pa_mod1b <- glmer(cast_tot ~ sex + agegroup + log(total_cannabis) + (1|iso), data[sample1 == T & total_cannabis>0], family = "poisson")
summary(pa_mod1b)





# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) TABLES
# ______________________________________________________________________________________________________________________

##  1) ...
# -------------------------------------------------------




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) SUPPLEMENTARY TABLES
# ______________________________________________________________________________________________________________________

##  1) SUPPLEMENTARY TABLE 1 - information on data collection
# -------------------------------------------------------


##  3) SUPPLEMENTARY TABLE 2 - descriptives
# -------------------------------------------------------

suptab2 <- data[edu.complete == T, .(.N,
                                     share_men = sum(sex == "men")/.N,
                                     age_mean = format(mean(age),digits = 3, nsmall = 1),
                                     #age_median = median(age),
                                     age_IQR = paste0(format(quantile(age,0.25),digits = 2),"-",format(quantile(age,0.75),digits = 2)),
                                     edu_low = sum(edugroup == "low")/.N,
                                     edu_high = sum(edugroup == "high")/.N,
                                     freq_high = sum(freqgroup == "29-30",na.rm = T)/sum(!is.na(freqgroup)),
                                     #freq_N = sum(!is.na(freqgroup)),
                                     pref_type = sum(pref_type == "flower")/.N,
                                     cast_risk = sum(highrisk == "high")/.N),
                by = country]

add <- data[edu.complete == T, .(country = "Total",
                                 .N,
                                 share_men = sum(sex == "men")/.N,
                                 age_mean = format(mean(age),digits = 3, nsmall = 1),
                                 #age_median = median(age),
                                 age_IQR = paste0(format(quantile(age,0.25),digits = 2),"-",format(quantile(age,0.75),digits = 2)),
                                 edu_low = sum(edugroup == "low")/.N,
                                 edu_high = sum(edugroup == "high")/.N,
                                 freq_high = sum(freqgroup == "29-30",na.rm = T)/sum(!is.na(freqgroup)),
                                 #freq_N = sum(!is.na(freqgroup)),
                                 pref_type = sum(pref_type == "flower")/.N,
                                 cast_risk = sum(highrisk == "high")/.N)]

suptab2 <- rbind(suptab2, add)
suptab2$country <- factor(suptab2$country) 

suptab2 <- suptab2[,.(country,
                      N,
                      sex = paste0(format(share_men*100,digits = 3),"%"),
                      age = paste0(age_mean,"(",age_IQR,")"),
                      edu_low = paste0(format(edu_low*100,digits = 3),"%"),
                      edu_high = paste0(format(edu_high*100,digits = 3),"%"),
                      freq_high = paste0(format(freq_high*100,digits = 3),"%"), # (N=",freq_N,")"),
                      pref_type = paste0(format(pref_type*100,digits = 3),"%"),
                      cast_risk = paste0(format(cast_risk*100,digits = 3),"%"))]

write.csv(suptab2, paste0("tabs/",Sys.Date(),"_SUPP TAB 2_descriptives.csv"), row.names = F)
rm(suptab2, add)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) FIGURES
# ______________________________________________________________________________________________________________________

##  1) ...
# -------------------------------------------------------




##  1) ...
# -------------------------------------------------------



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) SUPPLEMENTARY FIGURES
# ______________________________________________________________________________________________________________________

##  1) Differences of flower quantities by method
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

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_1_quantity comparison flower.png"),
       width = 15, height = 12)
rm(pdat, p1, p2, p3, p4)


##  2) Differences of resin quantities by method
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

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_2_quantity comparison resin.png"),
       width = 15, height = 12)
rm(pdat, p1, p2, p3, p4)







