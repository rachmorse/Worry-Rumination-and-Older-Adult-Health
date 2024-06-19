# Load packages
if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych"); library(psych)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse"); library(tidyverse)
library(cowplot)
library(readxl)

# Read in data ----
data <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx",
  col_types = c(
    "text",
    rep("numeric", 19),
    "text", "numeric",
    rep("text", 2),
    rep("numeric", 10)))

##################################
# Clean the data for analysis ----
##################################

# Rename variables
data <- data %>%
  rename(
    psw = `Penn State Worry`, # Worry
    rrsb = `Rumination Response Scale Brooding`, # Ruminative brooding
    whoqol = `Quality of Life measure`, # Subjective physical health
    age = `Age at V1`, 
    sex = `gender`,
    sbp = `SBP (mmHg)`, # Objective physical health: systolic blood pressure
    dbp = `DBP (mmHg)`, # Objective physical health: diastolic blood pressure
    anx = `STAI-A score`, 
    depresh = `Geriatric Depression Scale - Global`,
    edu = `Level of education`,
    frs = `Framingham`, # Objective physical health: Adjusted Framingham Risk Score
    cci = `Adjusted CCI`, # Objective physical health: Adjusted Charleston Comorbidity Index
    cci_unadj = `Unadjusted CCI`, # Objective physical health: Unadjusted Charleston Comorbidity Index
    pacc = `PACC5 (STANDARDIZED with combined cohorts)`, # Objective cognitive health: Adjusted PACC5
    paccscdage = `PACC5 (for separate cohort analysis)`, # Objective cognitive health: Adjusted PACC5 (for use in SCD-Well or Age-Well seperately)
    cds = `Cognitive Difficulties Scale (CDS)`, # Subjective cognitive health
    ravlt_cvlt = `ravlt OR cvlt (in Age-Well) this column can only be use for separate cohort analyses` 
  )

# Create a study variable 
# Note this creates: 1 = SCD-Well and 2 = Age-Well
data <- data %>%
  mutate(study = ifelse(Country %in% c("London, UK", "Cologne, Germany", "Barcelone, Spain", "Lyon, France"), 1, 
                        ifelse(Country %in% c("France"), 2, NA)))

# Create a dataframe for Age-Well and SCD-Well
agewell <- data %>%
  filter(study == 2)

scdwell <- data %>%
  filter(study == 1)

# For the sensitivity analyses where Model A included demographics, 
# create a new df for when all worry data is available and another when all rumination data is available 
# so that Model A includes the same participants as Model B
all_psw <- data %>% 
  filter(!is.na(psw))

all_rrsb <- data %>% 
  filter(!is.na(rrsb))

######################################################
# Run descriptive stats for demographic variables ----
######################################################

## Ruminative brooding ##
attach(data)
describe(rrsb)
shapiro.test(rrsb)
ggplot(data, aes(x = rrsb))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)
  
describe(agewell$rrsb)
describe(scdwell$rrsb)

wilcox.test(agewell$rrsb, scdwell$rrsb) # Check significance of difference between cohorts 

## Worry ##
describe(psw)
shapiro.test(psw) 
ggplot(data, aes(x = psw))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$psw)
describe(scdwell$psw)

wilcox.test(agewell$psw, scdwell$psw) 

## Subjective physical health ##
describe(whoqol)
shapiro.test(whoqol) 
ggplot(data, aes(x = whoqol))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$whoqol)
describe(scdwell$whoqol)

wilcox.test(agewell$whoqol, scdwell$whoqol) 

## Age ##
describe(age)
shapiro.test(age) # positive skew
sd(age, na.rm = TRUE)

describe(agewell$age)
describe(scdwell$age)

wilcox.test(agewell$age, scdwell$age) 

## Education ##
describe(edu)
shapiro.test(edu) 
ggplot(data, aes(x = edu))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$edu)
describe(scdwell$edu)

wilcox.test(agewell$edu, scdwell$edu) 

## SBP  ##
describe(sbp)
shapiro.test(sbp) 
ggplot(data, aes(x = sbp))+
  geom_histogram(fill="lightpink", color="black", binwidth =10)

describe(agewell$sbp)
describe(scdwell$sbp)

wilcox.test(agewell$sbp, scdwell$sbp) 

## DBP  ##
describe(dbp)
shapiro.test(dbp) 
ggplot(data, aes(x = dbp))+
  geom_histogram(fill="lightpink", color="black", binwidth =5)

describe(agewell$dbp)
describe(scdwell$dbp)

wilcox.test(agewell$dbp, scdwell$dbp) 

## Anxiety  ##
describe(anx)
shapiro.test(anx) 
ggplot(data, aes(x = anx))+
  geom_histogram(fill="lightpink", color="black", binwidth =2)

describe(agewell$anx)
describe(scdwell$anx)

wilcox.test(agewell$anx, scdwell$anx) 

## Depression  ##
describe(depresh)
shapiro.test(depresh) 
ggplot(data, aes(x = depresh))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$depresh)
describe(scdwell$depresh)

wilcox.test(agewell$depresh, scdwell$depresh) 

## Sex ##
describe(sex)
describe(agewell$sex)
describe(scdwell$sex)
chisq.test(data$sex, data$study) # Check significance of difference between cohorts

## Subjective cognitive complaints ##
describe(cds)
shapiro.test(cds) # positive skew
ggplot(data, aes(x = cds))+
  geom_histogram(fill="lightpink", color="black", binwidth =5)

describe(agewell$cds)
describe(scdwell$cds)

wilcox.test(agewell$cds, scdwell$cds) # Check significance of difference between cohorts

## PACC5 ##
describe(pacc)
shapiro.test(pacc)
ggplot(data, aes(x = pacc))+
  geom_histogram(fill="lightpink", color="black", binwidth =0.5)

describe(agewell$paccscdage)
describe(scdwell$paccscdage)

wilcox.test(agewell$pacc, scdwell$pacc) # Check significance of difference between cohorts

## PACC5 components ## 

  ## RAVLT / CVLT ##
  
  # First transform CVLT in Age-Well to be comparable with RAVLT in SCD-Well and name it cvlt_adj
  agewell <- agewell %>%
    filter(!is.na(ravlt_cvlt)) %>%
    mutate(cvlt_adj = (ravlt_cvlt / 16 * 15))
  
  # Merge the two subset RAVLT and CVLT tests to be able to compare 
  agewell_ravlt_cvlt <- agewell %>%
    select(ID, cvlt_adj)  %>%
    rename(ravlt_cvlt = cvlt_adj) 
  
  scdwell_ravlt_cvlt <- scdwell %>%
    select(ID, ravlt_cvlt) 
  
  ravlt_cvlt_df <- merge(agewell_ravlt_cvlt, scdwell_ravlt_cvlt, by = c("ID", "ravlt_cvlt"), all = TRUE)
  
  # Now look at the stats 
  describe(ravlt_cvlt_df$ravlt_cvlt)
  shapiro.test(ravlt_cvlt_df$ravlt_cvlt) 
  ggplot(ravlt_cvlt_df, aes(x = ravlt_cvlt))+
    geom_histogram(fill="lightpink", color="black", binwidth =1)
  
  describe(agewell$cvlt_adj)
  describe(scdwell$ravlt_cvlt)
  
  wilcox.test(agewell$cvlt_adj, scdwell$ravlt_cvlt) # Check significance of difference between cohorts
  
  ## Categorical fluency ##
  describe(fluency)
  shapiro.test(fluency) 
  ggplot(data, aes(x = fluency))+
    geom_histogram(fill="lightpink", color="black", binwidth =1)
  
  describe(agewell$fluency)
  describe(scdwell$fluency)
  
  wilcox.test(agewell$fluency, scdwell$fluency) # Check significance of difference between cohorts
  
  ## WAIS ##
  describe(coding)
  shapiro.test(coding) 
  ggplot(data, aes(x = coding))+
    geom_histogram(fill="lightpink", color="black", binwidth =1)
  
  describe(agewell$coding)
  describe(scdwell$coding)
  
  wilcox.test(agewell$coding, scdwell$coding) # Check significance of difference between cohorts
  
  ## Dementia Rating Scale ##
  describe(drs)
  shapiro.test(drs) 
  ggplot(data, aes(x = drs))+
    geom_histogram(fill="lightpink", color="black", binwidth =1)
  
  describe(agewell$drs)
  describe(scdwell$drs)
  
  wilcox.test(agewell$drs, scdwell$drs) # Check significance of difference between cohorts

## CCI ##
describe(cci)
shapiro.test(cci)
ggplot(data, aes(x = cci))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$cci)
describe(scdwell$cci)

wilcox.test(agewell$cci, scdwell$cci) # Check significance of difference between cohorts

## FRS ##
describe(frs)
shapiro.test(frs)
ggplot(data, aes(x = frs))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$frs)
describe(scdwell$frs)

wilcox.test(agewell$frs, scdwell$frs) # Check significance of difference between cohorts

###########################################################
# LINEAR REGRESSIONS SUBJECTIVE PHYSICAL HEALTH WHOQOL ----
###########################################################

## AGE-WELL AND SCD-WELL TOGETHER SUBJECTIVE PHYSICAL HEALTH 

# This is model 1
W1 <- lm(scale(whoqol) ~ scale(psw), data = data)
summary(W1)
confint(W1)

# This is model 2 / B (they are the same)
W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(W2)
confint(W2)

# This is the supplementary model with anxiety  
W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(anx), data = data)
summary(W3)
confint(W3)

# This is model A 
W4 <- lm(scale(whoqol) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(W4)
confint(W4)

# Note the pattern of models is the same for all combined cohort analyses

# This is model 1
B1 <- lm(scale(whoqol) ~ scale(rrsb), data = data)
summary(B1)
confint(B1)

# This is model 2 / B (they are the same)
B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(B2)
confint(B2)

# This is the supplementary model with anxiety  
B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = data)
summary(B3)
confint(B3)

# This is model A 
B4 <- lm(scale(whoqol) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(B4)
confint(B4)

# SCD-Well WORRY / WHOQOL

scdwell_W1 <- lm(scale(whoqol) ~ scale(psw), data = scdwell)
summary(scdwell_W1)
confint(scdwell_W1)

scdwell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_W2)
confint(scdwell_W2)

scdwell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_W3)
confint(scdwell_W3)

# Age-Well WORRY / WHOQOL
agewell_W1 <- lm(scale(whoqol) ~ scale(psw), data = agewell)
summary(agewell_W1)
confint(agewell_W1)

agewell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_W2)
confint(agewell_W2)

agewell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_W3)
confint(agewell_W3)

# SCD-Well BROODING / WHOQOL

scdwell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = scdwell)
summary(scdwell_B1)
confint(scdwell_B1)

scdwell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_B2)
confint(scdwell_B2)

scdwell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_B3)
confint(scdwell_B3)

# Age-Well BROODING / WHOQOL

agewell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = agewell)
summary(agewell_B1)
confint(agewell_B1)

agewell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_B2)
confint(agewell_B2)

agewell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_B3)
confint(agewell_B3)

##################################################################
# LINEAR REGRESSIONS OBJECTIVE PHYSICAL HEALTH BLOOD PRESSURE ----
##################################################################

# AGE-WELL & SCD-WELL WORRY / SBP

agescd_sbp1 <- lm(scale(sbp) ~ scale(psw), data = data)
summary(agescd_sbp1)
confint(agescd_sbp1)

agescd_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_sbp2)
confint(agescd_sbp2)

agescd_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = data)
summary(agescd_sbp3)
confint(agescd_sbp3)

agescd_sbp4 <- lm(scale(sbp) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(agescd_sbp4)
confint(agescd_sbp4)

# AGE-WELL & SCD-WELL BROODING / SBP

agescd_sbp5 <- lm(scale(sbp) ~ scale(rrsb), data = data)
summary(agescd_sbp5)
confint(agescd_sbp5)

agescd_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_sbp6)
confint(agescd_sbp6)

agescd_sbp7 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = data)
summary(agescd_sbp7)
confint(agescd_sbp7)

agescd_sbp8 <- lm(scale(sbp) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(agescd_sbp8)
confint(agescd_sbp8)

# SCD-Well WORRY / SBP

scdwell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = scdwell)
summary(scdwell_sbp1)
confint(scdwell_sbp1)

scdwell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_sbp2)
confint(scdwell_sbp2)

scdwell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_sbp3)
confint(scdwell_sbp3)

# Age-Well WORRY / SBP

agewell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = agewell)
summary(agewell_sbp1)
confint(agewell_sbp1)

agewell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_sbp2)
confint(agewell_sbp2)

agewell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_sbp3)
confint(agewell_sbp3)

# SCD-Well BROODING / SBP

scdwell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = scdwell)
summary(scdwell_sbp4)
confint(scdwell_sbp4)

scdwell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_sbp5)
confint(scdwell_sbp5)

scdwell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_sbp6)
confint(scdwell_sbp6)

# Age-Well BROODING / SBP

agewell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = agewell)
summary(agewell_sbp4)
confint(agewell_sbp4)

agewell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_sbp5)
confint(agewell_sbp5)

agewell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_sbp6)
confint(agewell_sbp6)

# AGE-WELL & SCD-WELL WORRY / DBP

agescd_dbp1 <- lm(scale(dbp) ~ scale(psw), data = data)
summary(agescd_dbp1)
confint(agescd_dbp1)

agescd_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_dbp2)
confint(agescd_dbp2)

agescd_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = data)
summary(agescd_dbp3)
confint(agescd_dbp3)

agescd_dbp4 <- lm(scale(dbp) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(agescd_dbp4)
confint(agescd_dbp4)

# AGE-WELL & SCD-WELL BROODING / DBP

agescd_dbp5 <- lm(scale(dbp) ~ scale(rrsb), data = data)
summary(agescd_dbp5)
confint(agescd_dbp5)

agescd_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_dbp6)
confint(agescd_dbp6)

agescd_dbp7 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = data)
summary(agescd_dbp7)
confint(agescd_dbp7)

agescd_dbp8 <- lm(scale(dbp) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(agescd_dbp8)
confint(agescd_dbp8)

# SCD-Well WORRY / DBP

scdwell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = scdwell)
summary(scdwell_dbp1)
confint(scdwell_dbp1)

scdwell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_dbp2)
confint(scdwell_dbp2)

scdwell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_dbp3)
confint(scdwell_dbp3)

# Age-Well WORRY / DBP

agewell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = agewell)
summary(agewell_dbp1)
confint(agewell_dbp1)

agewell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_dbp2)
confint(agewell_dbp2)

agewell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_dbp3)
confint(agewell_dbp3)

# SCD-Well BROODING / DBP

scdwell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = scdwell)
summary(scdwell_dbp4)
confint(scdwell_dbp4)

scdwell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_dbp5)
confint(scdwell_dbp5)

scdwell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_dbp6)
confint(scdwell_dbp6)

# Age-Well BROODING / DBP

agewell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = agewell)
summary(agewell_dbp4)
confint(agewell_dbp4)

agewell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_dbp5)
confint(agewell_dbp5)

agewell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_dbp6)
confint(agewell_dbp6)

#######################################################
# LINEAR REGRESSIONS OBJECTIVE PHYSICAL HEALTH CCI ----
#######################################################

# AGE-WELL & SCD-WELL WORRY / CCI

agescd_cci1 <- lm(scale(cci) ~ scale(psw), data = data)
summary(agescd_cci1)
confint(agescd_cci1)

agescd_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_cci2)
confint(agescd_cci2)

agescd_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = data)
summary(agescd_cci3)
confint(agescd_cci3)

agescd_cci4 <- lm(scale(cci) ~ scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(agescd_cci4)
confint(agescd_cci4)

# AGE-WELL & SCD-WELL BROODING / CCI

agescd_cci5 <- lm(scale(cci) ~ scale(rrsb), data = data)
summary(agescd_cci5)
confint(agescd_cci5)

agescd_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(study), data = data)
summary(agescd_cci6)
confint(agescd_cci6)

agescd_cci7 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = data)
summary(agescd_cci7)
confint(agescd_cci7)

agescd_cci8 <- lm(scale(cci) ~ scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(agescd_cci8)
confint(agescd_cci8)

# SCD-Well WORRY / CCI

scdwell_cci1 <- lm(scale(cci) ~ scale(psw), data = scdwell)
summary(scdwell_cci1)
confint(scdwell_cci1)

scdwell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cci2)
confint(scdwell_cci2)

scdwell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_cci3)
confint(scdwell_cci3)

# Age-Well WORRY / CCI

agewell_cci1 <- lm(scale(cci) ~ scale(psw), data = agewell)
summary(agewell_cci1)
confint(agewell_cci1)

agewell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cci2)
confint(agewell_cci2)

agewell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_cci3)
confint(agewell_cci3)

# SCD-Well BROODING / CCI

scdwell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = scdwell)
summary(scdwell_cci4)
confint(scdwell_cci4)

scdwell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cci5)
confint(scdwell_cci5)

scdwell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_cci6)
confint(scdwell_cci6)

# Age-Well BROODING / CCI

agewell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = agewell)
summary(agewell_cci4)
confint(agewell_cci4)

agewell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cci5)
confint(agewell_cci5)

agewell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_cci6)
confint(agewell_cci6)

##########################################################
# LINEAR REGRESSIONS OBJECTIVE PHYSICAL HEALTH FRS ----
##########################################################

# AGE-WELL & SCD-WELL WORRY / FRS

agescd_frs1 <- lm(scale(frs) ~ scale(psw), data = data)
summary(agescd_frs1)
confint(agescd_frs1)

agescd_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(study), data = data)
summary(agescd_frs2)
confint(agescd_frs2)

agescd_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx) + scale(study), data = data)
summary(agescd_frs3)
confint(agescd_frs3)

agescd_frs4 <- lm(scale(frs) ~ scale(edu) + scale(study), data = all_psw)
summary(agescd_frs4)
confint(agescd_frs4)

# AGE-WELL & SCD-Well BROODING / FRS

agescd_frs4 <- lm(scale(frs) ~ scale(rrsb), data = data)
summary(agescd_frs4)
confint(agescd_frs4)

agescd_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(study), data = data)
summary(agescd_frs5)
confint(agescd_frs5)

agescd_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh) + scale(study), data = data)
summary(agescd_frs6)
confint(agescd_frs6)

agescd_frs7 <- lm(scale(frs) ~ scale(edu) + scale(study), data = all_rrsb)
summary(agescd_frs7)
confint(agescd_frs7)

# SCD-Well WORRY / FRS

scdwell_frs1 <- lm(scale(frs) ~ scale(psw), data = scdwell)
summary(scdwell_frs1)
confint(scdwell_frs1)

scdwell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = scdwell)
summary(scdwell_frs2)
confint(scdwell_frs2)

scdwell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_frs3)
confint(scdwell_frs3)

# Age-Well WORRY / FRS

agewell_frs1 <- lm(scale(frs) ~ scale(psw), data = agewell)
summary(agewell_frs1)
confint(agewell_frs1)

agewell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = agewell)
summary(agewell_frs2)
confint(agewell_frs2)

agewell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = agewell)
summary(agewell_frs3)
confint(agewell_frs3)

# SCD-Well BROODING / FRS

scdwell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = scdwell)
summary(scdwell_frs4)
confint(scdwell_frs4)

scdwell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = scdwell)
summary(scdwell_frs5)
confint(scdwell_frs5)

scdwell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_frs6)
confint(scdwell_frs6)

# Age-Well BROODING / FRS

agewell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = agewell)
summary(agewell_frs4)
confint(agewell_frs4)

agewell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = agewell)
summary(agewell_frs5)
confint(agewell_frs5)

agewell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_frs6)
confint(agewell_frs6)

#########################################################
# LINEAR REGRESSION SUBJECTIVE COGNITIVE HEALTH CDS  ----
#########################################################

# AGE-WELL & SCD-WELL WORRY / CDS

WC1 <- lm(scale(cds) ~ scale(psw), data = data)
summary(WC1)
confint(WC1)

WC2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(WC2)
confint(WC2)

WC3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = data)
summary(WC3)
confint(WC3)

WC4 <- lm(scale(cds) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(WC4)
confint(WC4)

# AGE-WELL & SCD-WELL BROODING / CDS
BC1 <- lm(scale(cds) ~ scale(rrsb), data = data)
summary(BC1)
confint(BC1)

BC2 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(BC2)
confint(BC2)

BC3 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = data)
summary(BC3)
confint(BC3)

BC4 <- lm(scale(cds) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(BC4)
confint(BC4)

# SCD-Well WORRY / CDS

scdwell_cds1 <- lm(scale(cds) ~ scale(psw), data = scdwell)
summary(scdwell_cds1)
confint(scdwell_cds1)

scdwell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cds2)
confint(scdwell_cds2)

scdwell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_cds3)
confint(scdwell_cds3)

# Age-Well WORRY / CDS

agewell_cds1 <- lm(scale(cds) ~ scale(psw), data = agewell)
summary(agewell_cds1)
confint(agewell_cds1)

agewell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cds2)
confint(agewell_cds2)

agewell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_cds3)
confint(agewell_cds3)

# SCD-Well BROODING / CDS

scdwell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = scdwell)
summary(scdwell_cds4)
confint(scdwell_cds4)

scdwell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cds5)
confint(scdwell_cds5)

scdwell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_cds6)
confint(scdwell_cds6)

# Age-Well BROODING / CDS

agewell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = agewell)
summary(agewell_cds4)
confint(agewell_cds4)

agewell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cds5)
confint(agewell_cds5)

agewell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_cds6)
confint(agewell_cds6)

#########################################################
# LINEAR REGRESSIONS OBJECTIVE COGNITIVE HEALTH PACC ----
#########################################################

# AGE-WELL & SCD-WELL WORRY / PACC5 

WPACC1 <- lm(scale(pacc) ~ scale(psw), data = data)
summary(WPACC1)
confint(WPACC1)

WPACC2 <- lm(scale(pacc) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(WPACC2)
confint(WPACC2)

WPACC3 <- lm(scale(pacc) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(anx), data = data)
summary(WPACC3)
confint(WPACC3)

WPACC4 <- lm(scale(pacc) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_psw)
summary(WPACC4)
confint(WPACC4)

# AGE-WELL & SCD-WELL BROODING / PACC5 

BPACC1 <- lm(scale(pacc) ~ scale(rrsb), data = data)
summary(BPACC1)
confint(BPACC1)

BPACC2 <- lm(scale(pacc) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(BPACC2)
confint(BPACC2)

BPACC3 <- lm(scale(pacc) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = data)
summary(BPACC3)
confint(BPACC3)

BPACC4 <- lm(scale(pacc) ~ scale(age) + scale(sex) + scale(edu) + scale(study), data = all_rrsb)
summary(BPACC4)
confint(BPACC4)

# SCD-Well WORRY / PACC

scdwell_WPACC1 <- lm(scale(paccscdage) ~ scale(psw), data = scdwell)
summary(scdwell_WPACC1)
confint(scdwell_WPACC1)

scdwell_WPACC2 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_WPACC2)
confint(scdwell_WPACC2)

scdwell_WPACC3 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_WPACC3)
confint(scdwell_WPACC3)

# Age-Well WORRY / PACC

agewell_WPACC1 <- lm(scale(paccscdage) ~ scale(psw), data = agewell)
summary(agewell_WPACC1)
confint(agewell_WPACC1)

agewell_WPACC2 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_WPACC2)
confint(agewell_WPACC2)

agewell_WPACC3 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_WPACC3)
confint(agewell_WPACC3)


# SCD-Well BROODING / PACC

scdwell_BPACC1 <- lm(scale(paccscdage) ~ scale(rrsb), data = scdwell)
summary(scdwell_BPACC1)
confint(scdwell_BPACC1)

scdwell_BPACC2 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_BPACC2)
confint(scdwell_BPACC2)

scdwell_BPACC3 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_BPACC3)
confint(scdwell_BPACC3)

# Age-Well BROODING / PACC

agewell_BPACC1 <- lm(scale(paccscdage) ~ scale(rrsb), data = agewell)
summary(agewell_BPACC1)
confint(agewell_BPACC1)

agewell_BPACC2 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_BPACC2)
confint(agewell_BPACC2)

agewell_BPACC3 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_BPACC3)
confint(agewell_BPACC3)

#######################################################################
# Sensitivity analyses with worry and rumination in the same model ----
#######################################################################

WRCDS1 <- lm(scale(cds) ~ scale(psw) + scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(WRCDS1)
confint(WRCDS1)

WRWHOQOL1 <- lm(scale(whoqol) ~ scale(psw) + scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = data)
summary(WRWHOQOL1)
confint(WRWHOQOL1)

###########################
# Correlation analyses ----
###########################

# Pearson correlation CCI adjusted and unadjusted
corr_cci <- cor.test(cci, cci_unadj, method = "pearson")

print(paste(corr_cci$conf.int[1], "-", corr_cci$conf.int[2]))
print(paste(corr_cci$p.value))

# Pearson correlation worry & rumination
corr_pswrrsb <- cor.test(psw, rrsb, method = ("pearson"))

print(paste(corr_pswrrsb$conf.int[1], "-", corr_pswrrsb$conf.int[2]))
print(paste(corr_pswrrsb$p.value))

################################################################################################
# Sensitivity analyses using the unadjusted CCI, PACC5, and FRS in the Age-Well cohort only ----
################################################################################################

## PACC5 ##
unadj_pacc_rrs1 <- lm(scale(`Unadjusted PACC5`) ~ scale(rrsb), data = data)
summary(unadj_pacc_rrs1)
confint(unadj_pacc_rrs1)

unadj_pacc_rrs2 <- lm(scale(`Unadjusted PACC5`) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = data)
summary(unadj_pacc_rrs2)
confint(unadj_pacc_rrs2)

unadj_pacc_psw1 <- lm(scale(`Unadjusted PACC5`) ~ scale(psw), data = data)
summary(unadj_pacc_psw1)
confint(unadj_pacc_psw1)

unadj_pacc_psw2 <- lm(scale(`Unadjusted PACC5`) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = data)
summary(unadj_pacc_psw2)
confint(unadj_pacc_psw2)

## FRS ##
unadj_frs_rrs1 <- lm(scale(`Unadjusted FRS`) ~ scale(rrsb), data = data)
summary(unadj_frs_rrs1)
confint(unadj_frs_rrs1)

unadj_frs_rrs2 <- lm(scale(`Unadjusted FRS`) ~ scale(rrsb) + scale(edu), data = data)
summary(unadj_frs_rrs2)
confint(unadj_frs_rrs2)

unadj_frs_psw1 <- lm(scale(`Unadjusted FRS`) ~ scale(psw), data = data)
summary(unadj_frs_psw1)
confint(unadj_frs_psw1)

unadj_frs_psw2 <- lm(scale(`Unadjusted FRS`) ~ scale(psw) + scale(edu), data = data)
summary(unadj_frs_psw2)
confint(unadj_frs_psw2)

## CCI ##
unadj_cci_rrs1 <- lm(scale(cci_unadj) ~ scale(rrsb), data = data)
summary(unadj_cci_rrs1)
confint(unadj_cci_rrs1)

unadj_cci_rrs2 <- lm(scale(cci_unadj) ~ scale(rrsb) + scale(sex) + scale(edu), data = data)
summary(unadj_cci_rrs2)
confint(unadj_cci_rrs2)

unadj_cci_psw1 <- lm(scale(cci_unadj) ~ scale(psw), data = data)
summary(unadj_cci_psw1)
confint(unadj_cci_psw1)

unadj_cci_psw2 <- lm(scale(cci_unadj) ~ scale(psw) + scale(sex) + scale(edu), data = data)
summary(unadj_cci_psw2)
confint(unadj_cci_psw2)

###########################################################
# Figures for subjective physical and cognitive health ----
###########################################################

# Define a function to create the plots
create_plot <- function(x, y, x_label, y_label) {
  ggplot(data, aes(x = x, y = y)) +
    geom_point(size = 2.5) +  
    geom_smooth(method = "lm", se = TRUE, 
                color = "#86B6F6", 
                fill = "#B4D4FF") +  
    labs(x = x_label, y = y_label) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm")) 
}

# Create the plots
p1 <- create_plot(data$psw, data$whoqol, "Worry", "Subjective Physical Health")
p2 <- create_plot(data$rrsb, data$whoqol, "Ruminative Brooding", "Subjective Physical Health")
p3 <- create_plot(data$psw, data$cds, "Worry", "Subjective Cognitive Difficulties")
p4 <- create_plot(data$rrsb, data$cds, "Ruminative Brooding", "Subjective Cognitive Difficulties")

# Combine the plots into one figure
combined_physical_plot <- plot_grid(p1, p2, ncol = 1) 
combined_physical_plot # View plot

combined_cognitive_plot <- plot_grid(p3, p4, ncol = 1) 
combined_cognitive_plot # View plot

# Save the plots
ggsave("Fig 1.jpeg", plot = combined_physical_plot, width = 6, height = 8, dpi = 400, units = "in")
ggsave("Fig 2.jpeg", plot = combined_cognitive_plot, width = 6, height = 8, dpi = 400, units = "in")

