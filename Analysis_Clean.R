# Load packages
if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych"); library(psych)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse"); library(tidyverse)

# Reading in data ----
getwd()
setwd("Users/rachelmorse/Documents/2021:2022/Marchant Lab/Data")
library(readxl)
Data_Summary <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx",
  col_types = c(
    "text",
    rep("numeric", 19),
    "text", "numeric",
    rep("text", 2),
    rep("numeric", 10)
  )
)

# Rename variables----
Data_Summary <- Data_Summary %>%
  rename(
    psw = `Penn State Worry`,
    rrsb = `Rumination Response Scale Brooding`,
    whoqol = `Quality of Life measure`, 
    age = `Age at V1`,
    sex = `gender`,
    sbp = `SBP (mmHg)`,
    dbp = `DBP (mmHg)`,
    anx = `STAI-A score`,
    depresh = `Geriatric Depression Scale - Global`,
    edu = `Level of education`,
    frs = `Framingham`,
    cci = `Adjusted CCI`,
    cci_unadj = `Unadjusted CCI`,
    pacc = `PACC5 (STANDARDIZED with combined cohorts)`,
    paccscdage = `PACC5 (for separate cohort analysis)`,
    cds = `Cognitive Difficulties Scale (CDS)`
  )

### Create a study variable
# Note this creates: 1 = SCD-Well and 2 = Age-Well
Data_Summary <- Data_Summary %>%
  mutate(study = if_else(Country %in% c("London, UK", "Cologne, Germany", "Barcelone, Spain", "Lyon, France"), 1, 2))

# Create a dataframe for Age-Well and SCD-Well 
agewell <- slice(Data_Summary, 148:282)
scdwell <- slice(Data_Summary, 1:147)

# Descriptive stats of demographic variables ----

## RRSB ##
attach(Data_Summary)
describe(rrsb)
shapiro.test(rrsb)
ggplot(Data_Summary, aes(x = rrsb))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)
  
describe(agewell$rrsb)

describe(scdwell$rrsb)

wilcox.test(agewell$rrsb, scdwell$rrsb) # Check significance of difference between cohorts 

## PSW ##
describe(psw)
shapiro.test(psw) 
ggplot(Data_Summary, aes(x = psw))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$psw)

describe(scdwell$psw)

wilcox.test(agewell$psw, scdwell$psw) # Check significance of difference between cohorts 

## whoqol ##
describe(whoqol)
shapiro.test(whoqol) 
ggplot(Data_Summary, aes(x = whoqol))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$whoqol)

describe(scdwell$whoqol)

wilcox.test(agewell$whoqol, scdwell$whoqol) # Check significance of difference between cohorts 

## age ##
describe(age)
shapiro.test(age) # positive skew
sd(age, na.rm = TRUE)

describe(agewell$age)

describe(scdwell$age)

wilcox.test(agewell$age, scdwell$age) # Check significance of difference between cohorts 

## education ##
describe(edu)
shapiro.test(edu) 
ggplot(Data_Summary, aes(x = edu))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$edu)

describe(scdwell$edu)

wilcox.test(agewell$edu, scdwell$edu) # Check significance of difference between cohorts 

## sbp  ##
describe(sbp)
shapiro.test(sbp) 
ggplot(Data_Summary, aes(x = sbp))+
  geom_histogram(fill="lightpink", color="black", binwidth =10)

describe(agewell$sbp)

describe(scdwell$sbp)

wilcox.test(agewell$sbp, scdwell$sbp) # Check significance of difference between cohorts 

## dbp  ##
describe(dbp)
shapiro.test(dbp) 
ggplot(Data_Summary, aes(x = dbp))+
  geom_histogram(fill="lightpink", color="black", binwidth =5)

describe(agewell$dbp)

describe(scdwell$dbp)

wilcox.test(agewell$dbp, scdwell$dbp) # Check significance of difference between cohorts 

## anx  ##
describe(anx)
shapiro.test(anx) 
ggplot(Data_Summary, aes(x = anx))+
  geom_histogram(fill="lightpink", color="black", binwidth =2)

describe(agewell$anx)

describe(scdwell$anx)

wilcox.test(agewell$anx, scdwell$anx) # Check significance of difference between cohorts 

## depresh  ##
describe(depresh)
shapiro.test(depresh) 
ggplot(Data_Summary, aes(x = depresh))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$depresh)

describe(scdwell$depresh)

wilcox.test(agewell$depresh, scdwell$depresh) # Check significance of difference between cohorts 

## sex ##
describe(sex)
describe(agewell$sex)
describe(scdwell$sex)
chisq.test(Data_Summary$sex, Data_Summary$study) # Check significance of difference between cohorts

## cds ##
describe(cds)
shapiro.test(cds) # positive skew
ggplot(Data_Summary, aes(x = cds))+
  geom_histogram(fill="lightpink", color="black", binwidth =5)

describe(agewell$cds)

describe(scdwell$cds)

wilcox.test(agewell$cds, scdwell$cds) # Check significance of difference between cohorts

## pacc ##
describe(pacc)
shapiro.test(pacc)
ggplot(Data_Summary, aes(x = pacc))+
  geom_histogram(fill="lightpink", color="black", binwidth =0.5)

describe(agewell$paccscdage)

describe(scdwell$paccscdage)

wilcox.test(agewell$pacc, scdwell$pacc) # Check significance of difference between cohorts

## cci ##
describe(cci)
shapiro.test(cci)
ggplot(Data_Summary, aes(x = cci))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$cci)

describe(scdwell$cci)

wilcox.test(agewell$cci, scdwell$cci) # Check significance of difference between cohorts

## frs ##
describe(frs)
shapiro.test(frs)
ggplot(Data_Summary, aes(x = frs))+
  geom_histogram(fill="lightpink", color="black", binwidth =1)

describe(agewell$frs)

describe(scdwell$frs)

wilcox.test(agewell$frs, scdwell$frs) # Check significance of difference between cohorts

# LINEAR REGS WHOQOL ----

# SCD WORRY WHOQOL

scdwell_W1 <- lm(scale(whoqol) ~ scale(psw), data = scdwell)
summary(scdwell_W1)
confint(scdwell_W1)

scdwell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_W2)
confint(scdwell_W2)

scdwell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_W3)
confint(scdwell_W3)

# AGEWELL WORRY WHOQOL
agewell_W1 <- lm(scale(whoqol) ~ scale(psw), data = agewell)
summary(agewell_W1)
confint(agewell_W1)

agewell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_W2)
confint(agewell_W2)

agewell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_W3)
confint(agewell_W3)

# SCD BROODING WHOQOL

scdwell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = scdwell)
summary(scdwell_B1)
confint(scdwell_B1)

scdwell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_B2)
confint(scdwell_B2)

scdwell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_B3)
confint(scdwell_B3)

# AGEWELL BROODING WHOQOL

agewell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = agewell)
summary(agewell_B1)
confint(agewell_B1)

agewell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_B2)
confint(agewell_B2)

agewell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_B3)
confint(agewell_B3)


## AGEWELL AND SCDWELL TOGETHER WHOQOL

W1 <- lm(scale(whoqol) ~ scale(psw), data = Data_Summary)
summary(W1)
confint(W1)

W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(W2)
confint(W2)

W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(anx), data = Data_Summary)
summary(W3)
confint(W3)

B1 <- lm(scale(whoqol) ~ scale(rrsb), data = Data_Summary)
summary(B1)
confint(B1)

B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(B2)
confint(B2)

B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = Data_Summary)
summary(B3)
confint(B3)


# LIN REG BLOOD PRESSURE ----

# SCD WORRY SBP

scdwell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = scdwell)
summary(scdwell_sbp1)
confint(scdwell_sbp1)

scdwell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_sbp2)
confint(scdwell_sbp2)

scdwell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_sbp3)
confint(scdwell_sbp3)

# AGEWELL WORRY SBP

agewell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = agewell)
summary(agewell_sbp1)
confint(agewell_sbp1)

agewell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_sbp2)
confint(agewell_sbp2)

agewell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_sbp3)
confint(agewell_sbp3)

# SCD BROODING SBP

scdwell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = scdwell)
summary(scdwell_sbp4)
confint(scdwell_sbp4)

scdwell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_sbp5)
confint(scdwell_sbp5)

scdwell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_sbp6)
confint(scdwell_sbp6)

# AGEWELL BROODING SBP

agewell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = agewell)
summary(agewell_sbp4)
confint(agewell_sbp4)

agewell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_sbp5)
confint(agewell_sbp5)

agewell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_sbp6)
confint(agewell_sbp6)

# AGEWELL && SCDWELL WORRY SBP

agescd_sbp1 <- lm(scale(sbp) ~ scale(psw), data = Data_Summary)
summary(agescd_sbp1)
confint(agescd_sbp1)

agescd_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_sbp2)
confint(agescd_sbp2)

agescd_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary)
summary(agescd_sbp3)
confint(agescd_sbp3)

# AGEWELL && SCD BROODING SBP

agescd_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = Data_Summary)
summary(agescd_sbp4)
confint(agescd_sbp4)

agescd_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_sbp5)
confint(agescd_sbp5)

agescd_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary)
summary(agescd_sbp6)
confint(agescd_sbp6)

# SCD WORRY DBP

scdwell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = scdwell)
summary(scdwell_dbp1)
confint(scdwell_dbp1)

scdwell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_dbp2)
confint(scdwell_dbp2)

scdwell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_dbp3)
confint(scdwell_dbp3)

# AGEWELL WORRY DBP

agewell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = agewell)
summary(agewell_dbp1)
confint(agewell_dbp1)

agewell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_dbp2)
confint(agewell_dbp2)

agewell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_dbp3)
confint(agewell_dbp3)

# SCD BROODING DBP

scdwell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = scdwell)
summary(scdwell_dbp4)
confint(scdwell_dbp4)

scdwell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_dbp5)
confint(scdwell_dbp5)

scdwell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_dbp6)
confint(scdwell_dbp6)

# AGEWELL BROODING DBP

agewell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = agewell)
summary(agewell_dbp4)
confint(agewell_dbp4)

agewell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_dbp5)
confint(agewell_dbp5)

agewell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_dbp6)
confint(agewell_dbp6)

# AGEWELL && SCDWELL WORRY DBP

agescd_dbp1 <- lm(scale(dbp) ~ scale(psw), data = Data_Summary)
summary(agescd_dbp1)
confint(agescd_dbp1)

agescd_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_dbp2)
confint(agescd_dbp2)

agescd_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary)
summary(agescd_dbp3)
confint(agescd_dbp3)

# AGEWELL && SCD BROODING DBP
agescd_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = Data_Summary)
summary(agescd_dbp4)
confint(agescd_dbp4)

agescd_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_dbp5)
confint(agescd_dbp5)

agescd_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary)
summary(agescd_dbp6)
confint(agescd_dbp6)

# LIN REG CCI ----

# SCD WORRY CCI

scdwell_cci1 <- lm(scale(cci) ~ scale(psw), data = scdwell)
summary(scdwell_cci1)
confint(scdwell_cci1)

scdwell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cci2)
confint(scdwell_cci2)

scdwell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_cci3)
confint(scdwell_cci3)

# AGEWELL WORRY CCI

agewell_cci1 <- lm(scale(cci) ~ scale(psw), data = agewell)
summary(agewell_cci1)
confint(agewell_cci1)

agewell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cci2)
confint(agewell_cci2)

agewell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_cci3)
confint(agewell_cci3)

# SCD BROODING CCI

scdwell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = scdwell)
summary(scdwell_cci4)
confint(scdwell_cci4)

scdwell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cci5)
confint(scdwell_cci5)

scdwell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_cci6)
confint(scdwell_cci6)

# AGEWELL BROODING CCI

agewell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = agewell)
summary(agewell_cci4)
confint(agewell_cci4)

agewell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cci5)
confint(agewell_cci5)

agewell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_cci6)
confint(agewell_cci6)

# AGEWELL && SCDWELL WORRY CCI

agescd_cci1 <- lm(scale(cci) ~ scale(psw), data = Data_Summary)
summary(agescd_cci1)
confint(agescd_cci1)

agescd_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_cci2)
confint(agescd_cci2)

agescd_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary)
summary(agescd_cci3)
confint(agescd_cci3)

# AGEWELL && SCD BROODING CCI

agescd_cci4 <- lm(scale(cci) ~ scale(rrsb), data = Data_Summary)
summary(agescd_cci4)
confint(agescd_cci4)

agescd_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_cci5)
confint(agescd_cci5)

agescd_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary)
summary(agescd_cci6)
confint(agescd_cci6)

# LIN REG FRS ----

# SCD WORRY FRS

scdwell_frs1 <- lm(scale(frs) ~ scale(psw), data = scdwell)
summary(scdwell_frs1)
confint(scdwell_frs1)

scdwell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = scdwell)
summary(scdwell_frs2)
confint(scdwell_frs2)

scdwell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_frs3)
confint(scdwell_frs3)

# AGEWELL WORRY FRS

agewell_frs1 <- lm(scale(frs) ~ scale(psw), data = agewell)
summary(agewell_frs1)
confint(agewell_frs1)

agewell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = agewell)
summary(agewell_frs2)
confint(agewell_frs2)

agewell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = agewell)
summary(agewell_frs3)
confint(agewell_frs3)

# SCD BROODING FRS

scdwell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = scdwell)
summary(scdwell_frs4)
confint(scdwell_frs4)

scdwell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = scdwell)
summary(scdwell_frs5)
confint(scdwell_frs5)

scdwell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_frs6)
confint(scdwell_frs6)

# AGEWELL BROODING FRS

agewell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = agewell)
summary(agewell_frs4)
confint(agewell_frs4)

agewell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = agewell)
summary(agewell_frs5)
confint(agewell_frs5)

agewell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_frs6)
confint(agewell_frs6)

# AGEWELL && SCDWELL WORRY FRS

agescd_frs1 <- lm(scale(frs) ~ scale(psw), data = Data_Summary)
summary(agescd_frs1)
confint(agescd_frs1)

agescd_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_frs2)
confint(agescd_frs2)

agescd_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx) + scale(study), data = Data_Summary)
summary(agescd_frs3)
confint(agescd_frs3)

# AGEWELL && SCD BROODING FRS

agescd_frs4 <- lm(scale(frs) ~ scale(rrsb), data = Data_Summary)
summary(agescd_frs4)
confint(agescd_frs4)

agescd_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(study), data = Data_Summary)
summary(agescd_frs5)
confint(agescd_frs5)

agescd_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary)
summary(agescd_frs6)
confint(agescd_frs6)

# LIN REG CDS ----

# SCD WORRY CDS

scdwell_cds1 <- lm(scale(cds) ~ scale(psw), data = scdwell)
summary(scdwell_cds1)
confint(scdwell_cds1)

scdwell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cds2)
confint(scdwell_cds2)

scdwell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_cds3)
confint(scdwell_cds3)

# AGEWELL WORRY CDS

agewell_cds1 <- lm(scale(cds) ~ scale(psw), data = agewell)
summary(agewell_cds1)
confint(agewell_cds1)

agewell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cds2)
confint(agewell_cds2)

agewell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_cds3)
confint(agewell_cds3)

# SCD BROODING CDS

scdwell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = scdwell)
summary(scdwell_cds4)
confint(scdwell_cds4)

scdwell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_cds5)
confint(scdwell_cds5)

scdwell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_cds6)
confint(scdwell_cds6)

# AGEWELL BROODING CDS

agewell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = agewell)
summary(agewell_cds4)
confint(agewell_cds4)

agewell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_cds5)
confint(agewell_cds5)

agewell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_cds6)
confint(agewell_cds6)

## AGEWELL AND SCDWELL TOGETHER

WC1 <- lm(scale(cds) ~ scale(psw), data = Data_Summary)
summary(WC1)
confint(WC1)

WC2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(WC2)
confint(WC2)

WC3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary)
summary(WC3)
confint(WC3)

# Brooding
BC1 <- lm(scale(cds) ~ scale(rrsb), data = Data_Summary)
summary(BC1)
confint(BC1)

BC2 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(BC2)
confint(BC2)

BC3 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary)
summary(BC3)
confint(BC3)


# LINEAR REGS PACC ----

# SCD WORRY PACC

scdwell_WPACC1 <- lm(scale(paccscdage) ~ scale(psw), data = scdwell)
summary(scdwell_WPACC1)
confint(scdwell_WPACC1)

scdwell_WPACC2 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_WPACC2)
confint(scdwell_WPACC2)

scdwell_WPACC3 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell)
summary(scdwell_WPACC3)
confint(scdwell_WPACC3)

# AGEWELL WORRY PACC

agewell_WPACC1 <- lm(scale(paccscdage) ~ scale(psw), data = agewell)
summary(agewell_WPACC1)
confint(agewell_WPACC1)

agewell_WPACC2 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_WPACC2)
confint(agewell_WPACC2)

agewell_WPACC3 <- lm(scale(paccscdage) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell)
summary(agewell_WPACC3)
confint(agewell_WPACC3)


# SCD BROODING WHOQOL

scdwell_BPACC1 <- lm(scale(paccscdage) ~ scale(rrsb), data = scdwell)
summary(scdwell_BPACC1)
confint(scdwell_BPACC1)

scdwell_BPACC2 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell)
summary(scdwell_BPACC2)
confint(scdwell_BPACC2)

scdwell_BPACC3 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell)
summary(scdwell_BPACC3)
confint(scdwell_BPACC3)

# AGEWELL BROODING WHOQOL

agewell_BPACC1 <- lm(scale(paccscdage) ~ scale(rrsb), data = agewell)
summary(agewell_BPACC1)
confint(agewell_BPACC1)

agewell_BPACC2 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell)
summary(agewell_BPACC2)
confint(agewell_BPACC2)

agewell_BPACC3 <- lm(scale(paccscdage) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell)
summary(agewell_BPACC3)
confint(agewell_BPACC3)


## AGEWELL AND SCDWELL TOGETHER WHOQOL

WPACC1 <- lm(scale(pacc) ~ scale(psw), data = Data_Summary)
summary(WPACC1)
confint(WPACC1)

WPACC2 <- lm(scale(pacc) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(WPACC2)
confint(WPACC2)

WPACC3 <- lm(scale(pacc) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(anx), data = Data_Summary)
summary(WPACC3)
confint(WPACC3)

BPACC1 <- lm(scale(pacc) ~ scale(rrsb), data = Data_Summary)
summary(BPACC1)
confint(BPACC1)

BPACC2 <- lm(scale(pacc) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(BPACC2)
confint(BPACC2)

BPACC3 <- lm(scale(pacc) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = Data_Summary)
summary(BPACC3)
confint(BPACC3)

# Additional analyses with worry and rumination in the same model ----

WRCDS1 <- lm(scale(cds) ~ scale(psw) + scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(WRCDS1)
confint(WRCDS1)

WRWHOQOL1 <- lm(scale(whoqol) ~ scale(psw) + scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary)
summary(WRWHOQOL1)
confint(WRWHOQOL1)

# Correlation analyses ----
# Calculate the Pearson correlation CCI
corr_cci <- cor.test(cci, cci_unadj, method = "pearson")

print(paste(corr_cci$conf.int[1], "-", corr_cci$conf.int[2]))
print(paste(corr_cci$p.value))

# Calculate the Pearson correlation worry & rumination
corr_pswrrsb <- cor.test(psw, rrsb, method = ("pearson"))

print(paste(corr_pswrrsb$conf.int[1], "-", corr_pswrrsb$conf.int[2]))
print(paste(corr_pswrrsb$p.value))
