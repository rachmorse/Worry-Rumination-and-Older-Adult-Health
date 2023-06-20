# reading in data ----
getwd()  
setwd("Users/rachelmorse/Documents/2021:2022/Marchant Lab/Data")
library(readxl)
Data_Summary <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "text", "numeric", 
                                    "text", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric"))
View(Data_Summary)
as.data.frame (Data_Summary)
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}

attach(Data_Summary)
#rename vars----
psw <- (`Penn State Worry`)
rrsb <- (`Rumination Response Scale Brooding`)
whoqol <- (`Quality of Life measure`)
age <- (`Age at V1`)
sex <- (`gender`)
sbp <- (`SBP (mmHg)`)
dbp <- (`DBP (mmHg)`)
anx <- (`STAI-A score`)
depresh <- (`Geriatric Depression Scale - Global`)
edu <- (`Level of education`)
frs <- (`Framingham`)
cci <- (`Adjusted CCI`)
cci_unadj <- (`Unadjusted CCI`)
pacc <- (`PACC5 (STANDARDIZED with combined cohorts)`)
paccscdage  <- (`PACC5 (for separate cohort analysis)`)
cds <- (`Cognitive Difficulties Scale (CDS)`)

### CREATING STUDY VARIABLE 
#NOTE 1 = SCD-Well and 2 = Age-Well
Data_Summary <- Data_Summary %>% 
  mutate(study = case_when(Country == "London, UK" ~ 1,
                          Country == "Cologne, Germany" ~ 1,
                          Country == "Barcelone, Spain" ~ 1,
                          Country == "Lyon, France" ~ 1,
                          TRUE ~ 2)) 
study <- (Data_Summary$study)
head(study)

#SLICING AGEWELL AND SCDWELL
agewell <- slice(Data_Summary, 148:282)

scdwell <- slice(Data_Summary, 1:147)

# REMAINING AGEWELL AND SCDWELL VARS ----
attach(agewell)
agewell$psw <- (`Penn State Worry`)
agewell$rrsb <- (`Rumination Response Scale Brooding`)
agewell$whoqol <- (`Quality of Life measure`)
agewell$age <- (`Age at V1`)
agewell$sex <- (`gender`)
agewell$maq <- (`MAQ Physical activty past 12 months (H/W/Y)`)
agewell$alc <- (`Average number of glasses/week`)
agewell$medas <- (`MEDAS total`)
agewell$sbp <- (`SBP (mmHg)`)
agewell$dbp <- (`DBP (mmHg)`)
agewell$anx <- (`STAI-A score`)
agewell$depresh <- (`Geriatric Depression Scale - Global`)
agewell$edu <- (`Level of education`)
agewell$frs <- (`Framingham`)
agewell$cci <- (`Adjusted CCI`)
agewell$unadjcci <- (`Unadjusted CCI`)
agewell$unadjfrs <- (`Unadjusted FRS`)
agewell$unadjpacc <- (`Unadjusted PACC5`)
agewell$paccscdage <- (`PACC5 (for separate cohort analysis)`)
agewell$cds <- (`Cognitive Difficulties Scale (CDS)`)
agewell$pacc  <- (`PACC5 (STANDARDIZED with combined cohorts)`)

attach(scdwell)
scdwell$psw <- (`Penn State Worry`)
scdwell$rrsb <- (`Rumination Response Scale Brooding`)
scdwell$whoqol <- (`Quality of Life measure`)
scdwell$age <- (`Age at V1`)
scdwell$sex <- (`gender`)
scdwell$maq <- (`MAQ Physical activty past 12 months (H/W/Y)`)
scdwell$alc <- (`Average number of glasses/week`)
scdwell$medas <- (`MEDAS total`)
scdwell$sbp <- (`SBP (mmHg)`)
scdwell$dbp <- (`DBP (mmHg)`)
scdwell$anx <- (`STAI-A score`)
scdwell$depresh <- (`Geriatric Depression Scale - Global`)
scdwell$edu <- (`Level of education`)
scdwell$frs <- (`Framingham`)
scdwell$cci <- (`Adjusted CCI`)
scdwell$paccscdage <- (`PACC5 (for separate cohort analysis)`)
scdwell$cds <- (`Cognitive Difficulties Scale (CDS)`)
scdwell$pacc  <- (`PACC5 (STANDARDIZED with combined cohorts)`)

# Descriptive stats of demographic variables #####----
## Library ##

## rrsb ##
describe(rrsb)
shapiro.test(rrsb) # positive skew
ggdensity(rrs, fill = "lightpink")

attach(agewell)
agewell$rrsb <- (`Rumination Response Scale Brooding`)
describe(agewell$rrsb)

attach(scdwell)
scdwell$rrsb <- (`Rumination Response Scale Brooding`)
describe(scdwell$rrsb)

wilcox.test(agewell$rrsb,scdwell$rrsb)

## psw ##
describe(psw)
shapiro.test(psw) # positive skew
ggdensity(psw, fill = "lightpink")

attach(agewell)
agewell$psw <- (`Penn State Worry`)
describe(agewell$psw)

attach(scdwell)
scdwell$psw <- (`Penn State Worry`)
describe(scdwell$psw)

wilcox.test(agewell$psw,scdwell$psw)

## whoqol ##
describe(whoqol)
shapiro.test(whoqol) # positive skew
ggdensity(whoqol, fill = "lightpink")

attach(agewell)
agewell$whoqol <- (`Quality of Life measure`)
describe(agewell$whoqol)

attach(scdwell)
scdwell$whoqol <- (`Quality of Life measure`)
describe(scdwell$whoqol)

wilcox.test(agewell$whoqol,scdwell$whoqol)

## age ##
describe(age)
shapiro.test(age) # positive skew
sd(age, na.rm = TRUE)

attach(agewell)
agewell$age <- (`Age at V1`)
describe(agewell$age)

attach(scdwell)
scdwell$age <- (`Age at V1`)
describe(scdwell$age)

wilcox.test(agewell$age,scdwell$age)

## education ##
describe(edu)
shapiro.test(edu) # positive skew
ggdensity(rrs, fill = "lightpink")

agewell$edu <- (`Level of education`)
describe(agewell$edu)

scdwell$edu <- (`Level of education`)
describe(scdwell$edu)

wilcox.test(agewell$edu,scdwell$edu)

## sbp  ##
describe(sbp)
shapiro.test(sbp) # positive skew
ggdensity(sbp, fill = "lightpink")

attach(agewell)
agewell$sbp <- (`SBP (mmHg)`)
describe(agewell$sbp)

attach(scdwell)
scdwell$sbp <- (`SBP (mmHg)`)
describe(scdwell$sbp)

wilcox.test(agewell$sbp,scdwell$sbp)

## dbp  ##
describe(dbp)
shapiro.test(dbp) # positive skew
ggdensity(dbp, fill = "lightpink")

attach(agewell)
agewell$dbp <- (`DBP (mmHg)`)
describe(agewell$dbp)

attach(scdwell)
scdwell$dbp <- (`DBP (mmHg)`)
describe(scdwell$dbp)

wilcox.test(agewell$dbp,scdwell$dbp)

## anx  ##
describe(anx)
shapiro.test(anx) # positive skew
ggdensity(anx, fill = "lightpink")

attach(agewell)
agewell$anx <- (`STAI-A score`)
describe(agewell$anx)

attach(scdwell)
scdwell$anx <- (`STAI-A score`)
describe(scdwell$anx)

wilcox.test(agewell$anx,scdwell$anx)

## depresh  ##
describe(depresh)
shapiro.test(depresh) # positive skew
ggdensity(depresh, fill = "lightpink")

attach(agewell)
agewell$depresh <- (`Geriatric Depression Scale - Global`)
describe(agewell$depresh)

attach(scdwell)
scdwell$depresh <- (`Geriatric Depression Scale - Global`)
describe(scdwell$depresh)

wilcox.test(agewell$depresh,scdwell$depresh)

install.packages("Hmisc")
library(Hmisc)

## sex ##
describe(sex)
describe(agewell$sex)
describe(scdwell$sex)
chisq.test(sex)

## manually filling in number of M and F for cohorts to get p-value
datatable <- matrix(c(83,95,52,52),nrow=2,ncol=2)
chisq.test(datatable,correct=FALSE)

## cds ##
describe(cds)
shapiro.test(cds) # positive skew
ggdensity(cds, fill = "lightpink")

attach(agewell)
agewell$cds <- (`Cognitive Difficulties Scale (CDS)`)
describe(agewell$cds)

attach(scdwell)
scdwell$cds <- (`Cognitive Difficulties Scale (CDS)`)
describe(scdwell$cds)

wilcox.test(agewell$cds,scdwell$cds)

## pacc ##
# descriptive stats for combined cohort in other script 
attach(Data_Summary)
describe(pacc)
hist(pacc)  
shapiro.test(pacc)  

attach(agewell)
describe(agewell$paccscdage)

attach(scdwell)
describe(scdwell$paccscdage)

wilcox.test(agewell$pacc,scdwell$pacc)

## cci ##
describe(cci)

attach(agewell)
describe(agewell$cci)

attach(scdwell)
describe(scdwell$cci)

wilcox.test(agewell$cci,scdwell$cci)

## frs ##
describe(frs)

attach(agewell)
describe(agewell$frs)

attach(scdwell)
describe(scdwell$frs)

wilcox.test(agewell$frs,scdwell$frs)


# LINEAR REGS WHOQOL ----

# SCD WORRY WHOQOL 
attach(scdwell)

# Fit the linear regression model
scdwell_W1 <- lm(scale(whoqol) ~ scale(psw), data = scdwell)

# Obtain the summary of the model
summary(scdwell_W1)

# Calculate the confidence intervals
confint(scdwell_W1)

scdwell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_W2) 
confint(scdwell_W2)

scdwell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_W3) 
confint(scdwell_W3)

# AGEWELL WORRY WHOQOL
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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

B2 <- lm(scale(whoqol)~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(B2) 
confint(B2)

B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = Data_Summary) 
summary(B3) 
confint(B3)


# LIN REG BLOOD PRESSURE ----

# SCD WORRY SBP
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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
attach(Data_Summary)

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
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

WC1 <- lm(scale(cds) ~ scale(psw), data = Data_Summary) 
summary(WC1) 
confint(WC1)

WC2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(WC2) 
confint(WC2)

WC3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(WC3) 
confint(WC3)

#Brooding
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
attach(scdwell)

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
attach(agewell)

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
attach(scdwell)

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
attach(agewell)

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
attach(Data_Summary)

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

BPACC2 <- lm(scale(pacc)~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
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
if(!require(devtools)) install.packages("devtools")

# correlation CCI
corr_cci <- cor.test(cci,cci_unadj, method=("pearson"))
conf_int_cci<- corr_cci$conf.int
p_cci <- corr_cci$p.value

cor.test(cci,cci_unadj, method=("pearson"))
print(paste(conf_int_cci[1], "-", conf_int_cci[2]))
print(paste(p_cci))  

#ncorrelation worry & rumination
corr_pswrrsb <- cor.test(psw,rrsb, method=("pearson"))
conf_int_psw_rrsb<- corr_pswrrsb$conf.int
p_psw_rrsb <- corr_pswrrsb$p.value

cor.test(psw,rrsb, method=("pearson"))
print(paste(conf_int_psw_rrsb[1], "-", conf_int_psw_rrsb[2]))
print(paste(p_psw_rrsb))  

  
  