# reading in data ----
getwd()  
setwd("Users/rachelmorse/Documents/2021:2022/Marchant Lab/Data")
library(readxl)
data <- MS_Data <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx", 
                              sheet = "Summary", col_types = c("text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric","numeric", "numeric", "numeric", 
                                                               "numeric", "text", "numeric", "text", 
                                                               "numeric", "numeric","numeric", "numeric", "numeric", "numeric","numeric"))
Data_Summary <- (`data`)
as.data.frame (Data_Summary)
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("hrbrthemes")) {install.packages("hrbrthemes"); require("hrbrthemes")}
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if (!require("QuantPsyc")) {install.packages("QuantPsyc"); require("QuantPsyc")}

attach(Data_Summary)
#rename vars----
psw <- (`Penn State Worry`)
rrsb <- (`Rumination Response Scale Brooding`)
whoqol <- (`Quality of Life measure`)
age <- (`Age at V1`)
sex <- (`gender`)
maq <- (`MAQ Physical activty past 12 months (H/W/Y)`)
alc <- (`Average number of glasses/week`)
medas <- (`MEDAS total`)
sbp <- (`SBP (mmHg)`)
dbp <- (`DBP (mmHg)`)
anx <- (`STAI-A score`)
depresh <- (`Geriatric Depression Scale - Global`)
edu <- (`Level of education`)
frs <- (`Framingham`)
cci <- (`Adjusted CCI`)
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

#REMAINING AGEWELL AND SCDWELL VARS
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
agewell$pacc <- (`PACC5 (for separate cohort analysis)`)
agewell$cds <- (`Cognitive Difficulties Scale (CDS)`)
agewell$pacc_z <- (`pacc_z`)
agewell$pacc5  <- (`PACC5 (STANDARDIZED with combined cohorts)`)

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
scdwell$pacc<- (`PACC5 (for separate cohort analysis)`)
scdwell$cds <- (`Cognitive Difficulties Scale (CDS)`)
scdwell$pacc5  <- (`PACC5 (STANDARDIZED with combined cohorts)`)

##### Descriptive stats of demographic variables #####----
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
ggdensity(rrs, fill = "lightpink")
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
hist(pacc)  # Create a histogram of the data
shapiro.test(pacc)  # Perform a Shapiro-Wilk test of normality

attach(agewell)
describe(agewell$pacc5)

attach(scdwell)
describe(scdwell$pacc5)

wilcox.test(agewell$pacc5,scdwell$pacc5)
cor.test(agewell$pacc,scdwell$pacc, method=("pearson"))

wilcox.test(agewell$paccscdage,scdwell$paccscdage)


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

#WHOQOL GRAPHS----
png('SCD_psw_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
SCD_psw_WHOQOL <- ggplot(scdwell, aes(x=psw, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
SCD_psw_WHOQOL <- SCD_psw_WHOQOL + labs(x = "Worry", y = "Subjective Physical Health (SCD-Well)")
SCD_psw_WHOQOL
dev.off()
#coefficients 
scdwell_whoqol7 <- lm(whoqol ~ psw, data = scdwell) 
summary(scdwell_whoqol7)

png('SCD_rrsb_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
SCD_rrsb_WHOQOL <- ggplot(scdwell, aes(x=rrsb, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
SCD_rrsb_WHOQOL <- SCD_rrsb_WHOQOL + labs(x = "Ruminative Brooding", y = "Subjective Physical Health (SCD-Well)")
SCD_rrsb_WHOQOL
dev.off()
#coefficients 
scdwell_whoqol8 <- lm(whoqol ~ rrsb, data = scdwell) 
summary(scdwell_whoqol8)

png('Age_psw_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
Age_psw_WHOQOL <- ggplot(agewell, aes(x=psw, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
Age_psw_WHOQOL <- Age_psw_WHOQOL + labs(x = "Worry", y = "Subjective Physical Health (Age-Well)")
Age_psw_WHOQOL
dev.off()
#coefficients 
agewell_whoqol7 <- lm(whoqol ~ psw, data = agewell) 
summary(agewell_whoqol7)

png('Age_rrsb_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
Age_rrsb_WHOQOL <- ggplot(agewell, aes(x=rrsb, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
Age_rrsb_WHOQOL <- Age_rrsb_WHOQOL + labs(x = "Ruminative Brooding", y = "Subjective Physical Health (Age-Well)")
Age_rrsb_WHOQOL
dev.off()
agewell_whoqol8 <- lm(whoqol ~ rrsb, data = agewell) 
summary(agewell_whoqol8)

png('rrsb_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
rrsb_WHOQOL <- ggplot(Data_Summary, aes(x=rrsb, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
rrsb_WHOQOL <- rrsb_WHOQOL + labs(x = "Ruminative Brooding", y = "Subjective Physical Health")
rrsb_WHOQOL
dev.off()
#coefficients 
SCDAge_whoqol <- lm(whoqol ~ rrsb, data = Data_Summary) 
summary(SCDAge_whoqol)

png('psw_WHOQOL.png', pointsize=10, width=1788, height=1320, res=300)
psw_WHOQOL <- ggplot(Data_Summary, aes(x=psw, y=whoqol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
psw_WHOQOL <- psw_WHOQOL + labs(x = "Worry", y = "Subjective Physical Health")
psw_WHOQOL
dev.off()
#coefficients 
SCDAge_whoqol2 <- lm(whoqol ~ psw, data = Data_Summary) 
summary(SCDAge_whoqol2)


# LINEAR REGS WHOQOL ----

# SCD WORRY WHOQOL 
attach(scdwell)

scdwell_W1 <- lm(scale(whoqol) ~ scale(psw), data = scdwell) 
summary(scdwell_W1) 
-0.202922 - (1.96*0.083411) 
-0.202922 + (1.96*0.083411) 

scdwell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_W2) 
-0.237773 - (1.96*0.083266) 
-0.237773 + (1.96*0.083266) 

scdwell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_W3) 
-0.166980 - (1.96*0.086065) 
-0.166980 + (1.96*0.086065) 

# AGEWELL WORRY WHOQOL
attach(agewell)

agewell_W1 <- lm(scale(whoqol) ~ scale(psw), data = agewell) 
summary(agewell_W1) 
-2.731e-01 - (1.96*8.341e-02) 
-2.731e-01 + (1.96*8.341e-02) 

agewell_W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_W2) 
-0.277981 - (1.96*0.082822) 
-0.277981 + (1.96*0.082822) 

agewell_W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_W3) 
-0.249999  - (1.96* 0.085314) 
-0.249999  + (1.96* 0.085314) 

# SCD BROODING WHOQOL 
attach(scdwell)

scdwell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = scdwell) 
summary(scdwell_B1) 
-0.240397 - (1.96*0.084997) 
-0.240397 + (1.96*0.084997) 

scdwell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_B2) 
-0.258453 - (1.96*0.082877) 
-0.258453 + (1.96*0.082877) 

scdwell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_B3) 
-0.130767 - (1.96*0.089454) 
-0.130767 + (1.96*0.089454) 

# AGEWELL BROODING WHOQOL
attach(agewell)

agewell_B1 <- lm(scale(whoqol) ~ scale(rrsb), data = agewell) 
summary(agewell_B1) 
-0.182158 - (1.96*0.085883) 
-0.182158 + (1.96*0.085883) 

agewell_B2 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_B2) 
-0.190267 - (1.96*0.085080) 
-0.190267 + (1.96*0.085080) 

agewell_B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_B3) 
-0.1253096  - (1.96* 0.0822476) 
-0.1253096  + (1.96* 0.0822476) 

## AGEWELL AND SCDWELL TOGETHER
attach(Data_Summary)


W1 <- lm(scale(whoqol) ~ scale(psw), data = Data_Summary) 
summary(W1) 
-0.268380  - (1.96* 0.058307) 
-0.268380  + (1.96* 0.058307) 

W2 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(W2) 
-0.277035  - (1.96* 0.056258) 
-0.277035  + (1.96* 0.056258) 

W3 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(anx), data = Data_Summary) 
summary(W3) 
-0.194425  - (1.96* 0.058857) 
-0.194425  + (1.96* 0.058857) 


B1 <- lm(scale(whoqol) ~ scale(rrsb), data = Data_Summary) 
summary(B1) 
-0.24061  - (1.96* 0.05903) 
-0.24061  + (1.96* 0.05903) 

B2 <- lm(scale(whoqol)~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(B2) 
-0.24844  - (1.96* .05639) 
-0.24844  + (1.96* .05639) 

B3 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study) + scale(depresh), data = Data_Summary) 
summary(B3) 
-0.123248  - (1.96* 0.057233) 
-0.123248  + (1.96* 0.057233) 

###SCD AND AGE WELL WITH STUDY AS COVARIATE
attach(Data_Summary)


W5 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(W5) 
-0.245332  - (1.96* 0.057157) 
-0.245332  + (1.96* 0.057157) 

W6 <- lm(scale(whoqol) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(W6) 
-0.1375060  - (1.96* 0.0570893) 
-0.1375060  + (1.96* 0.0570893) 


B5 <- lm(scale(whoqol)~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(B5) 
-0.223721  - (1.96* 0.056374) 
-0.223721  + (1.96* 0.056374) 

B6 <- lm(scale(whoqol) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(site), data = Data_Summary) 
summary(B6) 
-0.123248  - (1.96* 0.057233) 
-0.123248  + (1.96* 0.057233) 


###SCD AND AGE WELL WITH STUDY AS COVARIATE and PSW and RRSB in 1 model 
BW1 <- lm(scale(whoqol) ~ scale(rrsb) + scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(BW1) 
-0.153103  - (1.96* 0.064898) 
-0.153103  + (1.96* 0.064898) 

-0.144059  - (1.96* 0.067004) 
-0.144059  + (1.96* 0.067004) 
# LIN REG BLOOD PRESSURE ----

# SCD WORRY SBP
attach(scdwell)

scdwell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = scdwell) 
summary(scdwell_sbp1) 
0.023582  - (1.96* 0.085409) 
0.023582  + (1.96* 0.085409) 

scdwell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_sbp2) 
0.089517  - (1.96* 0.084359) 
0.089517  + (1.96* 0.084359) 


scdwell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_sbp3) 
0.005502  - (1.96* 0.086797) 
0.005502  + (1.96* 0.086797) 


# AGEWELL WORRY SBP
attach(agewell)

agewell_sbp1 <- lm(scale(sbp) ~ scale(psw), data = agewell) 
summary(agewell_sbp1) 
-0.019031  - (1.96* 0.087172) 
-0.019031  + (1.96* 0.087172) 

agewell_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_sbp2) 
0.019081  - (1.96* 0.084080) 
0.019081  + (1.96* 0.084080) 

agewell_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_sbp3)
0.001549  - (1.96* 0.086829) 
0.001549  + (1.96* 0.086829) 

# SCD BROODING SBP
attach(scdwell)

scdwell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = scdwell) 
summary(scdwell_sbp4) 
0.04267  - (1.96* 0.09250) 
0.04267  + (1.96* 0.09250) 

scdwell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_sbp5) 
0.06978  - (1.96* 0.08922) 
0.06978  + (1.96* 0.08922) 

scdwell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_sbp6) 
0.05706  - (1.96* 0.09938) 
0.05706  + (1.96* 0.09938) 

# AGEWELL BROODING SBP
attach(agewell)

agewell_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = agewell) 
summary(agewell_sbp4)
-0.02358  - (1.96* 0.08704) 
-0.02358  + (1.96* 0.08704) 

agewell_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_sbp5) 
0.0002852  - (1.96* 0.0833305) 
0.0002852  + (1.96* 0.0833305) 

agewell_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_sbp6)
-0.020908  - (1.96* 0.084803) 
-0.020908  + (1.96* 0.084803) 

# AGEWELL && SCDWELL WORRY SBP
attach(Data_Summary)

agescd_sbp1 <- lm(scale(sbp) ~ scale(psw), data = Data_Summary) 
summary(agescd_sbp1) 
0.03773  - (1.96* 0.06071) 
0.03773  + (1.96* 0.06071) 

agescd_sbp2 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(agescd_sbp2) 
0.082729  - (1.96* 0.084080) 
0.082729  + (1.96* 0.084080) 

agescd_sbp3 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = Data_Summary) 
summary(agescd_sbp3)
0.013887  - (1.96* 0.060574) 
0.013887  + (1.96* 0.060574) 

# AGEWELL && SCD BROODING SBP
attach(Data_Summary)

agescd_sbp4 <- lm(scale(sbp) ~ scale(rrsb), data = Data_Summary) 
summary(agescd_sbp4) 
0.03306  - (1.96* 0.06285) 
0.03306  + (1.96* 0.06285) 

agescd_sbp5 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(agescd_sbp5) 
0.05580  - (1.96* 0.05977) 
0.05580  + (1.96* 0.05977) 

agescd_sbp6 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(agescd_sbp6) 
0.02783  - (1.96* 0.06404) 
0.02783  + (1.96* 0.06404) 

####SCD AND AGE SBP WITH STUDY INCLUDED

agescd_sbp7 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_sbp7) 
0.060665 - (1.96* 0.059336)
0.060665 + (1.96* 0.059336) 

agescd_sbp8 <- lm(scale(sbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(agescd_sbp8)

agescd_sbp9 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_sbp9) 
0.023270 - (1.96* 0.064016)
0.023270 + (1.96* 0.064016) 

agescd_sbp10 <- lm(scale(sbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(agescd_sbp10) 


# SCD WORRY DBP
attach(scdwell)

scdwell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = scdwell) 
summary(scdwell_dbp1) 
0.0947695  - (1.96* 0.0839415) 
0.0947695  + (1.96* 0.0839415) 

scdwell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_dbp2) 
0.083724  - (1.96* 0.085518) 
0.083724  + (1.96* 0.085518) 

scdwell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_dbp3) 
-0.0011924  - (1.96* 0.0880054) 
-0.0011924  + (1.96* 0.0880054) 

# AGEWELL WORRY DBP
attach(agewell)

agewell_dbp1 <- lm(scale(dbp) ~ scale(psw), data = agewell) 
summary(agewell_dbp1) 
-0.0150557  - (1.96* 0.0876746) 
-0.0150557  + (1.96* 0.0876746) 

agewell_dbp2 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_dbp2) 
-0.016582  - (1.96* 0.089536) 
-0.016582  + (1.96* 0.089536) 

agewell_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_dbp3)
-0.012471  - (1.96* .092614) 
-0.012471  + (1.96* .092614) 

# SCD BROODING DBP
attach(scdwell)

scdwell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = scdwell) 
summary(scdwell_dbp4) 
0.14557  - (1.96* 0.08805) 
0.14557  + (1.96* 0.08805) 

scdwell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_dbp5) 
0.13444  - (1.96* 0.08870) 
0.13444  + (1.96* 0.08870) 

scdwell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_dbp6) 
0.08866  - (1.96* 0.09837) 
0.08866  + (1.96* 0.09837) 

# AGEWELL BROODING DBP
attach(agewell)

agewell_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = agewell) 
summary(agewell_dbp4)
-0.04846  - (1.96* 0.08637) 
-0.04846  + (1.96* 0.08637) 

agewell_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_dbp5) 
-0.05239  - (1.96* 0.08765) 
-0.05239  + (1.96* 0.08765) 

agewell_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_dbp6)
-0.04677  - (1.96* 0.08966) 
-0.04677  + (1.96* 0.08966) 

# AGEWELL && SCDWELL WORRY DBP

agescd_dbp2 <- lm(scale(dbp) ~ scale(psw), data = Data_Summary) 
summary(agescd_dbp2) 
3.895e-02  - (1.96* 6.036e-02) 
3.895e-02  + (1.96* 6.036e-02) 

agescd_dbp3 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = Data_Summary) 
summary(agescd_dbp3)
-0.0149554  - (1.96* 0.0637623) 
-0.0149554  + (1.96* 0.0637623) 

# AGEWELL && SCD BROODING DBP
attach(Data_Summary)

agescd_dbp4 <- lm(scale(dbp) ~ scale(rrsb), data = Data_Summary) 
summary(agescd_dbp4) 
0.05002  - (1.96* 0.06137) 
0.05002  + (1.96* 0.06137) 

agescd_dbp5 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(agescd_dbp5) 
0.047808  - (1.96* 0.061455) 
0.047808  + (1.96* 0.061455) 

agescd_dbp6 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(agescd_dbp6) 
0.033511  - (1.96* 0.065953) 
0.033511  + (1.96* 0.065953) 

####SCD AND AGE DBP WITH STUDY INCLUDED
attach(Data_Summary)

agescd_dbp7 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_dbp7) 
0.036178  - (1.96* 0.066025) 
0.036178  + (1.96* 0.066025)

agescd_dbp8 <- lm(scale(dbp) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(agescd_dbp8)

agescd_dbp9 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_dbp9) 
-0.0095035  - (1.96* 0.0643147) 
-0.0095035  + (1.96* 0.0643147)

agescd_dbp10 <- lm(scale(dbp) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(agescd_dbp10) 


# LIN REG CCI ----

# SCD WORRY CCI
attach(scdwell)

scdwell_cci1 <- lm(scale(cci) ~ scale(psw), data = scdwell) 
summary(scdwell_cci1) 
0.103761  - (1.96* 0.077992) 
0.103761  + (1.96* 0.077992) 

scdwell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_cci2) 
0.04402  - (1.96* 0.08638) 
0.04402  + (1.96* 0.08638) 

scdwell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_cci3) 
0.08999  - (1.96* 0.09109) 
0.08999  + (1.96* 0.09109) 

# AGEWELL WORRY CCI
attach(agewell)

agewell_cci1 <- lm(scale(cci) ~ scale(psw), data = agewell) 
summary(agewell_cci1) 
-0.001774  - (1.96* 0.094705) 
-0.001774  + (1.96* 0.094705) 

agewell_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_cci2) 
-0.005394  - (1.96* 0.095267) 
-0.005394  + (1.96* 0.095267) 

agewell_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_cci3)
0.01042  - (1.96* 0.09848) 
0.01042  + (1.96* 0.09848) 

# SCD BROODING CCI
attach(scdwell)

scdwell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = scdwell) 
summary(scdwell_cci4) 
0.03311  - (1.96* 0.09347) 
0.03311  + (1.96* 0.09347) 

scdwell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_cci5) 
0.02576  - (1.96* 0.09438) 
0.02576  + (1.96* 0.09438) 

scdwell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_cci6) 
-0.01019  - (1.96* 0.10373) 
-0.01019  + (1.96* 0.10373) 

# AGEWELL BROODING CCI
attach(agewell)

agewell_cci4 <- lm(scale(cci) ~ scale(rrsb), data = agewell) 
summary(agewell_cci4)
-0.01371  - (1.96* 0.09361) 
-0.01371  + (1.96* 0.09361) 

agewell_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_cci5) 
-0.01606  - (1.96* 0.09414) 
-0.01606  + (1.96* 0.09414) 

agewell_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_cci6)
0.009657  - (1.96* 0.095705) 
0.009657  + (1.96* 0.095705) 

# AGEWELL && SCDWELL WORRY CCI
attach(Data_Summary)

agescd_cci1 <- lm(scale(cci) ~ scale(psw), data = Data_Summary) 
summary(agescd_cci1) 
0.06821  - (1.96* 0.06235) 
0.06821  + (1.96* 0.06235) 

agescd_cci2 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu), data = Data_Summary) 
summary(agescd_cci2) 
0.059787  - (1.96* 0.063126) 
0.059787  + (1.96* 0.063126) 

agescd_cci3 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx), data = Data_Summary) 
summary(agescd_cci3)
0.075724  - (1.96* 0.066792) 
0.075724  + (1.96* 0.066792) 

agescd_cci37 <- lm((cci) ~ (psw) + (age) + (sex) + (edu) + (anx), data = Data_Summary) 
summary(agescd_cci37)

# AGEWELL && SCD BROODING CCI
attach(Data_Summary)

agescd_cci4 <- lm(scale(cci) ~ scale(rrsb), data = Data_Summary) 
summary(agescd_cci4) 
0.044107  - (1.96* 0.065617) 
0.044107  + (1.96* 0.065617) 

agescd_cci5 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu), data = Data_Summary) 
summary(agescd_cci5) 
0.03925  - (1.96* 0.06597) 
0.03925  + (1.96* 0.06597) 

agescd_cci6 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(agescd_cci6) 
0.014293  - (1.96* 0.070322) 
0.014293  + (1.96* 0.070322) 

####SCD AND AGE CCI WITH STUDY INCLUDED

agescd_cci7 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_cci7) 
0.02441  - (1.96* 0.06248) 
0.02441  + (1.96* 0.06248) 

agescd_cci8 <- lm(scale(cci) ~ scale(psw) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(agescd_cci8)

agescd_cci9 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_cci9) 
0.05493  - (1.96* 0.06523) 
0.05493  + (1.96* 0.06523) 

agescd_cci10 <- lm(scale(cci) ~ scale(rrsb) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(agescd_cci10) 
0.009412  - (1.96* 0.068525) 
0.009412  + (1.96* 0.068525) 

# LIN REG FRS ----

# SCD WORRY FRS
attach(scdwell)

scdwell_frs1 <- lm(scale(frs) ~ scale(psw), data = scdwell) 
summary(scdwell_frs1) 
0.10603  - (1.96* 0.08336) 
0.10603  + (1.96* 0.08336) 

scdwell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = scdwell) 
summary(scdwell_frs2) 
0.10608  - (1.96* 0.08366) 
0.10608  + (1.96* 0.08366) 

scdwell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_frs3) 
0.04433  - (1.96* 0.08821) 
0.04433  + (1.96* 0.08821) 

# AGEWELL WORRY FRS
attach(agewell)

agewell_frs1 <- lm(scale(frs) ~ scale(psw), data = agewell) 
summary(agewell_frs1) 
-0.014774  - (1.96* 0.087047) 
-0.014774  + (1.96* 0.087047) 

agewell_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = agewell) 
summary(agewell_frs2) 
-0.030519  - (1.96* 0.084170) 
-0.030519  + (1.96* 0.084170) 

agewell_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_frs3)
-0.068104  - (1.96* 0.085953) 
-0.068104  + (1.96* 0.085953) 

# SCD BROODING FRS
attach(scdwell)

scdwell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = scdwell) 
summary(scdwell_frs4) 
0.087872  - (1.96* 0.090545) 
0.087872  + (1.96* 0.090545) 

scdwell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = scdwell) 
summary(scdwell_frs5) 
0.088123  - (1.96* 0.091019) 
0.088123  + (1.96* 0.091019) 

scdwell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_frs6) 
0.034183  - (1.96* 0.099991) 
0.034183  + (1.96* 0.099991) 

# AGEWELL BROODING FRS
attach(agewell)

agewell_frs4 <- lm(scale(frs) ~ scale(rrsb), data = agewell) 
summary(agewell_frs4)
-0.03454  - (1.96* 0.08709) 
-0.03454  + (1.96* 0.08709) 

agewell_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = agewell) 
summary(agewell_frs5) 
-0.04819  - (1.96* 0.08436) 
-0.04819  + (1.96* 0.08436) 

agewell_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_frs6)
-0.08561  - (1.96* 0.08498) 
-0.08561  + (1.96* 0.08498) 

# AGEWELL && SCDWELL WORRY FRS
attach(Data_Summary)

agescd_frs1 <- lm(scale(frs) ~ scale(psw), data = Data_Summary) 
summary(agescd_frs1) 
0.043032  - (1.96* 0.060076) 
0.043032  + (1.96* 0.060076) 

agescd_frs2 <- lm(scale(frs) ~ scale(psw) + scale(edu), data = Data_Summary) 
summary(agescd_frs2) 
0.040526  - (1.96* 0.059820) 
0.040526  + (1.96* 0.059820) 

agescd_frs3 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx), data = Data_Summary) 
summary(agescd_frs3)
-0.018326  - (1.96* 0.062805) 
-0.018326  + (1.96* 0.062805) 

# AGEWELL && SCD BROODING FRS
attach(Data_Summary)

agescd_frs4 <- lm(scale(frs) ~ scale(rrsb), data = Data_Summary) 
summary(agescd_frs4) 
0.025586  - (1.96* 0.062542) 
0.025586  + (1.96* 0.062542) 

agescd_frs5 <- lm(scale(frs) ~ scale(rrsb) + scale(edu), data = Data_Summary) 
summary(agescd_frs5) 
0.026393  - (1.96* 0.062211) 
0.026393  + (1.96* 0.062211) 

agescd_frs6 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(agescd_frs6) 
-0.024144  - (1.96* 0.066021) 
-0.024144  + (1.96* 0.066021) 


####SCD AND AGE FRS WITH STUDY INCLUDED

agescd_frs7 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_frs7) 
0.048370  - (1.96* 0.060954) 
0.048370  + (1.96* 0.060954) 

agescd_frs8 <- lm(scale(frs) ~ scale(psw) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(agescd_frs8)
-0.009261  - (1.96* 0.063041) 
-0.009261  + (1.96* 0.063041) 

agescd_frs9 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_frs9) 
0.03147  - (1.96* 0.06287) 
0.03147  + (1.96* 0.06287) 

agescd_frs10 <- lm(scale(frs) ~ scale(rrsb) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(agescd_frs10) 
-0.021511  - (1.96* 0.065963) 
-0.021511  + (1.96* 0.065963) 

#CDS GRAPHS----
png('SCD_psw_cds.png', pointsize=10, width=1788, height=1320, res=300)
SCD_psw_cds <- ggplot(scdwell, aes(x=psw, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
SCD_psw_cds <- SCD_psw_cds + labs(x = "Worry", y = "Subjective Cognitive Health (SCD-Well)")
SCD_psw_cds
dev.off()
#coefficients 
scdwell_cds7 <- lm(cds ~ psw, data = scdwell) 
summary(scdwell_cds7)

png('SCD_rrsb_cds.png', pointsize=10, width=1788, height=1320, res=300)
SCD_rrsb_cds <- ggplot(scdwell, aes(x=rrsb, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
SCD_rrsb_cds <- SCD_rrsb_cds + labs(x = "Ruminative Brooding", y = "Subjective Cognitive Health (SCD-Well)")
SCD_rrsb_cds
dev.off()
#coefficients 
scdwell_cds8 <- lm(cds ~ rrsb, data = scdwell) 
summary(scdwell_cds8)

png('Age_psw_cds.png', pointsize=10, width=1788, height=1320, res=300)
Age_psw_cds <- ggplot(agewell, aes(x=psw, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
Age_psw_cds <- Age_psw_cds + labs(x = "Worry", y = "Subjective Cognitive Health (Age-Well)")
Age_psw_cds
dev.off()
#coefficients 
agewell_cds7 <- lm(cds ~ psw, data = agewell) 
summary(agewell_cds7)

png('Age_rrsb_cds.png', pointsize=10, width=1788, height=1320, res=300)
Age_rrsb_cds <- ggplot(agewell, aes(x=rrsb, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
Age_rrsb_cds <- Age_rrsb_cds + labs(x = "Ruminative Brooding", y = "Subjective Cognitive Health (Age-Well)")
Age_rrsb_cds
dev.off()
#coefficients 
agewell_cds8 <- lm(cds ~ rrsb, data = agewell) 
summary(agewell_cds8)

png('rrsb_cds.png', pointsize=10, width=1788, height=1320, res=300)
rrsb_cds <- ggplot(Data_Summary, aes(x=rrsb, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
rrsb_cds <- rrsb_cds + labs(x = "Ruminative Brooding", y = "Subjective Cognitive Difficulties")
rrsb_cds
dev.off()
#coefficients 
SCDAge_cds <- lm(cds ~ rrsb, data = Data_Summary) 
summary(SCDAge_cds)

png('psw_cds.png', pointsize=10, width=1788, height=1320, res=300)
psw_cds <- ggplot(Data_Summary, aes(x=psw, y=cds)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() 
psw_cds <- psw_cds + labs(x = "Worry", y = "Subjective Cognitive Difficulties")
psw_cds
dev.off()
#coefficients 
SCDAge_cds2 <- lm(cds ~ psw, data = Data_Summary) 
summary(SCDAge_cds2)


# LIN REG CDS ----

# SCD WORRY CDS
attach(scdwell)

scdwell_cds1 <- lm(scale(cds) ~ scale(psw), data = scdwell) 
summary(scdwell_cds1) 
0.155651  - (1.96* 0.084170) 
0.155651  + (1.96* 0.084170) 

scdwell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_cds2) 
0.175413  - (1.96* 0.085117) 
0.175413  + (1.96* 0.085117) 

scdwell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = scdwell) 
summary(scdwell_cds3) 
0.06293  - (1.96* 0.08575) 
0.06293  + (1.96* 0.08575) 

# AGEWELL WORRY CDS
attach(agewell)

agewell_cds1 <- lm(scale(cds) ~ scale(psw), data = agewell) 
summary(agewell_cds1) 
2.625e-01  - (1.96* 8.367e-02) 
2.625e-01  + (1.96* 8.367e-02) 

agewell_cds2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_cds2) 
0.2596214  - (1.96* 0.0840571) 
0.2596214  + (1.96* 0.0840571) 

agewell_cds3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx), data = agewell) 
summary(agewell_cds3)
0.2239774 - (1.96* 0.0862546) 
0.2239774  + (1.96* 0.0862546) 

# SCD BROODING CDS
attach(scdwell)

scdwell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = scdwell) 
summary(scdwell_cds4) 
0.228314  - (1.96* 0.089385) 
0.228314  + (1.96* 0.089385) 

scdwell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_cds5) 
0.241169  - (1.96* 0.088886) 
0.241169  + (1.96* 0.088886) 

scdwell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = scdwell) 
summary(scdwell_cds6) 
0.113228  - (1.96* 0.095423) 
0.113228  + (1.96* 0.095423) 

# AGEWELL BROODING CDS
attach(agewell)

agewell_cds4 <- lm(scale(cds) ~ scale(rrsb), data = agewell) 
summary(agewell_cds4)
0.2984337  - (1.96* 0.0834138) 
0.2984337  + (1.96* 0.0834138) 

agewell_cds5 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_cds5) 
0.292681  - (1.96* 0.083576) 
0.292681  + (1.96* 0.083576) 

agewell_cds6 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = agewell) 
summary(agewell_cds6)
0.26882  - (1.96* 0.08496) 
0.26882  + (1.96* 0.08496) 

## AGEWELL AND SCDWELL TOGETHER
attach(Data_Summary)


WC1 <- lm(scale(cds) ~ scale(psw), data = Data_Summary) 
summary(WC1) 
0.257219  - (1.96* 0.058594) 
0.257219  + (1.96* 0.058594) 

WC2 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(WC2) 
0.274170  - (1.96* 0.056946) 
0.274170  + (1.96* 0.056946) 

WC3 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(WC3) 
0.163033  - (1.96* 0.058495) 
0.163033  + (1.96* 0.058495) 


BC1 <- lm(scale(cds) ~ scale(rrsb), data = Data_Summary) 
summary(BC1) 
0.28796  - (1.96* 0.05992) 
0.28796  + (1.96* 0.05992) 

BC2 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = Data_Summary) 
summary(BC2) 
0.29822  - (1.96* 0.05810) 
0.29822  + (1.96* 0.05810) 

BC3 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh), data = Data_Summary) 
summary(BC3) 
0.19545  - (1.96* 0.05992) 
0.19545  + (1.96* 0.05992) 

####SCD AND AGE CDS WITH STUDY INCLUDED

agescd_cds7 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_cds7) 
0.196497  - (1.96* 0.053986) 
0.196497  + (1.96* 0.053986) 

agescd_cds8 <- lm(scale(cds) ~ scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(anx) + scale(study), data = Data_Summary) 
summary(agescd_cds8)
0.12547  - (1.96* 0.05473) 
0.12547  + (1.96* 0.05473) 

agescd_cds9 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(agescd_cds9) 
0.23946  - (1.96* 0.05419) 
0.23946  + (1.96* 0.05419) 

agescd_cds10 <- lm(scale(cds) ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu) + scale(depresh) + scale(study), data = Data_Summary) 
summary(agescd_cds10) 
0.17355  - (1.96* 0.05635) 
0.17355  + (1.96* 0.05635) 

###SCD AND AGE WELL WITH STUDY AS COVARIATE and PSW and RRSB in 1 model 
BW2 <- lm(scale(cds) ~ scale(rrsb) + scale(psw) + scale(age) + scale(sex) + scale(edu) + scale(study), data = Data_Summary) 
summary(BW2) 
0.185113  - (1.96* 0.062702) 
0.185113  + (1.96* 0.062702) 

0.110854  - (1.96* 0.065037) 
0.110854  + (1.96* 0.065037) 

##NOTE- analysis of AgeWell and SCDWell PACC is on another script

# CORRELATION ANALYSES
if(!require(devtools)) install.packages("devtools")
cor.test(scdwell$cci,scdwell$whoqol, method=("pearson"))
cor.test(scdwell$frs,scdwell$whoqol, method=("pearson"))
cor.test(scdwell$cci,scdwell$whoqol, method=("pearson"))
cor.test(scdwell$dbp,scdwell$whoqol, method=("pearson"))

cor.test(agewell$cci,agewell$whoqol, method=("pearson"))
cor.test(agewell$frs,agewell$whoqol, method=("pearson"))
cor.test(agewell$sbp,agewell$whoqol, method=("pearson"))
cor.test(agewell$dbp,agewell$whoqol, method=("pearson"))

cor.test(cci,whoqol, method=("pearson"))
cor.test(frs,whoqol, method=("pearson"))
cor.test(sbp,whoqol, method=("pearson"))
cor.test(dbp,whoqol, method=("pearson"))

cor.test(scdwell$pacc,scdwell$cds, method=("pearson"))
cor.test(agewell$pacc,agewell$cds, method=("pearson"))
cor.test(pacc,cds, method=("pearson"))


## SENSITIVITY ANALYSES 
# CCI
attach(agewell)

agewell_unadjcci1 <- lm(scale(unadjcci) ~ scale(psw), data = agewell) 
summary(agewell_unadjcci1)
-1.342e-01  - (1.96* 8.593e-02) 
-1.342e-01  + (1.96* 8.593e-02) 

agewell_unadjcci2 <- lm(scale(unadjcci) ~ scale(psw) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_unadjcci2) 
-0.1333292  - (1.96* 0.0867049) 
-0.1333292  + (1.96* 0.0867049) 

agewell_unadjcci3 <- lm(scale(unadjcci) ~ scale(rrsb), data = agewell) 
summary(agewell_unadjcci3)
-0.114922  - (1.96* 0.086545) 
-0.114922  + (1.96* 0.086545) 

agewell_unadjcci4 <- lm(scale(unadjcci) ~ scale(rrsb)+ scale(sex) + scale(edu), data = agewell) 
summary(agewell_unadjcci4) 
-0.112519  - (1.96* 0.087207) 
-0.112519  + (1.96* 0.087207) 

# FRS 

agewell_unadjfrs1 <- lm(scale(unadjfrs) ~ scale(psw), data = agewell) 
summary(agewell_unadjfrs1)
-0.014774  - (1.96* 0.087047) 
-0.014774  + (1.96* 0.087047) 

agewell_unadjfrs2 <- lm(scale(unadjfrs) ~ scale(psw) + scale(edu), data = agewell) 
summary(agewell_unadjfrs2) 
-0.030519  - (1.96* 0.084170) 
-0.030519  + (1.96* 0.084170) 

agewell_unadjfrs3 <- lm(scale(unadjfrs) ~ scale(rrsb), data = agewell) 
summary(agewell_unadjfrs3)
-0.03454  - (1.96* 0.08709) 
-0.03454  + (1.96* 0.08709) 

agewell_unadjfrs4 <- lm(scale(unadjfrs) ~ scale(rrsb)+ scale(edu), data = agewell) 
summary(agewell_unadjfrs4) 
-0.04819  - (1.96* 0.08436) 
-0.04819  + (1.96* 0.08436) 


# PACC5 

agewell_unadjpacc1 <- lm(scale(unadjpacc) ~ scale(psw), data = agewell) 
summary(agewell_unadjpacc1)
-1.329e-01  - (1.96* 8.594e-02) 
-1.329e-01  + (1.96* 8.594e-02) 

agewell_unadjpacc2 <- lm(scale(unadjpacc) ~ scale(psw) + scale(edu) + scale(sex) + scale(age), data = agewell) 
summary(agewell_unadjpacc2) 
-0.169616  - (1.96* 0.075402) 
-0.169616  + (1.96* 0.075402) 

agewell_unadjpacc3 <- lm(scale(unadjpacc) ~ scale(rrsb), data = agewell) 
summary(agewell_unadjpacc3)
-0.033879  - (1.96* 0.087307) 
-0.033879  + (1.96* 0.087307) 

agewell_unadjpacc4 <- lm(scale(unadjpacc) ~ scale(rrsb)+ scale(edu) + scale(sex) + scale(age) + scale(study) + scale(depresh), data = agewell) 
summary(agewell_unadjpacc4) 
-0.052089  - (1.96* 0.077034) 
-0.052089  + (1.96* 0.077034) 

#ANALYSIS WITH AGEWELL AND SCD SEPARATE PACC ADAPTED 
scdwell_pacc1 <- lm(pacc ~ scale(psw), data = scdwell) 
summary(scdwell_pacc1) 
0.029102  - (1.96* 0.085371) 
0.029102  + (1.96* 0.085371) 

scdwell_pacc2 <- lm(pacc ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_pacc2) 
-0.0506233  - (1.96* 0.0802654) 
-0.0506233  + (1.96* 0.0802654) 

scdwell_pacc1 <- lm(pacc ~ scale(rrsb), data = scdwell) 
summary(scdwell_pacc1) 
-0.04692  - (1.96* 0.09499) 
-0.04692  + (1.96* 0.09499) 

scdwell_pacc2 <- lm(pacc ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = scdwell) 
summary(scdwell_pacc2) 
-0.082028  - (1.96* 0.087366) 
-0.082028  + (1.96* 0.087366)

#agewell
attach(agewell)
agewell_pacc1 <- lm(pacc ~ scale(psw), data = agewell) 
summary(agewell_pacc1) 
-1.243e-01 - (1.96* 8.604e-02) 
-1.243e-01  + (1.96* 8.604e-02) 

agewell_pacc2 <- lm(pacc ~ scale(psw) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_pacc2) 
-0.163544  - (1.96* 0.074546) 
-0.163544  + (1.96* 0.074546) 

agewell_pacc1 <- lm(pacc ~ scale(rrsb), data = agewell) 
summary(agewell_pacc1) 
-0.052082  - (1.96* 0.087204) 
-0.052082  + (1.96* 0.087204) 

agewell_pacc2 <- lm(pacc ~ scale(rrsb) + scale(age) + scale(sex) + scale(edu), data = agewell) 
summary(agewell_pacc2) 
-0.073497  - (1.96* 0.075883) 
-0.073497  + (1.96* 0.075883) 

#COMBINE COHORT PACC 
scdage_pacc <- lm(pacc ~ scale(psw)+ scale(edu) + scale(sex) + scale(age) + scale(study) + scale(anx), data = Data_Summary) 
summary(scdage_pacc) 
-0.030484  - (1.96* 0.054793) 
-0.030484  + (1.96* 0.054793) 


scdage_pacc1 <- lm(pacc ~ scale(rrsb)+ scale(edu) + scale(sex) + scale(age) + scale(study) + scale(depresh), data = Data_Summary) 
summary(scdage_pacc1) 
-0.081064  - (1.96* 0.059448) 
-0.081064  + (1.96* 0.059448) 




