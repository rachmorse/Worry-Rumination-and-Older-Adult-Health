
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

#### CREATING ADJUSTED FRS ####
library(readxl)
getwd()  
setwd("Documents/2020:2021/Dissertation/Data")
fr1 <- read_excel("Framingham Data.xlsx", 
                  sheet = "Data fr Adjusted", col_types = c("text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", "numeric", 
                                                            "numeric"))
attach(fr1)

fr1 <- data.frame(fr1)

fr1 <- fr1 %>% 
  #age points
  filter(!is.na(Age)) %>% 
  mutate(agefr = case_when((Age <= 64.99999 & Sex == 0) ~ 10,
                           (Age <= 64.99999 & Sex == 1) ~ 10,
                           (Age > 64.99999 & Age <= 69.99999 & Sex == 0) ~ 11,
                           (Age > 64.99999 & Age <= 69.99999 & Sex == 1) ~ 12,
                           (Age > 69.99999 & Age <= 74.99999 & Sex == 0) ~ 12,
                           (Age > 69.99999 & Age <= 74.99999 & Sex == 1) ~ 14,
                           (Age > 74.99999 & Sex == 0) ~ 13,
                            TRUE ~ 16)) %>% 

  #SBP points  
  filter(!is.na(SBP)) %>% 
  mutate(sbpfr = case_when(SBP <= 120 ~ 0,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 0 & Sex == 0) ~ 0,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 0 & Sex == 1) ~ 1,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 1 & Sex == 0) ~ 1,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 1 & Sex == 1) ~ 3,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 0 & Sex == 0) ~ 1,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 0 & Sex == 1) ~ 2,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 1 & Sex == 0) ~ 2,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 1 & Sex == 1) ~ 4,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 0 & Sex == 0) ~ 1,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 0 & Sex == 1) ~ 3,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 1 & Sex == 0) ~ 2,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 1 & Sex == 1) ~ 5,
                           (SBP >= 160 & BP.Medication == 0 & Sex == 0) ~ 2,
                           (SBP >= 160 & BP.Medication == 0 & Sex == 1) ~ 4,
                           (SBP >= 160 & BP.Medication == 1 & Sex == 0) ~ 3,
                           TRUE ~ 6)) %>% 
  
  #cholesterol points
  filter(!is.na(Cholesterol)) %>% 
  mutate(chfr = case_when((Cholesterol == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
                          (Cholesterol == 1 & Age <= 69.99999 & Sex == 1) ~ 1,
                          (Cholesterol == 1 & Age > 69.99999 & Sex == 0) ~ 0,
                          (Cholesterol == 1 & Age > 69.99999 & Sex == 1) ~ 0, 
                            TRUE ~ 0)) %>%
  #smoker points
  filter(!is.na(Smoker)) %>% 
  mutate(smokefr = case_when((Smoker == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
                             (Smoker == 1 & Age <= 69.99999 & Sex == 1) ~ 2,
                             (Smoker == 1 & Age > 69.99999 & Sex == 0) ~ 1,
                             (Smoker == 1 & Age > 69.99999 & Sex == 1) ~ 1,
                               TRUE ~ 0)) %>%
  

  mutate(frs1 = agefr + chfr + smokefr + sbpfr) 

#### CREATING UNADJUSTED FRS ####

fr2 <- read_excel("Framingham Data.xlsx", 
                 sheet = "Data for Complete", col_types = c("text", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric"))
attach(fr2)

fr2 <- data.frame(fr2)

fr2 <- fr2 %>% 
  #age points
  mutate(agefr2 = case_when((Age <= 64.99999 & Sex == 0) ~ 10,
                           (Age <= 64.99999 & Sex == 1) ~ 10,
                           (Age > 64.99999 & Age <= 69.99999 & Sex == 0) ~ 11,
                           (Age > 64.99999 & Age <= 69.99999 & Sex == 1) ~ 12,
                           (Age >  69.99999 & Age <= 74.99999 & Sex == 0) ~ 12,
                           (Age > 69.99999 & Age <= 74.99999 & Sex == 1) ~ 14,
                           (Age > 74.99999 & Sex == 0) ~ 13,
                           TRUE ~ 16)) %>% 
  #SBP points
  filter(!is.na(SBP)) %>% 
  mutate(sbpfr2 = case_when(SBP <= 120 ~ 0,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 0 & Sex == 0) ~ 0,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 0 & Sex == 1) ~ 1,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 1 & Sex == 0) ~ 1,
                           (SBP > 120 & SBP <= 129 & BP.Medication == 1 & Sex == 1) ~ 3,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 0 & Sex == 0) ~ 1,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 0 & Sex == 1) ~ 2,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 1 & Sex == 0) ~ 2,
                           (SBP > 129 & SBP <= 139 & BP.Medication == 1 & Sex == 1) ~ 4,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 0 & Sex == 0) ~ 1,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 0 & Sex == 1) ~ 3,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 1 & Sex == 0) ~ 2,
                           (SBP > 139 & SBP <= 159 & BP.Medication == 1 & Sex == 1) ~ 5,
                           (SBP >= 160 & BP.Medication == 0 & Sex == 0) ~ 2,
                           (SBP >= 160 & BP.Medication == 0 & Sex == 1) ~ 4,
                           (SBP >= 160 & BP.Medication == 1 & Sex == 0) ~ 3,
                           TRUE ~ 6)) %>% 
  
  #total cholesterol points
  mutate(chmgdl = (Total.Cholesterol * 38.67)) %>% #convert from mmol/L to mg/dL
  
  mutate(chfr2 = case_when(chmgdl <= 160 ~ 0,
            (chmgdl > 160 & chmgdl <= 199 & Age <= 69.99999 & Sex == 0) ~ 1,
            (chmgdl > 160 & chmgdl <= 199 & Age <= 69.99999 & Sex == 1) ~ 1,
            (chmgdl > 160 & chmgdl <= 199 & Age > 69.99999 & Sex == 0) ~ 0,
            (chmgdl > 160 & chmgdl <= 199 & Age > 69.99999 & Sex == 1) ~ 1,
            (chmgdl > 199 & chmgdl <= 239 & Age <= 69.99999 & Sex == 0) ~ 1,
            (chmgdl > 199 & chmgdl <= 239 & Age <= 69.99999 & Sex == 1) ~ 2,
            (chmgdl > 199 & chmgdl <= 239 & Age > 69.99999 & Sex == 0) ~ 0,
            (chmgdl > 199 & chmgdl <= 239 & Age > 69.99999 & Sex == 1) ~ 1,
            (chmgdl > 239 & chmgdl <= 279 & Age <= 69.99999 & Sex == 0) ~ 2,
            (chmgdl > 239 & chmgdl <= 279 & Age <= 69.99999 & Sex == 1) ~ 3,
            (chmgdl > 239 & chmgdl <= 279 & Age > 69.99999 & Sex == 0) ~ 1,
            (chmgdl > 239 & chmgdl <= 279 & Age > 69.99999 & Sex == 1) ~ 2,
            (chmgdl > 279 & Age <= 69.99999 & Sex == 0) ~ 3,
            (chmgdl > 279 & Age <= 69.99999 & Sex == 1) ~ 4,
            (chmgdl > 279 & Age > 69.99999 & Sex == 0) ~ 1,
            TRUE ~ 2))%>% 
 
  #hdl cholesterol points
  mutate(hdlmgdl = (HDL.Cholesterol * 38.67)) %>% 
  
  filter(!is.na(hdlmgdl)) %>% 
  mutate(hdlfr2 = case_when(hdlmgdl >= 60 ~ -1,
                            (hdlmgdl < 60 & hdlmgdl >= 50) ~ 0,
                            (hdlmgdl < 50 & hdlmgdl >= 40) ~ 1,
                            TRUE ~ 2)) %>% 
  #smoker points
  filter(!is.na(Smoker)) %>% 
  mutate(smokefr2 = case_when((Smoker == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
                             (Smoker == 1 & Age <= 69.99999 & Sex == 1) ~ 2,
                             (Smoker == 1 & Age > 69.99999 & Sex == 0) ~ 1,
                             (Smoker == 1 & Age > 69.99999 & Sex == 1) ~ 1,
                             TRUE ~ 0)) %>%
  
  mutate(frs2 = agefr2 + chfr2 + hdlfr2 + smokefr2 + sbpfr2) 

write.csv(fr2,"~/Documents/2020:2021/Dissertation/Data/Framingham Data\\Unadjusted FRS Data.csv", row.names = FALSE)

#### DESCRIBING & COMPARING FRS ADJ & UNADJ ####

if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

## frs1 ##
frs1 <- (fr1$frs1)
describe(frs1)
shapiro.test(frs1) # positive skew
ggdensity(frs1, fill = "lightpink")

agewellfrs <- slice(Data_Summary1, 1:113)
attach(agewellfrs)
as.data.frame (agewellfrs)
describe(agewellfrs$frs1)

scdwellfrs <- slice(Data_Summary1, 114:260)
attach(scdwellfrs)
as.data.frame (scdwellfrs)
describe(scdwellfrs$frs1)

wilcox.test(scdwellfrs$frs1,agewellfrs$frs1)

## frs2 ##
frs2 <- (fr2$frs2)
describe(frs2)
shapiro.test(frs2) # positive skew
ggdensity(frs2, fill = "lightpink")

framingham <- merge(fr2,fr1,by="ID")
attach(framingham)

# adjusted vs unadjusted ---
plot(framingham$frs1,framingham$frs2) 
a <- lm(framingham$frs2 ~ framingham$frs1, data = Data_Summary) 
abline(a$coef[1], a$coef[2], col = "red", lwd = 3) 
summary(a) 

