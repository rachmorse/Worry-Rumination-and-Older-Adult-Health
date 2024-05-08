
# Load packages
if (!require("tidyverse")) {
  install.packages("tidyverse")
  require("tidyverse")
}

if (!require("psych")) {
  install.packages("psych")
  require("psych")
}

library(readxl)

# Read in the data 
getwd()
setwd("/Users/rachelmorse/Documents/2020:2021/Dissertation/Data")
fr1 <- read_excel("Framingham Data.xlsx",
  sheet = "Data fr Adjusted", col_types = c(
    "text",
    rep("numeric", 8)
  )
)

# This code is used to create the Framingham Risk Score Adjusted called "frs1" in the df "fr1"
fr1 <- fr1 %>%
  # age points
  filter(!is.na(Age)) %>%
  mutate(agefr = case_when(
    (Age <= 64.99999 & Sex == 0) ~ 10,
    (Age <= 64.99999 & Sex == 1) ~ 10,
    (Age > 64.99999 & Age <= 69.99999 & Sex == 0) ~ 11,
    (Age > 64.99999 & Age <= 69.99999 & Sex == 1) ~ 12,
    (Age > 69.99999 & Age <= 74.99999 & Sex == 0) ~ 12,
    (Age > 69.99999 & Age <= 74.99999 & Sex == 1) ~ 14,
    (Age > 74.99999 & Sex == 0) ~ 13,
    TRUE ~ 16
  )) %>%
  # SBP points
  filter(!is.na(SBP)) %>%
  mutate(sbpfr = case_when(
    SBP <= 120 ~ 0,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 0 & Sex == 0) ~ 0,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 0 & Sex == 1) ~ 1,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 1 & Sex == 0) ~ 1,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 1 & Sex == 1) ~ 3,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 0 & Sex == 0) ~ 1,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 0 & Sex == 1) ~ 2,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 1 & Sex == 0) ~ 2,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 1 & Sex == 1) ~ 4,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 0 & Sex == 0) ~ 1,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 0 & Sex == 1) ~ 3,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 1 & Sex == 0) ~ 2,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 1 & Sex == 1) ~ 5,
    (SBP >= 160 & "BP Medication" == 0 & Sex == 0) ~ 2,
    (SBP >= 160 & "BP Medication" == 0 & Sex == 1) ~ 4,
    (SBP >= 160 & "BP Medication" == 1 & Sex == 0) ~ 3,
    TRUE ~ 6
  )) %>%
  # cholesterol points
  filter(!is.na(Cholesterol)) %>%
  mutate(chfr = case_when(
    (Cholesterol == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
    (Cholesterol == 1 & Age <= 69.99999 & Sex == 1) ~ 1,
    (Cholesterol == 1 & Age > 69.99999 & Sex == 0) ~ 0,
    (Cholesterol == 1 & Age > 69.99999 & Sex == 1) ~ 0,
    TRUE ~ 0
  )) %>%
  # smoker points
  filter(!is.na(Smoker)) %>%
  mutate(smokefr = case_when(
    (Smoker == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
    (Smoker == 1 & Age <= 69.99999 & Sex == 1) ~ 2,
    (Smoker == 1 & Age > 69.99999 & Sex == 0) ~ 1,
    (Smoker == 1 & Age > 69.99999 & Sex == 1) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(frs1 = agefr + chfr + smokefr + sbpfr)

# This code is used to create the Framingham Risk Score Unadjusted called "frs2" in the df "fr2"
fr2 <- read_excel("Framingham Data.xlsx",
  sheet = "Data for Complete", col_types = c(
    "text", 
    rep("numeric", 9)
  )
)

fr2 <- fr2 %>% 
  rename(
    Age = "age",
    Sex = "sex",
    Smoker = "smoker",
    "BP Medication" = "bp treatment",
    SBP = "sbp",
    Total.Cholesterol = "total cholesterol",
    HDL.Cholesterol = "hdl cholesterol"
    )

fr2 <- fr2 %>%
  # age points
  mutate(agefr2 = case_when(
    (Age <= 64.99999 & Sex == 0) ~ 10,
    (Age <= 64.99999 & Sex == 1) ~ 10,
    (Age > 64.99999 & Age <= 69.99999 & Sex == 0) ~ 11,
    (Age > 64.99999 & Age <= 69.99999 & Sex == 1) ~ 12,
    (Age > 69.99999 & Age <= 74.99999 & Sex == 0) ~ 12,
    (Age > 69.99999 & Age <= 74.99999 & Sex == 1) ~ 14,
    (Age > 74.99999 & Sex == 0) ~ 13,
    TRUE ~ 16
  )) %>%
  # SBP points
  filter(!is.na(SBP)) %>%
  mutate(sbpfr2 = case_when(
    SBP <= 120 ~ 0,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 0 & Sex == 0) ~ 0,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 0 & Sex == 1) ~ 1,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 1 & Sex == 0) ~ 1,
    (SBP > 120 & SBP <= 129 & "BP Medication" == 1 & Sex == 1) ~ 3,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 0 & Sex == 0) ~ 1,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 0 & Sex == 1) ~ 2,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 1 & Sex == 0) ~ 2,
    (SBP > 129 & SBP <= 139 & "BP Medication" == 1 & Sex == 1) ~ 4,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 0 & Sex == 0) ~ 1,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 0 & Sex == 1) ~ 3,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 1 & Sex == 0) ~ 2,
    (SBP > 139 & SBP <= 159 & "BP Medication" == 1 & Sex == 1) ~ 5,
    (SBP >= 160 & "BP Medication" == 0 & Sex == 0) ~ 2,
    (SBP >= 160 & "BP Medication" == 0 & Sex == 1) ~ 4,
    (SBP >= 160 & "BP Medication" == 1 & Sex == 0) ~ 3,
    TRUE ~ 6
  )) %>%
  # total cholesterol points
  mutate(chmgdl = (Total.Cholesterol * 38.67)) %>% # convert from mmol/L to mg/dL

  mutate(chfr2 = case_when(
    chmgdl <= 160 ~ 0,
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
    TRUE ~ 2
  )) %>%
  # hdl cholesterol points
  mutate(hdlmgdl = (HDL.Cholesterol * 38.67)) %>%
  filter(!is.na(hdlmgdl)) %>%
  mutate(hdlfr2 = case_when(
    hdlmgdl >= 60 ~ -1,
    (hdlmgdl < 60 & hdlmgdl >= 50) ~ 0,
    (hdlmgdl < 50 & hdlmgdl >= 40) ~ 1,
    TRUE ~ 2
  )) %>%
  # smoker points
  filter(!is.na(Smoker)) %>%
  mutate(smokefr2 = case_when(
    (Smoker == 1 & Age <= 69.99999 & Sex == 0) ~ 1,
    (Smoker == 1 & Age <= 69.99999 & Sex == 1) ~ 2,
    (Smoker == 1 & Age > 69.99999 & Sex == 0) ~ 1,
    (Smoker == 1 & Age > 69.99999 & Sex == 1) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(frs2 = agefr2 + chfr2 + hdlfr2 + smokefr2 + sbpfr2)

## Descriptive stats for frs1 (the adjusted score) ##
describe(fr1$frs1)

## Descriptive stats for frs2 (the unadjusted score) ##
describe(fr2$frs2)

# Subset the dfs to only ID and FRS to merge
fr1 <- select(fr1, ID, frs1)
fr2 <- select(fr2, ID, frs2)

# Merge the dfs for a correlation analysis
framingham <- merge(fr1, fr2, by = "ID")

# Calculate the Pearson correlation
corr_frs <- cor.test(framingham$frs1, framingham$frs2, method = "pearson")
print(paste(corr_frs$conf.int[1], "-", corr_frs$conf.int[2]))
print(paste(corr_frs$p.value))
