###############################################
### Creating PACC-5 for Age-Well & SCD-Well ###
###############################################

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

# Read in data 
data <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx",
  col_types = c(
    "text", 
    rep("numeric", 19),
    "text", "numeric",
    rep("text", 2),
    rep("numeric", 10)
  )
)

# Divide the data by cohort
data <- data %>%
  mutate(study = ifelse(Country %in% c("London, UK", "Cologne, Germany", "Barcelone, Spain", "Lyon, France"), 1, 
                        ifelse(Country %in% c("France"), 2, NA)))

agewell_pacc <- data %>%
  filter(study == 2)

scdwell_pacc <- data %>%
  filter(study == 1)

################################################################
## Creating adjusted PACC scores for combined cohort analysis ##
################################################################

# Rename CVLT in Age-Well 
agewell_pacc <- agewell_pacc %>% 
  rename(cvlt = `ravlt OR cvlt (in Age-Well) this column can only be use for separate cohort analyses`)

# Transform CVLT in Age-Well to be comparable with RAVLT in SCD-Well and name it cvlt_ravlt
agewell_pacc <- agewell_pacc %>%
  filter(!is.na(cvlt)) %>%
  mutate(cvlt_ravlt = (cvlt / 16 * 15))

# Rename RAVLT in SCD-Well 
scdwell_pacc <- scdwell_pacc %>% 
  rename(ravlt = `ravlt OR cvlt (in Age-Well) this column can only be use for separate cohort analyses`)

# Then create a new variable cvlt_ravlt to be able to compare cohorts
scdwell_pacc$cvlt_ravlt <- scdwell_pacc$ravlt

# Subset the df to needed columns and merge the cohorts 
columns_for_analysis <- c("ID", "drs", "cvlt_ravlt", "coding", "fluency") # Set list of columns

merged_df <- full_join(scdwell_pacc, agewell_pacc, by = columns_for_analysis) # Merge

merged_df <- merged_df %>% 
  select(columns_for_analysis) # Subset df to list of columns

merged_df <- merged_df[complete.cases(merged_df[columns_for_analysis]), ] # Remove NAs

## Standardize components (create z-scores) ##
cols_to_scale <- c("zdrs" = "drs", "zcvlt_ravlt" = "cvlt_ravlt", "zfluency" = "fluency", "zcoding" = "coding") # New names are labeled with z

merged_df <- merged_df %>%
  mutate(across(cols_to_scale, scale, .names = "{.col}"))

## Create the PACC scores ##
merged_df <- merged_df %>%
  mutate(
    pacc5 = rowMeans(select(., zdrs, zcvlt_ravlt, zfluency, zcoding)), # Calculate the mean for each row
    zpacc5 = scale(pacc5) # Scale the PACC variable
  )  

################################################
## Creating adjusted PACC scores for SCD-Well ##
################################################

## Standardise components (create z-scores) ##
cols_to_scale_scd <- c("zdrs" = "drs", "zravlt" = "ravlt", "zfluency" = "fluency", "zcoding" = "coding") # New names are labeled with z

scdwell_pacc <- scdwell_pacc %>%
  mutate(across(cols_to_scale_scd, scale, .names = "{.col}"))

## Create the PACC scores ##
scdwell_pacc <- scdwell_pacc %>%
  mutate(
    pacc5 = rowMeans(select(., zdrs, zravlt, zfluency, zcoding)), # Calculate the mean for each row
    zpacc5 = scale(pacc5) # Scale the PACC variable
  )  

##################################################
## Creating UNadjusted PACC scores for Age-Well ##
##################################################
# Note that this is a 5 component (with logical mem) PACC whereas the others are adjusted as they only include 4 components

# Rename variables
agewell_pacc$log <- agewell_pacc$`logical mem`

## Standardise components (create z-scores) ##
cols_to_scale_age <- c("zdrs" = "drs", "zcvlt" = "cvlt", "zfluency" = "fluency", "zcoding" = "coding", "zlog" = "log") # New names are labeled with z

agewell_pacc <- agewell_pacc %>%
  mutate(across(cols_to_scale_age, scale, .names = "{.col}"))

## Create the PACC scores ##
agewell_pacc <- agewell_pacc %>%
  mutate(
    unadj_pacc5 = rowMeans(select(., zdrs, cvlt, zfluency, zcoding, zlog)), # Calculate the mean for each row
    unadj_zpacc5 = scale(unadj_pacc5) # Scale the PACC variable
  )  

################################################
## Creating adjusted PACC scores for Age-Well ##
################################################

## Standardize components (create z-scores) ##
cols_to_scale_age_adj <- c("zdrs" = "drs", "zcvlt" = "cvlt", "zfluency" = "fluency", "zcoding" = "coding") # New names are labeled with z

agewell_pacc <- agewell_pacc %>%
  mutate(across(cols_to_scale_age_adj, scale, .names = "{.col}"))

## Create the PACC scores ##
agewell_pacc <- agewell_pacc %>%
  mutate(
    pacc5 = rowMeans(select(., zdrs, cvlt, zfluency, zcoding)), # Calculate the mean for each row
    zpacc5 = scale(pacc5) # Scale the PACC variable
  ) 

# Calculate the Pearson correlation for adjusted and unadjusted PACC
corr_pacc <- cor.test(agewell_pacc$unadj_zpacc5, agewell_pacc$zpacc5, method = "pearson")
print(paste(corr_pacc$conf.int[1], "-", corr_pacc$conf.int[2]))
print(paste(corr_pacc$p.value))

