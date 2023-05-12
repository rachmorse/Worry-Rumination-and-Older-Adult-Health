###############################################
### Creating PACC-5 for Age-Well & SCD-Well ###
###############################################

if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if (!require("psych")) {install.packages("psych"); require("psych")}

library(readxl)
data <- MS_Data <- read_excel("~/Documents/2021:2022/Marchant Lab/Data/MS Data.xlsx", 
                              sheet = "Summary", col_types = c("text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric","numeric", "numeric", "numeric", 
                                                               "numeric", "text", "numeric", "text", 
                                                               "numeric","numeric","numeric", "numeric","numeric","numeric","numeric"))

ageonlypacc <- slice(data, 148:284)

scdonlypacc <- slice(data, 1:147)

attach(ageonlypacc)
ageonlypacc$cvlt <- (`ravlt OR cvlt (in Age-Well) this column can only be use for separate cohort analyses`)

ageonlypacc <- ageonlypacc %>% 
  filter(!is.na(cvlt)) %>% 
  mutate(cvlt_rvlt = (cvlt/16*15))

attach(scdonlypacc)
scdonlypacc$ravlt <- (`ravlt OR cvlt (in Age-Well) this column can only be use for separate cohort analyses`)
scdonlypacc$cvlt_rvlt <- scdonlypacc$ravlt  

merged_df <- full_join(scdonlypacc, ageonlypacc, by = c("id","drs","cvlt_rvlt","coding","fluency"))

merged_df <- merged_df[complete.cases(merged_df$drs, merged_df$cvlt_rvlt, merged_df$fluency, 
                                      merged_df$coding), ]

## Standardise components (create z-scores) ##
merged_df$zdrs <- scale(merged_df$drs)
merged_df$zcvlt_rvlt <- scale(merged_df$cvlt_rvlt)
merged_df$zfluency <- scale(merged_df$fluency)
merged_df$zcoding <- scale(merged_df$coding)

## Create a matrix of the Z scores ##
zScores <- cbind (merged_df$zdrs, merged_df$zcvlt_rvlt, merged_df$zfluency, merged_df$zcoding) 

## Average standardised components to create the PACC-5 ##
merged_df$pacc5 <- rowMeans(zScores[,1:4], na.rm=TRUE)
rm(zScores)

merged_df$zpacc5 <- scale(merged_df$pacc5)

write.csv(merged_df,"~/MS RNT Physical Cognitive Health/\\ZPACC5 (combined cohorts for use in MS analysis).csv", row.names = FALSE)

## CREATING SCD PACC ##

## Standardise components (create z-scores) ##
scdonlypacc$zdrs <- scale(scdonlypacc$drs)
scdonlypacc$zravlt <- scale(scdonlypacc$ravlt)
scdonlypacc$zfluency <- scale(scdonlypacc$fluency)
scdonlypacc$zcoding <- scale(scdonlypacc$coding)

## Create a matrix of the Z scores ##
zScores_scdonlypacc <- cbind (scdonlypacc$zdrs, scdonlypacc$zravlt, scdonlypacc$zfluency, scdonlypacc$zcoding) 

## Average standardised components to create the PACC-5 ##
scdonlypacc$pacc5 <- rowMeans(zScores_scdonlypacc[,1:4], na.rm=TRUE)
rm(zScores_scdonlypacc)

scdonlypacc$zpacc5 <- scale(scdonlypacc$pacc5)

write.csv(scdonlypacc,"~/MS RNT Physical Cognitive Health/\\ZPACC5_scdpacc.csv", row.names = FALSE)

## CREATING AGE PACC ## ----
attach(ageonlypacc)
ageonlypacc$log <- (`logical mem`)

## Standardise components (create z-scores) ##
ageonlypacc$zdrs <- scale(ageonlypacc$drs)
ageonlypacc$zcvlt <- scale(ageonlypacc$cvlt)
ageonlypacc$zfluency <- scale(ageonlypacc$fluency)
ageonlypacc$zcoding <- scale(ageonlypacc$coding)
ageonlypacc$zlog <- scale(ageonlypacc$log)


## Create a matrix of the Z scores ##
zScores_ageonlypacc <- cbind (ageonlypacc$zdrs, ageonlypacc$zcvlt, ageonlypacc$zfluency, ageonlypacc$zcoding, ageonlypacc$zlog) 

## Average standardised components to create the PACC-5 ##
ageonlypacc$pacc5 <- rowMeans(zScores_ageonlypacc[,1:5], na.rm=TRUE)
rm(zScores_ageonlypacc)

ageonlypacc$zpacc5 <- scale(ageonlypacc$pacc5)

as.numeric(ageonlypacc$zpacc5)
describe(ageonlypacc$zpacc5)

write.csv(ageonlypacc,"~/MS RNT Physical Cognitive Health/\\ZPACC5_agepacc.csv", row.names = FALSE)

## CREATING ADJUTSED ADJUSTED ADJUSTED!!! AGE PACC ## ----

## Standardise components (create z-scores) ##
ageonlypacc$zdrs <- scale(ageonlypacc$drs)
ageonlypacc$zcvlt <- scale(ageonlypacc$cvlt)
ageonlypacc$zfluency <- scale(ageonlypacc$fluency)
ageonlypacc$zcoding <- scale(ageonlypacc$coding)

## Create a matrix of the Z scores ##
zScores_ageonlyadjpacc <- cbind (ageonlypacc$zdrs, ageonlypacc$zcvlt, ageonlypacc$zfluency, ageonlypacc$zcoding) 

## Average standardised components to create the PACC-5 ##
ageonlypacc$adjpacc5 <- rowMeans(zScores_ageonlyadjpacc[,1:4], na.rm=TRUE)
rm(zScores_ageonlyadjpacc)

ageonlypacc$adjzpacc5 <- scale(ageonlypacc$adjpacc5)

write.csv(ageonlypacc,"~/MS RNT Physical Cognitive Health/\\ZPACC5_agepaccadj.csv", row.names = FALSE)

###CORRELATION ADJ UNADJ PACC 
if(!require(devtools)) install.packages("devtools")
cor.test(ageonlypacc$adjzpacc5,ageonlypacc$zpacc5, method=("pearson"))

## PACC5 descriptive statistics ##
describe(merged_df$pacc5)
shapiro.test(merged_df$pacc5) # normally distributed
ggdensity(merged_df$pacc5, fill = "lightpink")

describe(merged_df$zpacc5)
shapiro.test(merged_df$zpacc5) # normally distributed
ggdensity(merged_df$zpacc5, fill = "lightpink")

write.csv(merged_df,"~/MS RNT Physical Cognitive Health/\\ZPACC5.csv", row.names = FALSE)

