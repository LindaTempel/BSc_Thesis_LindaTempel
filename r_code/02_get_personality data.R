##### ##### #####     Analysis scrips for personality data   ##### ##### #####
#                                 June 2018 
#                                     


# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
source('./r_funtions/getPacks.R') # <- path to getPacks function

# Load nescessary packages
pkgs <- c('dplyr', 'plyr', 'foreign', 'psych')
getPacks(pkgs)
rm(pkgs)

# ----- 1) Soscisurvey Datei einlesen ---------------------
Data_pers <- read.csv(file = "./Perso_IGT.csv", 
                      sep = ";", header=T)

# ----- 2) Pilot daten rauswerfen -------------------------
Data_pers <- Data_pers[-c(1, 2, 7), ]

# ----- 3) CASE in VP umcodieren --------------------------

# int to factor
Data_pers$CASE <- as.factor(Data_pers$CASE)
# recode
Data_pers$VP <- plyr::revalue(Data_pers$CASE, c('24' = '101', '25' = '102', 
                                                '26' = '103', '27' = '104', 
                                                '33' = '106', '34' = '107', 
                                                '35' = '108', '36' = '109', 
                                                '37' = '110', '38' = '111', 
                                                '39' = '112', '40' = '113', 
                                                '41' = '114', '42' = '115', 
                                                '43' = '116', '44' = '117', 
                                                '45' = '118', '46' = '119', 
                                                '47' = '120', '48' = '121',
                                                '49' = '122', '50' = '123', 
                                                '51' = '124', '52' = '125', 
                                                '53' = '126', '54' = '127', 
                                                '55' = '128', '56' = '129', 
                                                '58' = '130', '59' = '105'))

# ----- 4) Select relevant variables ---------------------
Data_pers <- Data_pers %>% dplyr::select(VP, SD01, SD02_01, SD07, SD07_09, SD08_01, SD09_01, SD22, 
                                        AE01_01:AE01_30, 
                                        RS01_01: RS01_84, 
                                        GA21_01:GA23_09)


# ----- 5) Compute MAE scale scores -----------------------

# Select MAE data
MAE <- Data_pers %>% select(VP, AE01_01:AE01_30)

# Chronbach's Alpha (internal concistency) 
psych::alpha(MAE[, grep(names(MAE), pattern = '^AE')], 
             check.keys = TRUE)

# COMPUTE SCALE SCORES
# Check internal concistency
psych::alpha(select(MAE, AE01_02, AE01_04, AE01_07, AE01_09, AE01_12, 
                    AE01_18, AE01_20, AE01_24, AE01_26, AE01_29), 
             check.keys=TRUE)    # Well-Being Subscale

psych::alpha(select(MAE, AE01_01, AE01_05, AE01_08, AE01_11, AE01_14, 
                    AE01_16, AE01_17, AE01_22, AE01_25, AE01_28), 
             check.keys=TRUE)    # Achievement Striving Subscale

psych::alpha(select(MAE, AE01_03, AE01_06, AE01_10, AE01_13, AE01_15, 
                    AE01_19, AE01_21, AE01_23, AE01_27, AE01_30), 
             check.keys=TRUE)    # Social Potency Subscale

# MAE SCALE VALUES
# Create aggregated scale values
MAE <- MAE %>% mutate( PE = (AE01_02 + AE01_04 - AE01_07 + AE01_09 + AE01_12 + AE01_18 + 
                               AE01_20 - AE01_24 + AE01_26 + AE01_29),
                       AC = (AE01_01 + AE01_05 - AE01_08 - AE01_11 + AE01_14 - AE01_16 + 
                               AE01_17 + AE01_22 - AE01_25 + AE01_28),
                       SP = (AE01_03 + AE01_06 + AE01_10 + AE01_13 - AE01_15 + AE01_19 - 
                               AE01_21 + AE01_23 + AE01_27 + AE01_30))

# MAE SCORE (Overall SCORE)
MAE <- MAE %>% mutate(MAE_Score = (PE+AC+SP))

agentic_ext <- dplyr::select(MAE, VP, PE, AC, SP, MAE_Score)


# ----- 5) Export MAE data --------------------------------
#write.table(agentic_ext, './agency_scores.txt', row.names = F)


# ----- 6) 

