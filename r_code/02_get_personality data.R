##### ##### #####     Analysis scripts for personality data   ##### ##### #####
#                                 June 2018 
#                                     


# Load helper functions
setwd("")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'foreign', 'psych')
getPacks(pkgs)
rm(pkgs)

# ----- 1) Read Soscisurvey file -------------------------
Data_pers <- read.csv(file = "./Perso_IGT.csv", 
                      sep = ";", header=T)

# ----- 2) Remove Pilot Data -----------------------------
Data_pers <- Data_pers[-c(1, 2, 7), ]

# ----- 3) Recode CASE into VP ---------------------------

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
MAE <- Data_pers %>% dplyr::select(VP, AE01_01:AE01_30)

# Chronbach's Alpha (internal concistency) 
psych::alpha(MAE[, grep(names(MAE), pattern = '^AE')], 
             check.keys = TRUE)

# COMPUTE SCALE SCORES
# Check internal concistency
psych::alpha(dplyr::select(MAE, AE01_02, AE01_04, AE01_07, AE01_09, AE01_12, 
                    AE01_18, AE01_20, AE01_24, AE01_26, AE01_29),
             check.keys=TRUE)    # Well-Being Subscale

psych::alpha(dplyr::select(MAE, AE01_01, AE01_05, AE01_08, AE01_11, AE01_14, 
                    AE01_16, AE01_17, AE01_22, AE01_25, AE01_28), 
             check.keys=TRUE)    # Achievement Striving Subscale

psych::alpha(dplyr::select(MAE, AE01_03, AE01_06, AE01_10, AE01_13, AE01_15, 
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

MAE_scales <- dplyr::select(MAE, VP, PE, AC, SP, MAE_Score)


# ----- 6) Export MAE data --------------------------------
#write.table(MAE_scales, './agency_scores.txt', row.names = F)


# ----- 7) Compute RST scale scores -----------------------

# Select RST data
RST <- Data_pers %>% dplyr::select(VP, RS01_01:RS01_84)

# Drop control items
RST <- select(RST, -RS01_63, -RS01_49, -RS01_59, -RS01_72)

# Chronbach's Alpha (internal concistency) 
psych::alpha(RST[, grep(names(RST), pattern = '^RS')], 
             check.keys = TRUE)

# Chronbach's Alpha (internal concistency) for sub scales
# FFFS
psych::alpha(dplyr::select(RST, RS01_10, RS01_24, RS01_52, 
                           RS01_60, RS01_61, RS01_64, RS01_69,
                           RS01_77, RS01_78, RS01_81), 
             check.keys = TRUE)

# BIS
psych::alpha(dplyr::select(RST, RS01_01, RS01_02, RS01_07, 
                           RS01_08, RS01_11, RS01_21, RS01_23, 
                           RS01_28, RS01_37, RS01_41, RS01_42, 
                           RS01_55, RS01_56, RS01_62, RS01_65, 
                           RS01_66, RS01_74, RS01_75, RS01_76, 
                           RS01_79, RS01_80, RS01_82, RS01_83), 
             check.keys = TRUE)

# BAS Reward Interest
psych::alpha(dplyr::select(RST, RS01_12, RS01_15, RS01_17, 
                           RS01_18, RS01_33, RS01_40, RS01_44), 
             check.keys = TRUE)

# BAS Reward Reactivity
psych::alpha(dplyr::select(RST, RS01_03, RS01_04, RS01_09, RS01_19, 
                           RS01_30, RS01_31, RS01_32, RS01_38, 
                           RS01_45, RS01_47), 
             check.keys = TRUE)

# BAS Goal-Drive Persistance
psych::alpha(dplyr::select(RST, RS01_05, RS01_13, RS01_25, RS01_39, 
                           RS01_54, RS01_71, RS01_84), 
             check.keys = TRUE)

# BAS Impulsivity
psych::alpha(dplyr::select(RST, RS01_29, RS01_35, RS01_36, RS01_48, 
                           RS01_53, RS01_57, RS01_68, RS01_70), 
             check.keys=TRUE)

# BAS ALL
psych::alpha(dplyr::select(RST, RS01_12, RS01_15, RS01_17, 
                           RS01_18, RS01_33, RS01_40, RS01_44,
                           RS01_03, RS01_04, RS01_09, RS01_19, 
                           RS01_30, RS01_31, RS01_32, RS01_38, 
                           RS01_45, RS01_47,
                           RS01_05, RS01_13, RS01_25, RS01_39, 
                           RS01_54, RS01_71, RS01_84,
                           RS01_29, RS01_35, RS01_36, RS01_48, 
                           RS01_53, RS01_57, RS01_68, RS01_70), 
             check.keys=TRUE)


# ----- Create aggregated scale values
# RST SCALE VALUES
RST <- dplyr::mutate(RST, 
                     FFFS = (RS01_10 + RS01_24 + RS01_52 + RS01_60 + RS01_61 + RS01_64 + RS01_69 + RS01_77 + RS01_78 + RS01_81),
                     BIS = (RS01_01 + RS01_02 + RS01_07 + RS01_08 + RS01_11 + RS01_21 + RS01_23 + RS01_28 + 
                              RS01_37 + RS01_41 + RS01_42 + RS01_55 + RS01_56 + RS01_62 + RS01_65 + RS01_66 +
                              RS01_74 + RS01_75 + RS01_76 +  RS01_79 + RS01_80 + RS01_82 + RS01_83),
                     BAS_Rew_Int = (RS01_12 + RS01_15 + RS01_17 + RS01_18 + RS01_33 + RS01_40 + RS01_44),
                     BAS_Rew_Reac = (RS01_03 + RS01_04 + RS01_09 + RS01_19 + RS01_30 + RS01_31 + RS01_32 + RS01_38 + RS01_45 + RS01_47),
                     BAS_Goal_Drive = (RS01_05 + RS01_13 + RS01_25 + RS01_39 + RS01_54 + RS01_71 + RS01_84),
                     BAS_Impulsiv = (RS01_29 +  RS01_35 + RS01_36 + RS01_48 + RS01_53 + RS01_57 + RS01_68 + RS01_70))

# ----- Overall RST-SCORE
# RST SCORE
RST <- dplyr::mutate(RST, 
                     BAS_Score = (BAS_Rew_Int + BAS_Rew_Reac + BAS_Goal_Drive + BAS_Impulsiv))

RST_scales <- dplyr::select(RST, VP, FFFS:BAS_Score)

# ----- 8) Export RST data --------------------------------
#write.table(RST_scales, './RST_Scales.txt', row.names = F)

#-------9) Combine RST and MAE-----------------------------
Data_pers_score <- merge (MAE_scales, RST_scales, by.x = 'VP', by.y = 'VP')

#-------10) Add Scores to rest of personality data-----------------------------
Data_pers2 <- Data_pers %>% dplyr::select(VP, SD01, SD02_01, SD07, SD07_09, SD08_01, SD09_01, SD22, 
                                         GA21_01:GA23_09)
Data_pers_full <- merge (Data_pers2, Data_pers_score, by.x = 'VP', by.y = 'VP')

#-------11) Delete work-in-progress-data
rm(MAE, MAE_scales, RST, RST_scales, Data_pers2)
