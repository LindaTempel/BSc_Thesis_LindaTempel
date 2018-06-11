##### ##### #####     Analysis scrips for personality data   ##### ##### #####
#                                 June 2018 
#                                     


# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
source('./r_funtions/getPacks.R') # <- path to getPacks function

# Load nescessary packages
pkgs <- c('dplyr', 'plyr', 'foreign')
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
Data_pers<- Data_pers %>% dplyr::select(VP, SD01, SD02_01, SD07, SD07_09, SD08_01, SD09_01, SD22, 
                                        AE01_01:AE01_30, 
                                        RS01_01: RS01_84, 
                                        GA21_01:GA23_09)


# ----- 5) Compute scale scores





## Datens?tze verbinden
Data_full<- merge (Data_card, Data_pers, by.x='VP', by.y = 'VP')

