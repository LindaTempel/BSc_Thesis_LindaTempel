
### Soscisurvey Datei einlesen

library(foreign)

file.choose()

Data_pers = read.spss("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\SPSS\\Fragebogen_Data.sav", to.data.frame=TRUE, use.value.labels = F)

###
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
Data_pers2 <- read.csv(file = "data_IGT-Ca_2018-06-01_09-52.csv", sep = "\t", header=T)
##


## Fälle löschen (unsere Eingaben)
Data_pers <- Data_pers[-c(1, 2, 7), ]

Data_pers$CASE <- factor(Data_pers$CASE)
Data_pers$VP <- plyr::revalue(Data_pers$CASE, c('24' = '101', '25' = '102', '26' = '103', '27' = '104', '33' = '106', 
                                                '34' = '107', '35' = '108', '36' = '109', '37' = '110', '38' = '111', 
                                                '39' = '112', '40' = '113', '41' = '114', '42' = '115', '43' = '116', 
                                                '44' = '117', '45' = '118', '46' = '119', '47' = '120', '48' = '121',
                                                '49' = '122', '50' = '123', '51' = '124', '52' = '125', '53' = '126', 
                                                '54' = '127', '55' = '128', '56' = '129', '58' = '130', '59' = '105'))

Data_pers<- Data_pers %>% dplyr::select(VP, AE01_01, AE01_02, AE01_03, AE01_04, AE01_05, AE01_06, AE01_07, AE01_08, AE01_09, AE01_10, 
                                        AE01_11, AE01_12, AE01_13, AE01_14, AE01_15, AE01_16, AE01_17, AE01_18, AE01_19, AE01_20,
                                        AE01_21, AE01_22, AE01_23, AE01_24, AE01_25, AE01_26, AE01_27, AE01_28, AE01_29, AE01_30, 
                                        RS01_01, RS01_02, RS01_03, RS01_04, RS01_05, RS01_06, RS01_07, RS01_08, RS01_09, RS01_10,
                                        RS01_11, RS01_12, RS01_13, RS01_14, RS01_15, RS01_16, RS01_17, RS01_18, RS01_19, RS01_20, 
                                        RS01_21, RS01_22, RS01_23, RS01_24, RS01_25, RS01_26, RS01_27, RS01_28, RS01_29, RS01_30, 
                                        RS01_31, RS01_32, RS01_33, RS01_34, RS01_35, RS01_36, RS01_37, RS01_38, RS01_39, RS01_40, 
                                        RS01_41, RS01_42, RS01_43, RS01_44, RS01_45, RS01_46, RS01_47, RS01_48, RS01_49, RS01_50, 
                                        RS01_51, RS01_52, RS01_53, RS01_54, RS01_55, RS01_56, RS01_57, RS01_58, RS01_59, RS01_60,
                                        RS01_61, RS01_62, RS01_63, RS01_64, RS01_65, RS01_66, RS01_68, RS01_69, RS01_70, 
                                        RS01_71, RS01_72, RS01_73, RS01_74, RS01_75, RS01_76, RS01_77, RS01_78, RS01_79, RS01_80,
                                        RS01_81, RS01_82, RS01_83, RS01_84, SD01, SD02_01, SD07, SD07_09, SD08_01, SD09_01, SD22, 
                                        GA21_01, GA21_02, GA21_03, GA21_04, GA21_05, GA21_05, GA21_06, GA21_07, GA21_08, GA21_09,
                                        GA01_01, GA01_02, GA01_03, GA01_04, GA01_05, GA01_05, GA01_06, GA01_07, GA01_08, GA01_09,
                                        GA22_01, GA22_02, GA22_03, GA22_04, GA22_05, GA22_05, GA22_06, GA22_07, GA22_08, GA22_09,
                                        GA23_01, GA23_02, GA23_03, GA23_04, GA23_05, GA23_05, GA23_06, GA23_07, GA23_08, GA23_09)


## Datensätze verbinden
Data_full<- merge (Data_card, Data_pers, by.x='VP', by.y = 'VP')

