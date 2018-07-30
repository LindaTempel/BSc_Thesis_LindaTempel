##### ##### #####     Analysis scripts for descriptive analysis  ##### ##### #####
#                                    June 2018 



# Load helper functions
setwd("")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr')
getPacks(pkgs)
rm(pkgs)

##-----------Part A: Behavioral data-------------------------

# means, standard deviation
mean(Data_card$RT)
sd(Data_card$RT)
mean(Data_card$Payoff)
sd(Data_card$Payoff)




# graphs


hist(Data_card$RT, xlab = "RT", ylab = "Häufigkeit", main = "Verteilung RT") 

hist(Data_card$Payoff, xlab = "payoff", ylab = "Häufigkeit", main = "Verteilung payoff")

boxplot(Data_card$Payoff)    



## ---------Part B: Personality data-------------------------

# means, standard deviation
mean(Data_pers_score$MAE_Score)
sd(Data_pers_score$MAE_Score)

mean (Data_pers_score$FFFS)
sd (Data_pers_score$FFFS)

mean(Data_pers_score$BIS)
sd(Data_pers_score$BIS)

mean(Data_pers_score$BAS_Score)
sd(Data_pers_score$BAS_Score)

#graphs

hist(Data_pers_score$MAE_Score)
hist(Data_pers_score$FFFS)
hist(Data_pers_score$BIS)
hist(Data_pers_score$BAS_Score)

#sample characteristics

range(Data_pers_full$SD02_01)
mean(Data_pers_full$SD02_01)
sd(Data_pers_full$SD02_01)


count(Data_pers_full, 'SD01')

Data_pers_full$SD09_01 <- plyr::revalue(Data_pers_full$SD09_01, c('Psychologie B.Sc.' = 'Psychologie',
                                                                  'Psychologiestudentin ' = 'Psychologie',
                                                                  'Psychologie, Bsc.' = 'Psychologie',
                                                                  'Bachelor Psychologie' ='Psychologie', 
                                                                  'Master Chemie' = 'Chemie', 
                                                                  'MSc Chemie'= 'Chemie', 
                                                                  'B.A. Sozialwissenschaften' = 'Sozialwissenschaften'))
count(Data_pers_full, 'SD09_01')


mean(Data_pers_full$SD08_01)

##Percentages

SD01_per <- Data_pers_full %>% 
  count('SD01') %>%            
  mutate(per = freq/sum(freq)*100)

SD09_01_per <- Data_pers_full %>% 
  count('SD09_01') %>%            
  mutate(per = freq/sum(freq)*100)


