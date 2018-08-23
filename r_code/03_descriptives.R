##### ##### #####     Analysis scripts for descriptive analysis  ##### ##### #####
#                                    June 2018 



# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'ggplot2', 'viridis', 'rcompanion')
getPacks(pkgs)
rm(pkgs)

##-----------Part A: Behavioral data-------------------------

# means, standard deviation
mean(Data_card$RT)
sd(Data_card$RT)
mean(Data_card$Payoff)
sd(Data_card$Payoff)

# Payoff per Block

Graph2 <- dplyr::select(Data_card, VP,Block,Payoff)
Graph2 <- Data_card %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(Payoff), SD=sd(Payoff), SE=sd(Payoff)/sqrt(sum(!is.na(Payoff))))
 
Graph2$Block<-as.factor(Graph2$Block)

ggplot(Graph2, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Payoff(means)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  ggtitle("Payoff") +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  theme_bw()+
  theme(legend.position="none")

#IGT-Score per Block

Graph3 <- dplyr::select(Data_score, VP,Block,IGT_Score)
Graph3 <- Data_score %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(IGT_Score), SD=sd(IGT_Score), SE=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))))

Graph3$Block<-as.factor(Graph3$Block)

ggplot(Graph3, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("IGT-Score (means)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  ggtitle("IGT-Score") +
  scale_y_continuous(breaks=seq(-10,10,5)) +
  theme_bw()+
  theme(legend.position="none")

# RT_mean by Block

Graph4 <- dplyr::select(Data_card, VP,Block,RT)
Graph4 <- Data_card %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(RT), SD=sd(RT), SE=sd(RT)/sqrt(sum(!is.na(RT))))

Graph4$Block<-as.factor(Graph4$Block)

ggplot(Graph4, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("RT (means)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  ggtitle("Reaktionszeit") +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")


# Check normal distribution 
plotNormalHistogram(Data_reg$RT_log)

plotNormalHistogram(Data_reg$RT_mean)

plotNormalHistogram(Data_reg$IGT_Score)

plotNormalHistogram(Data_reg$Payoff)

plotNormalHistogram(Data_full$RT)

   



## ----------Part B: Personality data-------------------------

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
hist(Data_pers_score$BAS_Impulsiv)

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

##Gambling experience
Data_gambling<-dplyr::select(Data_pers_full, VP, GA21_01:GA21_09)

Data_gambling<- dplyr::mutate(Data_gambling,Score_total=(GA21_01 + GA21_02 + GA21_03 + 
                                                           GA21_04 + GA21_05 + GA21_06 +
                                                           GA21_07 + GA21_08 + GA21_09))

Data_gambling$Score_total<-as.factor(Data_gambling$Score_total)
Data_gambling$Score_final <- plyr::revalue(Data_gambling$Score_total, c('9' = '0', '10'='1','11'='2', '12'='3', '12'='4'))

count(Data_gambling, 'Score_final')

#1= Nein, 2=Ja. 




