##### ##### #####     Analysis scripts for descriptive analysis  ##### ##### #####
#                                    June 2018 



# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'ggplot2', 'viridis', 'rcompanion')
getPacks(pkgs)
rm(pkgs)

#--------Means, standard deviation for personality scores------------------------
mean(Data_pers_score$MAE_Score)
sd(Data_pers_score$MAE_Score)

mean (Data_pers_score$FFFS)
sd (Data_pers_score$FFFS)

mean(Data_pers_score$BIS)
sd(Data_pers_score$BIS)

mean(Data_pers_score$BAS_Score)
sd(Data_pers_score$BAS_Score)

mean(Data_pers_score$BAS_Rew_Int)
sd(Data_pers_score$BAS_Rew_Int)

mean(Data_pers_score$BAS_Rew_Reac)
sd(Data_pers_score$BAS_Rew_Reac)

mean(Data_pers_score$BAS_Goal_Drive)
sd(Data_pers_score$BAS_Goal_Drive)

mean(Data_pers_score$BAS_Impulsiv)
sd(Data_pers_score$BAS_Impulsiv)

mean(Data_pers_score$PE)
sd(Data_pers_score$PE)

mean(Data_pers_score$AC)
sd(Data_pers_score$AC)

mean(Data_pers_score$SP)
sd(Data_pers_score$SP)

#--------Reliability personality scales----------------------
#--------Bar plot frequency of cards -----------------

Graph <- Data_sum %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))%>%
  dplyr::filter(!Card==0)

ggplot(Graph, aes(x=Block, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Gewählte Stapel (Mittelwert)") +
  scale_fill_viridis(option="viridis", discrete = T, name=("Karte")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=15, face = "bold"))+
  theme(legend.text = element_text(size=12))

#--------Bar plot IGT-Score per Block-----------------

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
  ylab("IGT-Score (Mittelwert)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(-10,10,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))

#--------Bar plot Payoff per Block------------------------

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
  ylab("Punktestand (Mittelwert)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))

#--------Bar plot RT_mean by Block----------------------

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
  ylab("RT (Mittelwert)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))




