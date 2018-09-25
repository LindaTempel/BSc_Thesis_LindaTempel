##### ##### #####     Analysis scripts for inferential statistics  ##### ##### #####
#                                    June 2018 

# Load helper functions
setwd("")
source('./r_functions/getPacks.R') # <- path to getPacks function
source('./r_functions/flattenCorrMatrix.R')
# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'multcomp', 'effects', 'phia', 'emmeans', 'lme4',
          'sjPlot', 'lmerTest', 'stargazer', 'lemon', 'gridExtra', 'ggplot2', 'tidyr',
          'reshape2', 'corrplot', 'visreg', 'pwr', 'lattice', 'viridis', 'rcompanion',
          'apaTables', 'scales', 'sjstats')
getPacks(pkgs)
rm(pkgs)


#CREATE DATA FRAMES FOR ANALYSIS

#--------1) Merge behavioral and personality data---------
Data_full<- merge (Data_card, Data_pers_full, by.x='VP', by.y = 'VP')

Data_full$Block<-as.factor(Data_full$Block)


#--------2) Data frame for frequency of cards---------

Data_sum <- Data_full %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)


#--------3) Calculating IGT-Score, RT mean and Payoff-------------------------

#turning long into wide format
Data_score <- dcast(Data_sum, VP + Block ~ Card, value.var="N")

#Switching columns
Data_score <- Data_score[c("VP", "Block", "A", "B", "C", "D")]

#----IGT-Score

#Calculating Score for each Block
Data_score[is.na(Data_score)] <- 0
Data_score<- dplyr::mutate(Data_score,IGT_Score=((C+D)-(A+B)))

#Calculating total IGT_Score

Data_score_all <- Data_score %>% 
  dplyr::group_by(VP) %>% 
  dplyr::summarise(IGT_Score_all=sum(IGT_Score))

#Merge with personality scores
Data_pers_score_all <- merge (Data_score_all, Data_pers_score) 

#----Payoff

#Select payoff at end of Block only
Data_behav3<-Data_behav %>% dplyr::select(VP, Block,Trial, Payoff)
Data_behav3<-dplyr::filter(Data_behav3, Trial==100)
Data_behav3<-Data_behav3 %>% dplyr::select(VP, Block,Payoff)

#Add payoff to data frame
Data_reg <- merge (Data_score, Data_behav3)
Data_reg <- merge (Data_reg, Data_pers_score, by.x = 'VP', by.y = 'VP')

#----RT

#Calculating mean of RT for each Block
Data_RT <- Data_card %>% 
  dplyr::group_by(VP, Block) %>% 
  dplyr::summarise(RT_mean=mean(RT))

#Add RT_mean to data frame
Data_reg <- merge (Data_reg, Data_RT)

#Log Transformation for RT
Data_reg$RT_log = log(Data_reg$RT_mean)

rm(Data_behav3, Data_score, Data_score_all)

#--------3) Alternative: bigger data set for Analysis with slopes------

Data_RT_all <- Data_card %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(RT_mean=mean(RT)) %>%
  dplyr::filter(!Card==0)

perso <- dplyr::select(Data_pers_score, BAS_Score, MAE_Score, BIS, FFFS, 
                       PE, AC, SP, BAS_Rew_Int, 
                       BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, VP)
Data_reg_all <- merge(Data_sum, perso, 'VP')
Data_reg_all <-merge(Data_reg_all, Data_RT_all)
Data_reg_all <-merge(Data_reg_all, Data_score)
Data_reg_all <- merge (Data_reg_all, Data_behav3)
Data_reg_all$VP <-as.factor(Data_reg_all$VP)  

Data_reg_all$RT_log = log(Data_reg_all$RT_mean)


#--------4) Center around zero: Personality variables----

Data_reg <- within(Data_reg, {
  MAE_Score_z <- MAE_Score - mean(MAE_Score, na.rm=T)
  BAS_Score_z <- BAS_Score - mean(BAS_Score, na.rm=T )
  BIS_z <- BIS - mean(BIS, na.rm=T)
  FFFS_z <- FFFS - mean(FFFS, na.rm=T)
  BAS_Rew_Int_z <- BAS_Rew_Int - mean(BAS_Rew_Int, na.rm=T)
  BAS_Rew_Reac_z <- BAS_Rew_Reac - mean(BAS_Rew_Reac, na.rm=T)
  BAS_Goal_Drive_z <- BAS_Goal_Drive - mean(BAS_Goal_Drive, na.rm=T)
  BAS_Impulsiv_z <- BAS_Impulsiv - mean(BAS_Impulsiv, na.rm=T)
  PE_z <- PE - mean(PE, na.rm=T)
  AC_z <- AC - mean(AC, na.rm=T)
  SP_z <- SP - mean(SP, na.rm=T)
})

#--------5) Data frame for RT after losses/wins---------

Data_RT_split<-dplyr::select(Data_full, VP, Block, RT, Card, net_payoff)

Data_RT_split<-dplyr::filter(Data_RT_split, Card %in% c('A', 'B', 'C', 'D'))
Data_RT_split$Card <- factor(Data_RT_split$Card)

#Separate Data by wins and losses
Data_RT_loss <- dplyr::filter(Data_RT_split, lag(net_payoff, 1)<0)
Data_RT_win <- dplyr::filter(Data_RT_split, lag(net_payoff,1)>0)

#Create Variable Condition
Data_RT_loss$Cond<-c("loss")
Data_RT_win$Cond<-c("win")

#Create new data frame
Data_loss_win<-rbind(Data_RT_loss, Data_RT_win)

Data_loss_win$Cond <- as.factor(Data_loss_win$Cond)
Data_loss_win$VP <- as.factor(Data_loss_win$VP)

rm(Data_RT_loss, Data_RT_win, Data_RT_split)

#Calculate mean RT

Data_loss_win <- Data_loss_win %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

#log transform RT
Data_loss_win$RT_log = log(Data_loss_win$RT_mean)
Data_reg$RT_log = log(Data_reg$RT_mean)


#--------------------------------------------------------------

#ANALYSIS OF PERSONALITY DATA

#--------1) Correlations in Personality data----------------------


Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score)

matrix <- cor(Data_pers_score_all)
round(matrix,2)

#Switching columns
Data_pers_score_all <- Data_pers_score_all[c("MAE_Score", "PE", "AC", "SP", "FFFS", "BIS", "BAS_Score",
                                             "BAS_Rew_Int", "BAS_Rew_Reac", "BAS_Goal_Drive", "BAS_Impulsiv")]

matrix2<-rcorr(as.matrix(Data_pers_score_all), type=c("pearson"))

#for p-values
matrix2

matrix2$r
matrix2$P

flattenCorrMatrix(matrix2$r, matrix2$P)
rcorr(matrix, type=c("pearson"))
matrix$P

#Rename variables
colnames(matrix2$r) <- c("Extraversion", "Glücklichkeit", "Leistung", "Soziale Stärke", 
                         "FFFS", "BIS", "BAS", "Interesse a.B.", 
                         "Reaktivität a.B.", "z. Beharrlichkeit", "Impulsivität")
rownames(matrix2$r) <- c("Extraversion", "Glücklichkeit", "Leistung", "Soziale Stärke", 
                         "FFFS", "BIS", "BAS", "Interesse a.B", 
                         "Reaktivität a.B.", "z. Beharrlichkeit", "Impulsivität")

# Plot: only significant correlation 
corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)

#Plot: all correlation
corrplot(matrix2$r, method=c("number"), type="full", order="original", tl.col="black", tl.srt=45)

#--------------------------------------------------------------

#ANALYSIS OF BEHAVIORAL DATA

#--------1) Chosen Decks----------------

#Effect-coding
contrasts(Data_reg_all$Block) <- contr.sum(3); contrasts(Data_reg_all$Block)
Data_reg_all$Card <-droplevels.factor(Data_reg_all$Card, exclude= c(0))
contrasts(Data_reg_all$Card)  <- contr.sum(4);contrasts(Data_reg_all$Card)

#----Model with random intercept and random slope, by Card and Block

mod_card <- lmer(N ~ Card*Block + (1+Card|VP), data=Data_reg_all, REML = F)
anova(mod_card)
summary(mod_card)

#Post-hoc

emmeans(mod_card, pairwise ~ Card |Block, adjust='Bonferroni')

#Residuals ok?

sjPlot::plot_model(mod_card, "diag")

#change order of levels 
Data_reg_all$Block <- factor(Data_reg_all$Block, levels = c('2', '3', '1'))
contrasts(Data_reg_all$Block) <- contr.sum(3); contrasts(Data_reg_all$Block)
Data_reg_all$Card <- factor(Data_reg_all$Card, levels = c('B', 'C', 'D', 'A'))
contrasts(Data_reg_all$Card)  <- contr.sum(4);contrasts(Data_reg_all$Card)
#refit model#

#get table

sjPlot::tab_model(mod_card,  show.aic = T, show.std = T, show.r2 = T, 
                  string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Karte B", "Karte C", "Karte D",
                                  "Block 2", "Block 3",
                                  "Karte B*Block 2", "Karte C*Block 2", "Karte D*Block 2", 
                                  "Karte B*Block 3", "Karte C*Block 3", "Karte D*Block 3"), 
                  dv.labels='Wahl der Stapel')

#change order of levels back to normal
Data_reg_all$Block <- factor(Data_reg_all$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg_all$Block) <- contr.sum(3); contrasts(Data_reg_all$Block)
Data_reg_all$Card <- factor(Data_reg_all$Card, levels = c('A', 'B', 'C', 'D'))
contrasts(Data_reg_all$Card)  <- contr.sum(4);contrasts(Data_reg_all$Card)
#refit model#


#--------2) IGT-Score---------------------

#----Model with random intercept, by Block, BAS, BIS, FFFS, MAE

m1<-lmer(IGT_Score ~ 
           Block*BAS_Score_z + 
           Block*BIS_z + 
           Block*FFFS_z + 
           Block*MAE_Score_z + (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1) 

sd(Data_reg$MAE_Score_z)
sd(Data_reg$BIS_z)

#Post-hoc 

emmeans(m1, pairwise~Block|MAE_Score_z, 
        at=list(MAE_Score_z = c(-13, 13)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block|BIS_z, 
        at=list(BIS_z=c(-12, 12)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block, 
        adjust='Bonferroni')

#Graphs

emmip(m1,Block ~ MAE_Score_z, at = list(MAE_Score_z = c(-13, 13)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                     (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Extraversion") + ylab("IGT-Score")  +
      scale_x_discrete(labels=c("-13" = "-1SD", "13" = "+1SD"))

emmip(m1,Block ~ BIS_z, at = list(BIS_z = c(-12, 12)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("BIS") + ylab("IGT-Score") +
      scale_x_discrete(labels=c("-12" = "-1SD", "12" = "+1SD"))

#Residuals ok?

sjPlot::plot_model(m1, "diag") 

#change order of levels
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m1, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "BAS", "BIS", "FFFS", 
                                  "Extraversion", "Block 2*BAS", "Block 3*BAS", 
                                  "Block 2*BIS", "Block 3*BIS", "Block 2*FFFS", 
                                  "Block 3*FFFS", "Block 2*Extraversion", 
                                  "Block 3*Extraversion"), 
                  dv.labels='IGT-Score')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#----Model with random intercept, by Block, Rew_Int, Rew_Reac, Goal_Drive, Impulsiv (BAS-Subscales)

m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int_z +
           Block*BAS_Rew_Reac_z +
           Block*BAS_Goal_Drive_z +
           Block*BAS_Impulsiv_z + (1|VP), data=Data_reg, REML=F)
anova(m2)
summary(m2)

sd(Data_reg$BAS_Goal_Drive_z)
sd(Data_reg$BAS_Rew_Int_z)

#Post-hoc

emmeans(m2, pairwise~Block|BAS_Goal_Drive_z,
        at=list(BAS_Goal_Drive_z= c(-3,3)), 
        adjust='Bonferroni')
emmeans(m2, pairwise~Block|BAS_Rew_Int_z, 
        at=list(BAS_Rew_Int_z = c(-3, 3)),
        adjust='Bonferroni')
emmeans(m2, pairwise~Block, 
        adjust='Bonferroni')


emtrends(m2, var='BAS_Goal_Drive_z', ~1, 
         at=list(BAS_Rew_Int_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')
emtrends(m2, var='BAS_Rew_Int_z', ~1, 
         at=list(BAS_Goal_Drive_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')

#Graphs

emmip(m2,Block ~ BAS_Goal_Drive_z, at = list(BAS_Goal_Drive_z = c(-3, 3)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Zielgerichtete Beharrlichkeit") + ylab("IGT-Score")+ 
      scale_x_discrete(labels=c("-3" = "-1SD", "3" = "+1SD"))

emmip(m2,Block ~ BAS_Rew_Int_z, at = list(BAS_Rew_Int_z = c(-3, 3)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Interesse an Belohnung") + ylab("IGT-Score") +
      scale_x_discrete(labels=c("-3" = "-1SD", "3" = "+1SD"))

visreg(m2, xvar= 'BAS_Rew_Int_z', line=list(col='#2D718EFF'), 
       xlab=("Interesse an Belohnung"), ylab=("IGT-Score"),gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

visreg(m2, xvar='BAS_Goal_Drive_z', line=list(col='#2D718EFF'),
       xlab=("Zielgerichtete Beharrlichkeit"), ylab=("IGT-Score"), gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

#Residuals ok?

sjPlot::plot_model(m2,"diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m2, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Interesse a. Belohnung", 
                                  "Reaktivität a. Belohnung", "z. Beharrlichkeit",
                                  "Impulsivität","Block 2*Interesse a. Belohnung", 
                                  "Block 3*Interesse a. Belohnung",
                                  "Block 2*Reaktivität a. Belohnung", 
                                  "Block 3*Reaktivität a. Belohnung",
                                  "Block 2*z. Beharrlichkeit", "Block 3*z. Beharrlichkeit", 
                                  "Block 2*Impulsivität", "Block 3*Impulsivität"), 
                  dv.labels='IGT-Score')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#----Model with random intercept, by Block, PE, AC, SP (MAE-Subscales)

m3<-lmer(IGT_Score ~   
           Block*PE_z + 
           Block*AC_z + 
           Block*SP_z+
           (1|VP), data=Data_reg, REML=F)
anova(m3)
summary(m3)

sd(Data_reg$SP_z)

#Post-hoc

emmeans(m3, pairwise~Block|SP_z,
        at=list(SP_z=c(-7,7)),
        adjust='Bonferroni')
emmeans(m3, pairwise~Block, 
        adjust='Bonferroni')

#Graphs

emmip(m3,Block ~ SP_z, at = list(SP_z = c(-7, 7)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Soziale Stärke") + ylab("IGT-Score") + 
      scale_x_discrete(labels=c("-7" = "-1SD", "7" = "+1SD"))

#Residuals ok?

sjPlot::plot_model(m3, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m3, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Glücklichkeit", "Leistung",
                                  "soziale Stärke",
                                  "Block 2*Glücklichkeit", "Block 3*Glücklichkeit",
                                  "Block 2*Leistung", "Block 3*Leistung",
                                  "Block 2*soziale Stärke", "Block 3*soziale Stärke"), 
                  dv.labels='IGT-Score')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#--------3) Payofff------------------

#Correlation IGT-Score and Payoff

cor.test(Data_reg$IGT_Score, Data_reg$Payoff)

#----Model with random intercept, by Block, BAS, BIS, FFFS, MAE

m1<-lmer(Payoff~Block*BAS_Score_z+
           Block*BIS_z+
           Block*FFFS_z+
           Block*MAE_Score_z+ (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1)

sd(Data_reg$MAE_Score_z)
sd(Data_reg$BIS_z)

#Post-hoc

emmeans(m1, pairwise~Block|BIS_z,
        at=list(BIS_z=c(-12,12)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block|MAE_Score_z,
        at=list(MAE_Score_z=c(-13,13)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block, 
        adjust='Bonferroni')

#Graphs

emmip(m1,Block ~ MAE_Score_z, at = list(MAE_Score_z = c(-13, 13)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Extraversion") + ylab("Punktestand")  +
      scale_x_discrete(labels=c("-13" = "-1SD", "13" = "+1SD"))

emmip(m1,Block ~ BIS_z, at = list(BIS_z = c(-12, 12)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("BIS") + ylab("Punktestand") + 
      scale_x_discrete(labels=c("-12" = "-1SD", "12" = "+1SD"))

#Residuals ok?

sjPlot::plot_model(m1, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m1, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "BAS", "BIS", "FFFS", 
                                  "Extraversion", "Block 2*BAS", "Block 3*BAS", 
                                  "Block 2*BIS", "Block 3*BIS", 
                                  "Block 2*FFFS", "Block 3*FFFS", 
                                  "Block 2*Extraversion", "Block 3*Extraversion"), 
                  dv.labels='Punktestand')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#----Model with random intercept, by Block, Rew_Int, Rew_Reac, Goal_Drive, Impulsiv (BAS-Subscales)

m2<-lmer(Payoff~Block*BAS_Rew_Int_z+ 
           Block*BAS_Rew_Reac_z+ 
           Block*BAS_Goal_Drive_z+
           Block*BAS_Impulsiv_z+ (1|VP), data=Data_reg, REML=F)
anova(m2)
summary(m2)

#model without interaction
m2_1<-lmer(Payoff~Block+BAS_Rew_Int_z+ 
           Block+BAS_Rew_Reac_z+ 
           Block+BAS_Goal_Drive_z+
           Block+BAS_Impulsiv_z+ (1|VP), data=Data_reg, REML=F)
anova(m2_1)

#Post-hoc

emmeans(m2_1, pairwise~Block, 
        adjust='Bonferroni')
emtrends(m2_1, var='BAS_Goal_Drive_z', ~1, 
         at=list(BAS_Rew_Int_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')
emtrends(m2_1, var='BAS_Rew_Reac_z', ~1, 
         at=list(BAS_Goal_Drive_z=0, BAS_Rew_Int_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')

#Graphs

visreg(m2, xvar='BAS_Goal_Drive_z', line=list(col='#20A386FF'),
       xlab=("Zielgerichtete Beharrlichkeit"), ylab=("Punktestand"), gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

visreg(m2, xvar='BAS_Rew_Reac_z', line=list(col='#20A386FF'),
       xlab=("Reaktivität auf Belohnung"), ylab=("Punktestand"), gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

#Residuals ok?

sjPlot::plot_model(m2, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m2,m2_1, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Interesse a. Belohnung", 
                                  "Reaktivität a. Belohnung",
                                  "z. Beharrlichkeit","Impulsivität",
                                  "Block 2*Interesse a. Belohnung", 
                                  "Block 3*Interesse a. Belohnung",
                                  "Block 2*Reaktivität a. Belohnung", 
                                  "Block 3*Reaktivität a. Belohnung",
                                  "Block 2*z. Beharrlichkeit", "Block 3*z. Beharrlichkeit", 
                                  "Block 2*Impulsivität", "Block 3*Impulsivität"), 
                  dv.labels='Punktestand')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#----Model with random intercept, by Block, PE, AC, SP (MAE-Subscales)

m3<-lmer(Payoff~Block*PE_z+
           Block*AC_z+
           Block*SP_z+(1|VP), data=Data_reg, REML=F)
anova(m3)
summary(m3)

sd(Data_reg$SP_z)

#Post-hoc

emmeans(m3, pairwise~Block|SP_z, 
        at=list(SP_z=c(-7, 7)),
        adjust='Bonferroni')
emmeans(m3, pairwise~Block, 
        adjust='Bonferroni')

#Graphs

emmip(m3,Block ~ SP_z, at = list(SP_z = c(-7, 7)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Soziale Stärke") + ylab("Punktestand") +
      scale_x_discrete(labels=c("-7" = "-1SD", "7" = "+1SD"))

#Residuals ok?

sjPlot::plot_model(m3, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m3, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Glücklichkeit", "Leistung",
                                  "soziale Stärke",
                                  "Block 2*Glücklichkeit", "Block 3*Glücklichkeit",
                                  "Block 2*Leistung", "Block 3*Leistung",
                                  "Block 2*soziale Stärke", "Block 3*soziale Stärke"), 
                  dv.labels='Punktestand')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#--------4) RT_log-----------------

#----Model with random intercept, by Block, BAS, BIS, FFFS, MAE

m1<-lmer(RT_log ~ 
           Block*BAS_Score_z +
           Block*BIS_z + 
           Block*FFFS_z +
           Block*MAE_Score_z + (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1)

#Model without interaction
m1_1<-lmer(RT_log ~ 
           Block+BAS_Score_z +
           Block+BIS_z + 
           Block+FFFS_z +
           Block+MAE_Score_z + (1|VP), data=Data_reg, REML = F)
anova(m1_1)

#Model with inverse RT
m0<-lmer(-1000/RT_mean ~ 
           Block*BAS_Score_z +
           Block*BIS_z + 
           Block*FFFS_z +
           Block*MAE_Score_z + (1|VP), data=Data_reg, REML = F)
anova(m0)
sjPlot::plot_model(m0, "diag")

#Post-hoc

emmeans(m1_1, pairwise~Block, 
        adjust='Bonferroni')
emtrends(m1_1, var='BAS_Score_z', ~1, 
         at=list(BIS_z=0, FFFS_z=0, MAE_Score_z=0), 
         adjust='Bonferroni')
emtrends(m1_1, var='FFFS_z', ~1, 
         at=list(BAS_Score_z=0, BIS_z=0, MAE_Score_z=0), 
         adjust='Bonferroni')

#Graphs

visreg(m1, xvar='BAS_Score_z', line=list(col='#481568FF'),
       xlab=("BAS"), ylab=("Reaktionszeit"), gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

visreg(m1, xvar='FFFS_z', line=list(col='#481568FF'),
       xlab=("FFFS"), ylab=("Reaktionszeit"), gg=T)+ theme_bw()+
      theme(axis.title.x = element_text(size=18))+
      theme(axis.title.y = element_text(size=18))+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.text.y = element_text(size = 15))

#Residuals ok?

sjPlot::plot_model(m1, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m1_1, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "BAS", "BIS", "FFFS", 
                                  "Extraversion"), 
                  dv.labels='Reaktionszeit')

sjPlot::tab_model(m1,m0, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "BAS", "BIS", "FFFS", 
                                  "Extraversion", "Block 2*BAS", "Block 3*BAS", 
                                  "Block 2*BIS", "Block 3*BIS", 
                                  "Block 2*FFFS", "Block 3*FFFS", 
                                  "Block 2*Extraversion", "Block 3*Extraversion"), 
                  dv.labels='Reaktionszeit')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#----Model with random intercept, by Block, Rew_Int, Rew_Reac, Goal_Drive, Impulsiv (BAS-Subscales)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int_z + 
           Block*BAS_Rew_Reac_z + 
           Block*BAS_Goal_Drive_z + 
           Block*BAS_Impulsiv_z + (1|VP), data=Data_reg, REML = F)
anova(m2)
summary(m2)

#Model with inverse RT
m02<-lmer(-1000/RT_mean ~ 
            Block*BAS_Rew_Int_z + 
            Block*BAS_Rew_Reac_z + 
            Block*BAS_Goal_Drive_z + 
            Block*BAS_Impulsiv_z + (1|VP), data=Data_reg, REML = F)
anova(m02)
sjPlot::plot_model(m02, "diag")

sd(Data_reg$BAS_Rew_Int_z)

#Post-hoc

emmeans(m2, pairwise~Block|BAS_Rew_Int_z, 
        at=list(BAS_Rew_Int_z=c(-3,3)),
        adjust='Bonferroni')
emmeans(m2, pairwise~Block, 
        adjust='Bonferroni')

#Graphs

emmip(m2,Block ~ BAS_Rew_Int_z, at = list(BAS_Rew_Int_z = c(-3, 3)), 
      CIs=T,engine ="ggplot") + theme_bw() + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(axis.title.x = element_text(size=15))+
      theme(axis.title.y = element_text(size=15))+
      theme(axis.text.y = element_text(size=13))+
      scale_color_manual(values = viridis(3, option = 'D', end=.6, direction = -1),
                         (name="Block"), labels=c("1", "2", "3"))+
      theme(legend.text = element_text(size=10))+
      theme(legend.title = element_text(size = 10, face = "bold" ))+
      xlab("Interesse an Belohnung") + ylab("RT") +
      scale_x_discrete(labels=c("-3" = "-1SD", "3" = "+1SD"))

#Residuals ok?

sjPlot::plot_model(m2, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m2,m02, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Interesse a. Belohnung", 
                                  "Reaktivität a. Belohnung",
                                  "z. Beharrlichkeit","Impulsivität",
                                  "Block 2*Interesse a. Belohnung", 
                                  "Block 3*Interesse a. Belohnung",
                                  "Block 2*Reaktivität a. Belohnung", 
                                  "Block 3*Reaktivität a. Belohnung",
                                  "Block 2*z. Beharrlichkeit", "Block 3*z. Beharrlichkeit", 
                                  "Block 2*Impulsivität", "Block 3*Impulsivität"), 
                  dv.labels='Reaktionszeit')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#----Model with random intercept, by Block, PE, AC, SP (MAE-Subscales)

m3<-lmer(RT_log ~ 
           Block*PE_z +
           Block*AC_z +
           Block*SP_z + (1|VP), data=Data_reg, REML = F)
anova(m3)
summary(m3)

#Model without interaction
m3_1<-lmer(RT_log ~ 
           Block+PE_z +
           Block+AC_z +
           Block+SP_z + (1|VP), data=Data_reg, REML = F)
anova(m3_1)

#Model with inverse RT
m03<-lmer(-1000/RT_mean ~ 
            Block+PE_z +
            Block+AC_z +
            Block+SP_z + (1|VP), data=Data_reg, REML = F)
anova(m03)
sjPlot::plot_model(m03, "diag")

#Post-hoc

emmeans(m3_1, pairwise~Block, 
        adjust='Bonferroni')

#Residuals ok?

sjPlot::plot_model(m3, "diag")

#change order of levels 
Data_reg$Block <- factor(Data_reg$Block, levels = c('2', '3', '1'))
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#

#Get table

sjPlot::tab_model(m3, m3_1, show.aic = T, show.std = T, show.r2 = T, 
                  string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Glücklichkeit", "Leistung",
                                  "soziale Stärke",
                                  "Block 2*Glücklichkeit", "Block 3*Glücklichkeit",
                                  "Block 2*Leistung", "Block 3*Leistung",
                                  "Block 2*soziale Stärke", "Block 3*soziale Stärke"), 
                  dv.labels='Reaktionszeit')

sjPlot::tab_model(m3_1, m03, show.aic = T, show.std = T, show.r2 = T, 
                  string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","Block 2",
                                  "Block 3", "Glücklichkeit", "Leistung",
                                  "soziale Stärke"), 
                  dv.labels='Reaktionszeit')

#change order of levels back to normal
Data_reg$Block <- factor(Data_reg$Block, levels = c('1', '2', '3'))  
contrasts(Data_reg$Block) <- contr.sum(3); contrasts(Data_reg$Block)
#refit model#


#----RT after losses

#Model with intercept, by Card, Cond and Block

m1<-lmer(RT_log ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
summary(m1_1)
anova(m1)

#Model without intercations
m1_1<-lmer(RT_log ~  Cond+ Card+ Block+ 
           (1|VP), data = Data_loss_win, REML=F)
AIC(m1, m1_1)
anova(m1_1)

#Post-hoc

emmeans(m1, pairwise ~ Cond | Block, 
        adjust='Bonferroni')
emmeans(m1, pairwise ~ Block | Cond, 
        adjust='Bonferroni')
emmeans(m1_1, pairwise ~ Cond, 
        adjust='Bonferroni')

#Residuals ok?

sjPlot::plot_model(m1, "diag")

#change order of levels 
Data_loss_win$Block <- factor(Data_loss_win$Block, levels = c('2', '3', '1'))
contrasts(Data_loss_win$Block) <- contr.sum(3); contrasts(Data_loss_win$Block)
#refit model#

#Get table

sjPlot::tab_model(m1,m1_1, show.aic = T, show.std = T, show.r2 = T, 
                  string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","nach Gewinn",
                                  "Karte B", "Karte C", "Karte D",
                                  "Block 2", "Block 3",
                                  "nach Gewinn*Karte B", "nach Gewinn*Karte C", 
                                  "nach Gewinn*Karte D", "Karte B*Block 2", 
                                  "Karte C*Block 3","Karte D*Block 2", 
                                  "Karte B*Block 3", "Karte C*Block 3",
                                  "Karte D*Block 3","nach Gewinn*Block 2", 
                                  "nach Gewinn*Block 2"), 
                  dv.labels='Reaktionszeit')

sjPlot::tab_model(m1_1, show.aic = T, show.std = T, show.r2 = T, string.pred = 'Prädiktoren', 
                  pred.labels = c("(Intercept)","nach Gewinn",
                                  "Karte B", "Karte C", "Karte D",
                                  "Block 2", "Block 3"), 
                  dv.labels='Reaktionszeit')

#change order of levels back to normal
Data_loss_win$Block <- factor(Data_loss_win$Block, levels = c('1', '2', '3'))
contrasts(Data_loss_win$Block) <- contr.sum(3); contrasts(Data_loss_win$Block)
#refit model#


#Graphs

emmip(m1, Cond ~ Block, CIs=T,engine ="ggplot") + theme_bw() + 
  theme(axis.text.x = element_text(size = 13)) + 
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=13))+
  scale_color_manual(values = viridis(2, option = 'D', end=.6, direction = -1),
                     (name="Bedingung"), labels=c("nach Verlust", "nach Gewinn"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size = 10, face = "bold" ))+
  xlab("Block") +
  ylab("Reaktionszeit") 

emmip(m1_1, Cond ~ Block, CIs=T,engine ="ggplot") + theme_bw() + 
  theme(axis.text.x = element_text(size = 13)) + 
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=13))+
  scale_color_manual(values = viridis(2, option = 'D', end=.6, direction = -1),
                     (name="Bedingung"), labels=c("nach Verlust", "nach Gewinn"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size = 10, face = "bold" ))+
  xlab("Block") +
  ylab("Reaktionszeit")

#--------5) Tables (for basic layout, needs to be filled in by hand)--------------

apa.reg.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11.doc", table.number = 1)

apa.aov.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11a.doc", table.number = 1)
