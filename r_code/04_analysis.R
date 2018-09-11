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
          'apaTables', 'scales')
getPacks(pkgs)
rm(pkgs)



#--------Merge behavioral and personality data---------
Data_full<- merge (Data_card, Data_pers_full, by.x='VP', by.y = 'VP')

Data_full$Block<-as.factor(Data_full$Block)


#--------Data frame for frequency of cards---------

Data_sum <- Data_full %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)


#--------Calculating IGT-Score, RT mean and Payoff-------------------------

#turning long into wide format
Data_score <- dcast(Data_sum, VP + Block ~ Card, value.var="N")

#Switching columns
Data_score <- Data_score[c("VP", "Block", "A", "B", "C", "D")]

#-------Calculating Score for each Block
Data_score[is.na(Data_score)] <- 0
Data_score<- dplyr::mutate(Data_score,IGT_Score=((C+D)-(A+B)))

#--------Select payoff at end of Block only
Data_behav3<-Data_behav %>% dplyr::select(VP, Block,Trial, Payoff)
Data_behav3<-dplyr::filter(Data_behav3, Trial==100)
Data_behav3<-Data_behav3 %>% dplyr::select(VP, Block,Payoff)

#merge with personality
Data_reg <- merge (Data_score, Data_behav3)
Data_reg <- merge (Data_reg, Data_pers_score, by.x = 'VP', by.y = 'VP')

#--------Calculating mean of RT for each Block
Data_RT <- Data_card %>% 
  dplyr::group_by(VP, Block) %>% 
  dplyr::summarise(RT_mean=mean(RT))

#merge into final data frame for regression
Data_reg <- merge (Data_reg, Data_RT)

#Log Transformation for RT
Data_reg$RT_log = log(Data_reg$RT_mean)


##-------Calculating total score
Data_score_all <- Data_score %>% 
  dplyr::group_by(VP) %>% 
  dplyr::summarise(IGT_Score_all=sum(IGT_Score))

#merge with personality scores
Data_pers_score_all <- merge (Data_score_all, Data_pers_score)  

#--------Alternative: bigger data set for Analysis with slopes------

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


#--------Center around zero: Personality variables----

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

#--------Data frame for RT after losses/wins---------

Data_RT_split<-dplyr::select(Data_full, VP, Block, RT, Card, net_payoff)

Data_RT_split<-dplyr::filter(Data_RT_split, Card %in% c('A', 'B', 'C', 'D'))
Data_RT_split$Card <- factor(Data_RT_split$Card)

Data_RT_loss <- dplyr::filter(Data_RT_split, lag(net_payoff, 1)<0)
Data_RT_win <- dplyr::filter(Data_RT_split, lag(net_payoff,1)>0)

Data_RT_loss$Cond<-c("loss")
Data_RT_win$Cond<-c("win")

Data_loss_win<-rbind(Data_RT_loss, Data_RT_win)

Data_loss_win$Cond <- as.factor(Data_loss_win$Cond)
Data_loss_win$VP <- as.factor(Data_loss_win$VP)

Data_loss_win <- Data_loss_win %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

#Personality Data

#--------Correlations in Personality data----------------------


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

colnames(matrix2$r) <- c("Extraversion", "Wohlbefinden", "Leistung", "Soziale Stärke", 
                         "FFFS", "BIS", "BAS", "Interesse a.B.", 
                         "Reaktivität a.B.", "z. Beharrlichkeit", "Impulsivität")
rownames(matrix2$r) <- c("Extraversion", "Wohlbefinden", "Leistung", "Soziale Stärke", 
                         "FFFS", "BIS", "BAS", "Interesse a.B", 
                         "Reaktivität a.B.", "z. Beharrlichkeit", "Impulsivität")

# Plot:  Insignificant correlation 
corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)

#Plot: full
corrplot(matrix2$r, method=c("number"), type="full", order="original", tl.col="black", tl.srt=45)



#Behavioral Data

#---------------Chosen Decks----------------

mod_card <- lmer(N ~ Card*Block + (1+Card|VP), data=Data_reg_all, REML = F)
summary(mod_card)
anova(mod_card)
emmeans(mod_card, pairwise ~ Card |Block, adjust='Bonferroni')

AIC(mod_card)

resid(mod_card)
car::qqPlot(resid(mod_card))


#---------------IGT-Score---------------------

m1<-lmer(IGT_Score ~ 
           Block*BAS_Score_z + 
           Block*BIS_z + 
           Block*FFFS_z + 
           Block*MAE_Score_z
         + (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1)

sjPlot::tab_model(m1)

sd(Data_reg$MAE_Score_z)
sd(Data_reg$BIS_z)

emmeans(m1, pairwise~Block|MAE_Score_z, at=list(MAE_Score_z = c(-13, 13)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block|BIS_z, at=list(BIS_z=c(-12, 12)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block, adjust='Bonferroni')


visreg(m1, xvar = 'MAE_Score_z', by='Block', line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ 
  xlab("Extraversion")+ ylab("IGT-Score")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold"))

visreg(m1, xvar = 'BIS_z', by='Block', line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ 
  xlab("BIS")+ ylab("IGT-Score")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold"))      

resid(m1)
car::qqPlot(resid(m1))


m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int_z +
           Block*BAS_Rew_Reac_z +
           Block*BAS_Goal_Drive_z +
           Block*BAS_Impulsiv_z + (1|VP), data=Data_reg, REML=F)
anova(m2)
summary(m2)

sd(Data_reg$BAS_Goal_Drive_z)
sd(Data_reg$BAS_Rew_Int_z)

emmeans(m2, pairwise~Block|BAS_Goal_Drive_z,at=list(BAS_Goal_Drive_z= c(-3,3)), 
        adjust='Bonferroni')
emmeans(m2, pairwise~Block|BAS_Rew_Int_z, at=list(BAS_Rew_Int_z = c(-3, 3)),
        adjust='Bonferroni')
emmeans(m2, pairwise~Block, adjust='Bonferroni')
emmeans(m2, pairwise~BAS_Rew_Int_z, adjust='Bonferroni')

emtrends(m2, var='BAS_Goal_Drive_z', ~1, 
         at=list(BAS_Rew_Int_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')
emtrends(m2, var='BAS_Rew_Int_z', ~1, 
         at=list(BAS_Goal_Drive_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')


visreg(m2, xvar = 'BAS_Goal_Drive_z', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+
  xlab("Zielgerichtete Beharrlichkeit")+ ylab("IGT-Score")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold")) 

visreg(m2, xvar = 'BAS_Rew_Int_z', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+
  xlab("Interesse an Belohnung")+ ylab("IGT-Score")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold")) 

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


resid(m2)
car::qqPlot(resid(m2))

m3<-lmer(IGT_Score ~   
           Block*PE_z + 
           Block*AC_z + 
           Block*SP_z+
           (1|VP), data=Data_reg, REML=F)
anova(m3)
summary(m3)

sd(Data_reg$SP_z)

emmeans(m3, pairwise~Block|SP_z,at=list(SP_z=c(-7,7)),
        adjust='Bonferroni')
emmeans(m3, pairwise~Block, adjust='Bonferroni')

visreg(m3, xvar = 'SP_z', by='Block',line=list(col='#2D718EFF'),
       gg=T) + theme_bw()+ 
  xlab("Soziale Stärke")+ ylab("IGT-Score")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold")) 

resid(m3)
car::qqPlot(resid(m3))

#---------------Payofff------------------

cor.test(Data_reg$IGT_Score, Data_reg$Payoff)

m1<-lmer(Payoff~Block*BAS_Score_z+
           Block*BIS_z+
           Block*FFFS_z+
           Block*MAE_Score_z+ (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1)

sd(Data_reg$MAE_Score_z)
sd(Data_reg$BIS_z)

emmeans(m1, pairwise~Block|BIS_z,at=list(BIS_z=c(-12,12)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block|MAE_Score_z,at=list(MAE_Score_z=c(-13,13)),
        adjust='Bonferroni')
emmeans(m1, pairwise~Block, adjust='Bonferroni')

visreg(m1, xvar = 'MAE_Score_z', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+
  xlab("Extraversion")+ ylab("Punktestand")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold"))

visreg(m1, xvar = 'BIS_z', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+ 
  xlab("BIS")+ ylab("Punktestand")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold"))

resid(m1)
car::qqPlot(resid(m1))

m2<-lmer(Payoff~Block*BAS_Rew_Int_z+ 
           Block*BAS_Rew_Reac_z+ 
           Block*BAS_Goal_Drive_z+
           Block*BAS_Impulsiv_z+ (1|VP), data=Data_reg, REML=F)
anova(m2)
summary(m2)

emmeans(m2, pairwise~Block, adjust='Bonferroni')
emtrends(m2, var='BAS_Goal_Drive_z', ~1, 
         at=list(BAS_Rew_Int_z=0, BAS_Rew_Reac_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')
emtrends(m2, var='BAS_Rew_Reac_z', ~1, 
         at=list(BAS_Goal_Drive_z=0, BAS_Rew_Int_z=0, BAS_Impulsiv_z=0), 
         adjust='Bonferroni')

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

resid(m2)
car::qqPlot(resid(m2))


m3<-lmer(Payoff~Block*PE_z+
           Block*AC_z+
           Block*SP_z+(1|VP), data=Data_reg, REML=F)
anova(m3)
summary(m3)

sd(Data_reg$SP_z)

emmeans(m3, pairwise~Block|SP_z, at=list(Sp_z=c(-7, 7)),
        adjust='Bonferroni')
emmeans(m3, pairwise~Block, adjust='Bonferroni')

visreg(m3, xvar = 'SP_z', by='Block',line=list(col='#20A386FF'),
       gg=T) + theme_bw()+ 
  xlab("Soziale Stärke")+ ylab("Punktestand")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold")) 


resid(m3)
car::qqPlot(resid(m3))

#---------------RT_log-----------------

m1<-lmer(RT_log ~ 
           Block*BAS_Score_z +
           Block*BIS_z + 
           Block*FFFS_z +
           Block*MAE_Score_z + (1|VP), data=Data_reg, REML = F)
anova(m1)
summary(m1)

emmeans(m1, pairwise~Block, adjust='Bonferroni')
emtrends(m1, var='BAS_Score_z', ~1, 
         at=list(BIS_z=0, FFFS_z=0, MAE_Score_z=0), 
         adjust='Bonferroni')
emtrends(m1, var='FFFS_z', ~1, 
         at=list(BAS_Score_z=0, BIS_z=0, MAE_Score_z=0), 
         adjust='Bonferroni')

visreg(m1, xvar='Block', line=list(col='#481568FF'),
       xlab=("Block"), ylab=("Reaktionszeit"), gg=T)+ theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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


resid(m1)
car::qqPlot(resid(m1))


m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int_z + 
           Block*BAS_Rew_Reac_z + 
           Block*BAS_Goal_Drive_z + 
           Block*BAS_Impulsiv_z + (1|VP), data=Data_reg, REML = F)
anova(m2)
summary(m2)

sd(Data_reg$BAS_Rew_Int_z)

emmeans(m2, pairwise~Block|BAS_Rew_Int_z, at=list(BAS_Rew_Int_z=c(-3,3)),
        adjust='Bonferroni')
emmeans(m2, pairwise~Block, adjust='Bonferroni')

visreg(m2, xvar = 'BAS_Rew_Int_z', by='Block',line=list(col='#481568FF'),
       gg=T) + theme_bw()+ 
  xlab("Interesse an Belohnung")+ ylab("Reaktionszeit")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15))+
  theme(strip.text = element_text(size = 12, face="bold"))

resid(m2)
car::qqPlot(resid(m2))


m3<-lmer(RT_log ~ 
           Block*PE_z +
           Block*AC_z +
           Block*SP_z + (1|VP), data=Data_reg, REML = F)
anova(m3)
summary(m3)

emmeans(m3, pairwise~Block, adjust='Bonferroni')

resid(m3)
car::qqPlot(resid(m3))


m4<-lmer(RT_log~Block + (1|VP), data = Data_reg, REML = F)
anova(m4)
summary(m4)

emmeans(m4, pairwise ~ Block, adjust='Bonferroni')

resid(m4)
car::qqPlot(resid(m4))

# RT after losses

Data_loss_win$RT_log = log(Data_loss_win$RT_mean)

m1<-lmer(RT_log ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
summary(m1)
anova(m1)

resid(m1)
car::qqPlot(resid(m1))

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmeans(m1, pairwise ~ Cond | Block, adjust='Bonferroni')

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

emmip(m1, Cond~Block|Card, CIs=T)+ theme_bw()+
  theme(axis.text.x = element_text(size = 13)) + 
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=13))+
  scale_color_manual(values = viridis(2, option = 'D', end=.6, direction = -1),
                     (name="Bedingung"), labels=c("nach Verlust", "nach Gewinn"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size = 10, face = "bold" ))+
  theme(strip.text = element_text(size = 10, face = "bold"))
xlab("Block") +
  ylab("Reaktionszeit")


emmip(m1, Cond~Card|Block, CIs=T)+ theme_bw()
emmip(m1, Card~Cond|Block, CIs=T)+ theme_bw()
emmip(m1, Block~Cond|Card, CIs=T)+ theme_bw()
emmip(m1, Card~Block|Cond, CIs=T)+ theme_bw()
emmip(m1, Block~Card|Cond, CIs=T)+ theme_bw()


#Tables (for basic layout, needs to be filled in by hand)

apa.reg.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11.doc", table.number = 1)

apa.aov.table(m1_1, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten\\Table11a.doc", table.number = 1)
