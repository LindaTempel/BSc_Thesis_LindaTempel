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
          'apaTables')
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
  ylab("Chosen Deck(means)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  ggtitle("Chosen Decks") +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()
  
  
#ggplot(Data_sum2, aes(x = Card, y = M)) 
geom_bar(fill = "#0073C2FF", stat = "identity") 
geom_text(aes(label = means), vjust = -0.3)
  
  
#--------HLM RT by Block and VP----------

# Model 1
fm1 <- lmer(RT ~ Block + (1 | VP), data=Data_full, REML = F)
summary(fm1)
anova(fm1)

# Model 2
fm2 <- lmer(RT ~ Block + (1+ Block | VP), data=Data_full, REML = F)
summary(fm2)
anova(fm2)

# Model 3
fm3 <- lmer(RT ~ Block + (1 | VP/Block), data=Data_full, REML = F)
anova(fm3)
summary(fm3)

# Compare models to determine best fit
anova(fm1, fm2, fm3)

# plotting random effects
sjp.lmer(fm3, y.offset = .4)

sjp.lmer(fm3,
         facet.grid = F,
         sort.est = 'sort.all',
         y.offset = .4)

sjp.lmer(fm3, type = "ri.slope")

sjp.lmer(fm1, type = "rs.ri", vars = "Block", sample.n = 30)

sjp.lmer(fm3, 
         type = "rs.ri", 
         vars = "Block",
         facet.grid = FALSE)

#--------Correlations in Personality data----------------------


Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score)

matrix <- cor(Data_pers_score_all)
round(matrix,2)

matrix2<-rcorr(as.matrix(Data_pers_score_all))
matrix2

matrix2$r
matrix2$P

flattenCorrMatrix(matrix2$r, matrix2$P)
rcorr(matrix, type=c("pearson"), )
matrix$r


# Plot:  Insignificant correlation 
corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)

#Plot: full
corrplot(matrix2$r, method=c("number"), type="full", order="original", tl.col="black", tl.srt=45)



#--------Plot Personality Scores by Person------

mplot <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
BIS<-c(55,64,50,66,49,59,39,49,82,38,65,62,33,47,50,47,60,50,52,74,62,80,48,74,62,59,46,64,47,51)
BAS<-c(103,101,100,87,77,84,96,80,70,103,88,85,81,88,98,91,105,76,91,84,86,91,85,87,95,89,70,97,70,94)
FFFS<-c(25,29,34,33,12,24,17,21,32,21,17,18,11,14,21,21,27,15,18,22,14,25,21,30,23,12,20,24,12,22)
MAE <-c(60,55,65,40,32,55,54,43,12,69,44,40,67,59,74,47,73,48,38,40,50,46,43,37,52,53,47,55,31,56)

plot(mplot, BIS, type="o", col="blue", pch="o", lty=1, ylim = c(0,120),ylab="Personality_Score", xlab="Person")
points(mplot, BAS, col="red", pch="*")
lines(mplot, BAS, col="red", lty=2)
points(mplot, FFFS, col="dark red", pch="+")
lines(mplot, FFFS, col="dark red", lty=3)
points(mplot, MAE, col="green", pch="#")
lines(mplot, MAE, col="green", lty=4)
legend(1,125,legend=c("BIS","BAS","FFFS","MAE"), col=c("blue","red","dark red", "green"),
       pch=c("o","*","+", "#"),lty=c(1,2,3,4), ncol=4)


rm(BAS,BIS,FFFS,MAE)

#Standardize values

BAS_z <- scale (Data_pers_score$BAS_Score)
BIS_z <- scale (Data_pers_score$BIS)
FFFS_z <- scale (Data_pers_score$FFFS)
MAE_z <- scale (Data_pers_score$MAE_Score)


show_col(viridis_pal()(20))

plot(mplot, BIS_z, type="o", col="#482878FF", pch="o", lty=1,lwd=2, ylim = c(-5,5),ylab="Personality_Score", xlab="Person")
points(mplot, BAS_z, col="#2D718EFF", pch="o")
lines(mplot, BAS_z, col="#2D718EFF", lty=1, lwd=2)
points(mplot, FFFS_z, col="#29AF7FFF", pch="o")
lines(mplot, FFFS_z, col="#29AF7FFF", lty=1, lwd=2)
points(mplot, MAE_z, col="#B8DE29FF", pch="o")
lines(mplot, MAE_z, col="#B8DE29FF", lty=1, lwd=2)
legend(1,5,legend=c("BIS","BAS","FFFS","MAE"), col=c("#482878FF","#2D718EFF","#29AF7FFF", "#B8DE29FF"),
       pch=c("o","o","o", "o"),lty=c(1,1,1,1), ncol=4)

#--------Correlation matrix including IGT Score------

Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score, IGT_Score_all) 

matrix2<-rcorr(as.matrix(Data_pers_score_all))

corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)                 

#--------HLM-Card by Block and Card-----------------------------

mod_card <- lm(N ~ Card*Block, data=Data_reg_all)
car::Anova(mod_card,type=3)
emmeans(mod_card, pairwise ~ Card, adjust='Bonferroni')
   
     
mod_card <- lmer(N ~ Card*Block + (1+Card|VP), data=Data_reg_all, REML = F)
anova(mod_card)
AIC(mod_card)
emmeans(mod_card, pairwise ~ Card |Block, adjust='Bonferroni')

visreg(mod_card,'Card', by='Block')

mf<-allEffects((mod_card))
plot(mf)

plot.emm():
mod_card <- lmer(N ~ Card*Block + (1|VP/Card), data=Data_reg_all, REML= F)
AIC(mod_card)

emmip(mod_card, Block ~ Card)
emmip(mod_card, Block ~ Card | Block, CIs=T)

#--------Regression: IGT Score by Personality and Block -----

m1<-lm(IGT_Score ~ 
         Block*BAS_Score + 
         Block*BIS + 
         Block*FFFS + 
         Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1, type=3)


m1_1<-lm(IGT_Score ~ Block + BAS_Score + BIS + 
         + FFFS + MAE_Score,
       data=Data_reg)
summary(m1_1)
car::Anova(m1_1, type=3)
visreg(m1_1)


m2<-lm(IGT_Score ~ 
         Block*BAS_Rew_Int +
         Block*BAS_Rew_Reac +
         Block*BAS_Goal_Drive +
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2, type=3)

m2_1<-lm(IGT_Score ~ Block +
         BAS_Rew_Int +
         BAS_Rew_Reac +
         BAS_Goal_Drive +
         BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1, type=3)
visreg(m2_1)

m3<-lm(IGT_Score ~   
         Block*PE + 
         Block*AC + 
         Block*SP, 
         data=Data_reg)
summary(m3)
car::Anova(m3,type=3)

m3_1<-lm(IGT_Score ~ Block + PE + AC + SP, 
       data=Data_reg)
car::Anova(m3_1,type=3)
visreg(m3_1)

#Score by RT and by Block only
m4<-lm(IGT_Score~RT_mean, data=Data_reg)
summary(m4)
car::Anova(m4,type=3)

m5<-lm(IGT_Score~Block, data=Data_reg)
summary(m5)
car::Anova(m5,type=3)

m5<-lmer(IGT_Score~Block + (1|VP), data=Data_reg)
anova(m5)
emmeans(m5, pairwise ~ Block, adjust='fdr')
visreg(m5)

#--------Regression: Payoff by Personality----------------------

cor.test(Data_reg$Payoff, Data_reg$IGT_Score)

m1<-lm(Payoff~Block*BAS_Score+
         Block*BIS+
         Block*FFFS+
         Block*MAE_Score, data=Data_reg)
car::Anova(m1,type=3)

m1_1<-lm(Payoff~Block+BAS_Score+BIS+FFFS+MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)


m2<-lm(Payoff~Block*BAS_Rew_Int+ 
         Block*BAS_Rew_Reac+ 
         Block*BAS_Goal_Drive+
         Block*BAS_Impulsiv, data=Data_reg)
car::Anova(m2,type=3)

m2_1<-lm(Payoff~Block+BAS_Rew_Int+BAS_Rew_Reac+BAS_Goal_Drive+BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)


m3<-lm(Payoff~Block*PE+
         Block*AC+
         Block*SP, data=Data_reg)
car::Anova(m3,type=3)

m3_1<-lm(Payoff~Block+PE+AC+SP, data=Data_reg)
car::Anova(m3_1,type=3)


#--------Regression: RT_mean by Personality and Block----------

m1<-lm(RT_mean ~ 
          Block*BAS_Score +
          Block*BIS + 
          Block*FFFS +
          Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)

m1_1<-lm(RT_mean ~ 
         Block + BAS_Score +
         BIS + 
         FFFS +
         MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)

visreg(m1_1)

m2<-lm(RT_mean ~ 
         Block*BAS_Rew_Int + 
         Block*BAS_Rew_Reac + 
         Block*BAS_Goal_Drive + 
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_1<-lm(RT_mean ~ 
         Block + BAS_Rew_Int + 
         BAS_Rew_Reac + 
         BAS_Goal_Drive + 
         BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)
visreg(m2_1)
      
m3<-lm(RT_mean ~ 
         Block*PE +
         Block*AC +
         Block*SP, data=Data_reg)
summary(m3)
car::Anova(m3, type=3)

m3_1<-lm(RT_mean ~ 
         Block  +
         PE + AC + SP, data=Data_reg)
car::Anova(m3_1, type=3)
visreg(m3_1)

m4<-lmer(RT_mean~Block + (1|VP), data = Data_reg, REML=F)
summary(m4)
anova(m4)
emmeans(m4, pairwise ~ Block, adjust='Bonferroni')

visreg(m4)

#--------Regression: IGT-Score by RT and Personality--------------

m1<-lm(IGT_Score ~ 
         RT_mean*BAS_Score + 
         RT_mean*BIS + 
         RT_mean*FFFS + 
         RT_mean*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)


m1_1<-lm(IGT_Score ~ RT_mean + BAS_Score + BIS + 
           + FFFS + MAE_Score,
         data=Data_reg)
summary(m_1_1)
car::Anova(m1_1,type=3)


m2<-lm(IGT_Score ~ 
         RT_mean*BAS_Rew_Int +
         RT_mean*BAS_Rew_Reac +
         RT_mean*BAS_Goal_Drive +
         RT_mean*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_2<-lm(IGT_Score ~ RT_mean +
           BAS_Rew_Int +
           BAS_Rew_Reac +
           BAS_Goal_Drive +
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_2,type=3)


m3<-lm(IGT_Score ~   
         RT_mean*PE + 
         RT_mean*AC + 
         RT_mean*SP, 
       data=Data_reg)
summary(m3)
car::Anova(m3,type=3)

m3_1<-lm(IGT_Score ~ RT_mean + PE + AC + SP, 
         data=Data_reg)

car::Anova(m3_1,type=3)

#--------HLM: IGT-Score by Personality and Block-----

m1<-lmer(IGT_Score ~ 
         Block*BAS_Score + 
         Block*BIS + 
         Block*FFFS + 
         Block*MAE_Score
         + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1, xvar = 'MAE_Score', by='Block')
visreg(m1, xvar = 'BIS', by='Block')

m1_1<-lmer(IGT_Score ~ Block + BAS_Score + BIS + 
           + FFFS + MAE_Score + (1|VP),
         data=Data_reg)
anova(m1_1)


m2<-lmer(IGT_Score ~ 
         Block*BAS_Rew_Int +
         Block*BAS_Rew_Reac +
         Block*BAS_Goal_Drive +
         Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML=F)
anova(m2)

visreg(m2, xvar = 'BAS_Goal_Drive', by='Block')
visreg(m2, xvar = 'BAS_Rew_Int', by='Block')
visreg(m2)

m2_1<-lmer(IGT_Score ~ Block +
           BAS_Rew_Int +
           BAS_Rew_Reac +
           BAS_Goal_Drive +
           BAS_Impulsiv + (1|VP), data=Data_reg)
anova(m2_1)


m3<-lmer(IGT_Score ~   
         Block*PE + 
         Block*AC + 
         Block*SP+
         (1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')

m3_1<-lmer(IGT_Score ~ Block + PE + AC +  (1| VP), 
         data=Data_reg)
anova(m3_1)

#--------HLM: Payoff by Personality and Block-----

m1<-lmer(Payoff~Block*BAS_Score+
           Block*BIS+
           Block*FFFS+
           Block*MAE_Score+ (1|VP), data=Data_reg, REML = F)
anova(m1)
emmeans(m1, pairwise~Block|BIS, adjust='Bonferroni')
emmeans(m1, pairwise~Block|MAE_Score, adjust='Bonferroni')

visreg(m1, by='Block')
visreg(m1, xvar = 'MAE_Score', by='Block')
visreg(m1, xvar = 'BIS', by='Block')

m1_1<-lmer(Payoff~Block+BAS_Score+BIS+FFFS+MAE_Score
           +(1|VP), data=Data_reg)
anova(m1_1)

m2<-lmer(Payoff~Block*BAS_Rew_Int+ 
           Block*BAS_Rew_Reac+ 
           Block*BAS_Goal_Drive+
           Block*BAS_Impulsiv+ (1|VP), data=Data_reg, REML=F)
anova(m2)
visreg(m2)

m2_1<-lmer(Payoff~Block+BAS_Rew_Int+BAS_Rew_Reac+
            BAS_Goal_Drive+BAS_Impulsiv + 
            (1|VP), data=Data_reg)
anova(m2_1)

m3<-lmer(Payoff~Block*PE+
           Block*AC+
           Block*SP+(1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')


m3_1<-lmer(Payoff~Block+PE+AC+SP+
             (1|VP), data=Data_reg)
anova(m3_1)

#--------HLM: RT_mean by Personality and Block-------------------------------

m1<-lmer(RT_mean ~ 
         Block*BAS_Score +
         Block*BIS + 
         Block*FFFS +
         Block*MAE_Score + 
         (1|VP), data=Data_reg, REML=F)
anova(m1)

m1_1<-lmer(RT_mean ~ 
           Block + BAS_Score +
           BIS + 
           FFFS +
           MAE_Score + (1|VP), data=Data_reg)
anova(m1_1)

m2<-lmer(RT_mean ~ 
         Block*BAS_Rew_Int + 
         Block*BAS_Rew_Reac + 
         Block*BAS_Goal_Drive + 
         Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML=F)
anova(m2)

m2_1<-lmer(RT_mean ~ 
           Block + BAS_Rew_Int + 
           BAS_Rew_Reac + 
           BAS_Goal_Drive + 
           BAS_Impulsiv + (1|VP), data=Data_reg)
anova(m2_1)


m3<-lmer(RT_mean ~ 
         Block*PE +
         Block*AC +
         Block*SP + (1|VP), data=Data_reg, REML=F)
anova(m3)

m3_1<-lmer(RT_mean ~ 
           Block  +
           PE + AC + SP + (1|VP), data=Data_reg)
anova(m3_1)

#--------HLM with intercept and slopes for IGT-Score and RT----------------
m1<-lmer(IGT_Score ~ 
           Block*BAS_Score + 
           Block*BIS + 
           Block*FFFS + 
           Block*MAE_Score
         + (1+Card|VP), data=Data_reg_all)
anova(m1)


m1_1<-lmer(IGT_Score ~ Block + BAS_Score + BIS + 
             + FFFS + MAE_Score + (1+Card|VP),
           data=Data_reg_all)
anova(m1_1)



m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int +
           Block*BAS_Rew_Reac +
           Block*BAS_Goal_Drive +
           Block*BAS_Impulsiv + 
           (1+Card|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(IGT_Score ~ Block +
             BAS_Rew_Int +
             BAS_Rew_Reac +
             BAS_Goal_Drive +
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(IGT_Score ~   
           Block*PE + 
           Block*AC + 
           Block*SP+
           (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(IGT_Score ~ Block + PE + AC +  (1+Card| VP), 
           data=Data_reg_all)
anova(m3_1)



m1<-lmer(RT_mean ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + 
           (1+Card|VP), data=Data_reg_all)
anova(m1)

m1_1<-lmer(RT_mean ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1+Card|VP), data=Data_reg_all)
anova(m1_1)

m2<-lmer(RT_mean ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(RT_mean ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(RT_mean ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(RT_mean ~ 
             Block  +
             PE + AC + SP + (1+Card|VP), data=Data_reg_all)
anova(m3_1)

AIC(m1)

#--------RT after losses-------------------------

#RT mean
m1<-lmer(RT_mean ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
anova(m1)
car::qqPlot(resid(m1))

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmip(m1, ~ Cond | Block, CIs=T)
emmip(m1, ~ Card | Block, CIs=T)
emmip(m1, ~ Card| Cond, CIs=T)
emmip(m1, ~ Card | Block | Cond, CIs=T)

m1_1<-lmer(RT_mean ~  Cond  + Card +  Block+
             (1|VP), data = Data_loss_win, REML=F)
anova(m1_1)
car::qqPlot(resid(m1_1))

emmeans(m1_1, pairwise ~ Block | Cond, adjust='fdr')
emmeans(m1_1, pairwise ~ Card | Cond, adjust='fdr')
emmeans(m1_1, pairwise ~ Card | Block, adjust='fdr')
emmeans(m1_1, pairwise ~ Card, adjust='fdr')
emmeans(m1_1, pairwise ~ Block, adjust='fdr')
emmeans(m1_1, pairwise ~ Cond, adjust='fdr')
emmip(m1_1, ~ Cond | Block, CIs=T)
emmip(m1_1, ~ Card | Block, CIs=T)
emmip(m1_1, ~ Card| Cond, CIs=T)
emmip(m1_1, ~ Card | Block | Cond, CIs=T)

m1<-lmer(RT_mean ~ Cond*Block +(1|VP), data = Data_loss_win, REML=F)
anova(m1)
car::qqPlot(resid(m1))

emmeans(m1, pairwise ~ Block | Cond, adjust = 'fdr')
emmip(m1, ~ Block | Cond, CIs=T)


# HLM separate

Data_RT_loss <- Data_RT_loss %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

m1_1<-lmer(RT_mean~Block + (1 |VP), data=Data_RT_loss, REML=F)
anova(m1_1)

emmeans(m1_1, pairwise ~ Block, adjust="fdr")
emmip(m1_1, ~ Block, CIs=T)


Data_RT_win <- Data_RT_win %>% 
  dplyr::group_by(VP, Block, Card, Cond) %>% 
  dplyr::summarise(RT_mean=mean(RT))

m2_1<-lmer(RT_mean~Block + (1|VP), data=Data_RT_win, REML=F)
anova(m2_1)

emmeans(m2_1, pairwise ~ Block, adjust= "fdr")
emmip(m2_1, ~ Block, CIs=T)

#--------RT Analysis for log transformed Data-----

#Regression
m1<-lm(RT_log ~ 
         Block*BAS_Score +
         Block*BIS + 
         Block*FFFS +
         Block*MAE_Score, data=Data_reg)
summary(m1)
car::Anova(m1,type=3)

m1_1<-lm(RT_log ~ 
           Block + BAS_Score +
           BIS + 
           FFFS +
           MAE_Score, data=Data_reg)
car::Anova(m1_1,type=3)


m2<-lm(RT_log ~ 
         Block*BAS_Rew_Int + 
         Block*BAS_Rew_Reac + 
         Block*BAS_Goal_Drive + 
         Block*BAS_Impulsiv, data=Data_reg)
summary(m2)
car::Anova(m2,type=3)

m2_1<-lm(RT_log ~ 
           Block + BAS_Rew_Int + 
           BAS_Rew_Reac + 
           BAS_Goal_Drive + 
           BAS_Impulsiv, data=Data_reg)
car::Anova(m2_1,type=3)
visreg(m2_1)

m3<-lm(RT_log ~ 
         Block*PE +
         Block*AC +
         Block*SP, data=Data_reg)
summary(m3)
car::Anova(m3, type=3)

m3_1<-lm(RT_log ~ 
           Block  +
           PE + AC + SP, data=Data_reg)
car::Anova(m3_1, type=3)



m4<-lmer(RT_log~Block + (1|VP), data = Data_reg, REML = F)
summary(m4)
anova(m4)
emmeans(m4, pairwise ~ Block, adjust='Bonferroni')
visreg(m4)

#HLM with Intercept

m1<-lmer(RT_log ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1)

m1_1<-lmer(RT_log ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1|VP), data=Data_reg, REML = F)
anova(m1_1)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML = F)
anova(m2)

visreg(m2, xvar = 'BAS_Rew_Int', by='Block')

m2_1<-lmer(RT_log ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1|VP), data=Data_reg, REML = F)
anova(m2_1)


m3<-lmer(RT_log ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1|VP), data=Data_reg, REML = F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')

m3_1<-lmer(RT_log ~ 
             Block  +
             PE + AC + SP + (1|VP), data=Data_reg, REML=F)
anova(m3_1)

## HLM with Intercept and Slopes

m1<-lmer(RT_log ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + 
           (1+Card|VP), data=Data_reg_all)
anova(m1)

m1_1<-lmer(RT_log ~ 
             Block + BAS_Score +
             BIS + 
             FFFS +
             MAE_Score + (1+Card|VP), data=Data_reg_all)
anova(m1_1)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg_all)
anova(m2)

m2_1<-lmer(RT_log ~ 
             Block + BAS_Rew_Int + 
             BAS_Rew_Reac + 
             BAS_Goal_Drive + 
             BAS_Impulsiv + (1+Card|VP), data=Data_reg_all)
anova(m2_1)


m3<-lmer(RT_log ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1+Card|VP), data=Data_reg_all)
anova(m3)

m3_1<-lmer(RT_log ~ 
             Block  +
             PE + AC + SP + (1+Card|VP), data=Data_reg_all)
anova(m3_1)

## RT after losses

Data_loss_win$RT_log = log(Data_loss_win$RT_mean)


m1<-lmer(RT_log ~ Cond*Block 
         +(1|VP), data = Data_loss_win, REML=F)
anova(m1)

emmeans(m1, pairwise ~ Block | Cond, adjust = 'fdr')

m1<-lmer(RT_log ~  Cond * Card + Card * Block + Cond * Block+ 
           (1|VP), data = Data_loss_win, REML=F)
anova(m1)

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmip(m1, ~Card|Cond|Block, CIs=T)
summary(m1)

#--------Poweranalysis-------------------

#IGT Score by Block in Data_reg
pwr.f2.test(u = 2, v = NULL , f2 = 0.02 , sig.level = 0.05, power = .8)

etasq(m1, anova=TRUE, partial =T) 

#IGT Score by Personality and Block in Data_reg
pwr.f2.test(u = 2, v =75 , f2 = 0.018, sig.level = 0.05)


#--------Visualisierung-------------------------------
stargazer(m1, out="m1.htm")

stargazer(m2, m3, type="html", intercept.bottom = FALSE, single.row = T,
          dep.var.labels = c("IGT_Score"), 
          covariate.labels = c("A"), out= "m2u3.htm")

visreg(m5, "IGT_Score", by= "Block")

visreg(m3, "IGT_Score", by= "MAE_Score", main= "MAE", ylab="IGT-Score", xlab="Extraversion")

visreg(m3, "IGT_Score",by="MAE_Score", cond=list("Block"="1"), main="Block1")

visreg(m3)
visreg(m2)
visreg(m1)


#--------Regression tables------------------

apa.reg.table(m1, filename = "\\Table1.doc", table.number = 1)
apa.aov.table(mod_card, filename = "\\Table1.doc", table.number = 1)


#--------Partial Correlation----------------

cor(Data_reg$IGT_Score, Data_reg$BAS_Score)
cor.test(Data_reg$IGT_Score, Data_reg$BAS_Score)
pcor.test(Data_reg$IGT_Score,Data_reg$BAS_Score,Data_reg[c("RT_mean")])
          







###OLD CODE-----------------------------------------------------------------
#--------Anova RT by Block-----------------------------------------------

#---- M1: RT by Block
Data_full$Block <- as.factor(Data_full$Block)
m1<-lm(RT~Block,data=Data_full)                            
summary(m1)
anova(m1)

# Post-hoc Test
mult1<- glht(m1,mcp(Block="Tukey"))             
summary(mult1)

summarise(group_by(Data_card,Block),mean(RT))   
visreg(m1)
#--------Anova Frequency Cards by Block and Card--------

#M3: N by Block and Card
m3<-lm(N~Block*Card, data=Data_sum)
anova(m3)
plot(m3)

# plot effects for M3
m3f<-allEffects((m3))
plot(m3f)

#--------Regression Analysis -----------------

#Total score by personality/RT in large data frame
#merge into large data set
Data_full2 <- merge (Data_score_all, Data_full, by.x = 'VP', by.y = 'VP')

m1<-lm(IGT_Score_all~Block*BAS_Score*BIS*MAE_Score, data=Data_full2)
summary(m4)

m2<-lm(IGT_Score_all~RT, data=Data_full2)
summary(m5)

m3<-lm(IGT_Score_all~BAS_Score, data=Data_full2)
summary(m6)

# Total Score in small data frame
m1<-lm(IGT_Score~Block,data=Data_score)                            
summary(m1)
anova(m1)

m2<-lm(IGT_Score_all~MAE_Score, data=Data_pers_score_all)
summary(m2)

m3<-lm(IGT_Score_all~BAS_Score, data=Data_pers_score_all)
summary(m3)


#Block 3 Score in small data frame
m1<-lm(IGT_Score~BIS,data=Data_pers_score3)                            
summary(m1)

m2<-lm(IGT_Score~MAE_Score, data=Data_pers_score3)
summary(m2)

m3<-lm(IGT_Score~BAS_Score, data=Data_pers_score3)
summary(m3)

#Split by Blocks in final data frame
Data_reg1<-dplyr::filter(Data_reg, Block==1)
Data_reg2<-dplyr::filter(Data_reg, Block==2)
Data_reg3<-dplyr::filter(Data_reg, Block==3)

#Score by Personality in Block1
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg1)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg1)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg1)
summary(m3)
anova(m3)

#Score by Personality in Block2
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg2)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg2)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg2)
summary(m3)
anova(m3)

#Score by Personality in Block3
m1<-lm(IGT_Score~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg3)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg3)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~MAE_Score*PE*AC*SP, data=Data_reg3)
summary(m3)
anova(m3)