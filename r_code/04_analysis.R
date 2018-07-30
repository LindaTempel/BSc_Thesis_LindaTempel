##### ##### #####     Analysis scripts for inferential statistics  ##### ##### #####
#                                    June 2018 

# Load helper functions
setwd("")
source('./r_functions/getPacks.R') # <- path to getPacks function
source('./r_functions/flattenCorrMatrix.R')
# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'multcomp', 'effects', 'phia', 'emmeans', 'lme4',
          'sjPlot', 'lmerTest', 'stargazer', 'lemon', 'gridExtra', 'ggplot2', 'tidyr',
          'reshape2', 'corrplot')
getPacks(pkgs)
rm(pkgs)



#---------Merge behavioral and personality data---------
Data_full<- merge (Data_card, Data_pers_full, by.x='VP', by.y = 'VP')

Data_full$Block<-as.factor(Data_full$Block)

#---------new data frame for frequency of cards---------

Data_sum <- Data_full %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)

Graph <- Data_sum %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))%>%
  dplyr::filter(!Card==0)

Graph$Card<-factor(Graph$Card,levels(Graph$Card)[c(1,4,3,2)])



# Bar plot
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
  scale_fill_manual(name="Card",
                 values=c("goldenrod1", "brown1", "dodgerblue", "forestgreen")) +
  ggtitle("Chosen Decks") +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()




Data_sum2 <-Data_sum %>% dplyr::group_by(Block, Card) %>% dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(N)))





pdf("data_sum2.pdf", height=11, width=8.5)
grid.table(Data_sum2)
dev.off()



#ggplot(Data_sum2, aes(x = Card, y = M)) 
  geom_bar(fill = "#0073C2FF", stat = "identity") 
  geom_text(aes(label = means), vjust = -0.3)
  



#--------Analysis: Anova RT by Block-----------------------------------------------

#---- M1: RT by Block
Data_full$Block <- as.factor(Data_full$Block)
m1<-lm(RT~Block,data=Data_full)                            
summary(m1)
anova(m1)

# Post-hoc Test
mult1<- glht(m1,mcp(Block="Tukey"))             
summary(mult1)

summarise(group_by(Data_card,Block),mean(RT))              

# -------Analysis: Anova RT by Block and VP-------------------------

# M2: RT by Block and VP
m2<-lm(RT~Block*VP,data=Data_full)                   
summary(m2)
anova(m2)

# simple effects analysis for interaction Block and VP
testInteractions(m2)
emmeans(m2, pairwise ~ VP)


# plot effects for M2
Data_full$VP <- as.factor(Data_full$VP)
m2f<-allEffects(m2)
plot(m2f)

# -------Analysis: Anova Frequency Cards by Block and Card--------

#M3: N by Block and Card
m3<-lm(N~Block*Card, data=Data_sum)
anova(m3)
plot(m3)

# plot effects for M3
m3f<-allEffects((m3))
plot(m3f)

# -------Analysis: HLM with Block and VP----------

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

#------Analysis: Personality for RT-------------------

#MAE total Score
m4<-lm(RT~MAE_Score, data=Data_full)
anova(m4)
summary(m4)
 
#BIS
m5<-lm(RT~BIS, data=Data_full)
anova(m5)
summary(m5)

#BAS
m6<-lm(RT~BAS_Score, data=Data_full)
anova(m6)
summary(m6)

#FFS
m7<-lm(RT~FFFS, data=Data_full)
anova(m7)
summary(m7)

####To do: same for subscales



#Diagramm: each score on separate line 

#-------Analysis: Personality and Block for RT------

#MAE total Score
m4.2<-lm(RT~MAE_Score*Block, data=Data_full)
anova(m4.2)
summary(m4.2)

#BIS
m5.2<-lm(RT~BIS*Block, data=Data_full)
anova(m5.2)
summary(m5.2)

#BAS
m6.2<-lm(RT~BAS_Score*Block, data=Data_full)
anova(m6.2)
summary(m6.2)

#FFS
m7.2<-lm(RT~FFFS*Block, data=Data_full)
anova(m7.2)
summary(m7.2)

m8<-lm(RT~MAE_Score+BIS+BAS_Score+FFFS, data=Data_full)
summary(m8)


####To do: same for subscales!



#---------Correlations in Personality data: all----------------------


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

?corrplot

#---------Correlations in Personality data: without subscales----------------------
##problem: more than 4 Variables needed

Data_pers_score_main<- Data_pers_score %>% dplyr::select(MAE_Score, FFFS, BIS, BAS_Score)

Data_pers_score_main$VP<- as.numeric(Data_pers_score_main$VP)
matrix <- cor(Data_pers_score_main)
round(matrix,2)

matrix2<-rcorr(as.matrix(Data_pers_score_main))
matrix2

matrix2$r
matrix2$P

flattenCorrMatrix(matrix2$r, matrix2$P)
rcorr(matrix, type=c("pearson"))
matrix$r

# Plot:  Insignificant correlation 
corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)

#Plot: full
corrplot(matrix2$r, method=c("number"), type="full", order="original", tl.col="black", tl.srt=45)




#-----Plotting by Person

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


#-------------Calculating IGT-Score-------------------------


#turning long into wide format
Data_score <- dcast(Data_sum, VP + Block ~ Card, value.var="N")

#Switching columns
Data_score <- Data_score[c("VP", "Block", "A", "B", "C", "D")]

#-------Calculating Score for each Block
Data_score[is.na(Data_score)] <- 0
Data_score<- dplyr::mutate(Data_score,IGT_Score=((C+D)-(A+B)))

#-------Calculating total score
Data_score_all <- Data_score %>% 
  dplyr::group_by(VP) %>% 
  dplyr::summarise(IGT_Score_all=sum(IGT_Score))

#merge with personality scores
Data_pers_score <- merge (Data_score_all, Data_pers_score, by.x = 'VP', by.y = 'VP')

#-------Calculating score for Block 3
Data_score_3<-dplyr::filter(Data_score, Block==3)
Data_score_3<-dplyr::select(Data_score_3, VP,IGT_Score)

#merge with personality scores
Data_pers_score3 <- merge (Data_score_3, Data_pers_score, by.x = 'VP', by.y = 'VP')


#--------Select payoff at end of Block only
Data_behav3<-Data_behav %>% dplyr::select(VP, Block,Trial, Payoff)
Data_behav3<-dplyr::filter(Data_behav3, Trial==100)
Data_behav3<-Data_behav3 %>% dplyr::select(VP, Block,Payoff)

#merge with personality
Data_reg <- merge (Data_score, Data_behav3)
Data_reg <- merge (Data_reg, Data_pers_score, by.x = 'VP', , by.y = 'VP')

#--------Calculating mean of RT for each Block
Data_RT <- Data_behav %>% 
  dplyr::group_by(VP, Block) %>% 
  dplyr::summarise(RT_sum=sum(RT)/100)

#merge into final data frame for regression
Data_reg <- merge (Data_reg, Data_RT)




#-----------Regression Analysis-----------------

#Total score by personality/RT in large data frame
#merge into large data set
Data_full2 <- merge (Data_score_all, Data_full, by.x = 'VP', by.y = 'VP')

m1<-lm(IGT_Score_all~Block*BAS_Score*BIS*MAE_Score, data=Data_full2)
summary(m4)

m2<-lm(IGT_Score_all~RT, data=Data_full2)
summary(m5)

m3<-lm(IGT_Score_all~BAS_Score, data=Data_full2)
summary(m6)

#Score in small data frame
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

#Score by Personality and Block in final Data frame
m1<-lm(IGT_Score~Block*BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg)
summary(m1)
anova(m1)

m2<-lm(IGT_Score~Block*BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg)
summary(m2)
anova(m2)

m3<-lm(IGT_Score~Block*MAE_Score*PE*AC*SP, data=Data_reg)
summary(m3)
anova(m3)

#Split by Blocks
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

#Payoff by Personality in Block3
m1<-lm(Payoff~BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg3)
summary(m1)
anova(m1)
      
m2<-lm(Payoff~BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg3)
summary(m2)
anova(m2)
      
m3<-lm(Payoff~MAE_Score*PE*AC*SP, data=Data_reg3)
summary(m3)
anova(m3)

#Score by RT 
m4<-lm(IGT_Score~RT_sum, data=Data_reg)
summary(m4)
anova(m4)

m5<-lm(IGT_Score~Block, data=Data_reg)
summary(m5)
anova(m5)

#poweranalysis
etasq(m1, anova=TRUE, partial =T) 

#RT by Personality and Block in final Data frame
m1<-lm(RT_sum~Block*BAS_Score*BIS*FFFS*MAE_Score, data=Data_reg)
summary(m1)
anova(m1)

m2<-lm(RT_sum~Block*BAS_Rew_Int*BAS_Rew_Reac*BAS_Goal_Drive*BAS_Impulsiv, data=Data_reg)
summary(m2)
anova(m2)

m3<-lm(RT_sum~Block*MAE_Score*PE*AC*SP, data=Data_reg)
summary(m3)
anova(m3)

m4<-lm(RT_sum~Block, data = Data_reg)
summary(m4)
anova(m4)

#-------------Correlation matrix including IGT Score------
Data_pers_score_all<- Data_pers_score %>% dplyr::select(PE, AC, SP, MAE_Score, FFFS, BIS, BAS_Rew_Int, BAS_Rew_Reac, BAS_Goal_Drive, BAS_Impulsiv, BAS_Score, IGT_Score_all) 

matrix2<-rcorr(as.matrix(Data_pers_score_all))

corrplot(matrix2$r, method=c("number"), type="full", order="original", 
         p.mat = matrix2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)                 


#----------------RT after losses-------------------------

Data_RT_loss<-dplyr::select(Data_full, VP, Block, RT, net_payoff)
Data_RT_loss<-dplyr::filter(Data_RT_loss, net_payoff<0)
Data_RT_loss<-dplyr::lag(Data_RT_loss$net_payoff<0)
Data_RT_win<-dplyr::filter(Data_RT_loss,net_payoff>0)

?lag
