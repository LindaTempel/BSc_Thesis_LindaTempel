##### ##### #####     Analysis scripts for inferential statistics  ##### ##### #####
#                                    June 2018 

# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Bachelorarbeit\\Daten")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'multcomp', 'effects', 'phia', 'emmeans', 'lme4',
          'sjPlot', 'lmerTest', 'stargazer', 'lemon', 'gridExtra', 'ggplot2')
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
  

#stargazer(Data_sum2, type='html', out='table1.htm')

##Data_sum$score <- plyer::revalue(Data_sum$score= C+D - A+B)

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


####To do: same for subscales!



#---------Correlations in Personality data----------------------

rcorr(as.matrix(Data_pers_score)) 

