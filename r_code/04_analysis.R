## Anova 
Data_full$Block <- as.factor(Data_full$Block)
m1<-lm(RT~Block,data=Data_full)                            ## Modell 1 festelegen

summary(m1)
anova(m1)

mult1<- glht(m1,mcp(Block="Tukey"))             ## Post-hoc-Test: Tukey Test
summary(mult1)

summarise(group_by(Data_full,Block),mean(RT))              ## Mittelwert RT aufgeteilt nach Block


m2<-lm(RT~Block*VP,data=Data_full)              ## zweifaktorielle ANOVA     
summary(m2)
anova(m2)

Data_full$VP <- as.factor(Data_full$VP)
m2f<-allEffects(m2)
plot(m2f)

visreg(m2,"VP", by="Block" )



Data_sum <- Data_full %>% 
  dplyr::group_by(VP, Block, Card) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)

Data_sum %>% dplyr::group_by(Block, Card) %>% dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(N)))

m3<-lm(N~Block*Card, data=Data_sum)
anova(m3)
plot(m3)

m3f<-allEffects((m3))
plot(m3f)
