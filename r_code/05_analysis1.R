##ANALYSIS 1

#Wahl der Decks

mod_card <- lmer(N ~ Card*Block + (1+Card|VP), data=Data_reg_all, REML = F)
anova(mod_card)
AIC(mod_card)
emmeans(mod_card, pairwise ~ Card |Block, adjust='Bonferroni')

#IGT-Score

m1<-lmer(IGT_Score ~ 
           Block*BAS_Score + 
           Block*BIS + 
           Block*FFFS + 
           Block*MAE_Score
         + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1, xvar = 'MAE_Score', by='Block')
visreg(m1, xvar = 'BIS', by='Block')

m2<-lmer(IGT_Score ~ 
           Block*BAS_Rew_Int +
           Block*BAS_Rew_Reac +
           Block*BAS_Goal_Drive +
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML=F)
anova(m2)

visreg(m2, xvar = 'BAS_Goal_Drive', by='Block')
visreg(m2, xvar = 'BAS_Rew_Int', by='Block')
visreg(m2)

m3<-lmer(IGT_Score ~   
           Block*PE + 
           Block*AC + 
           Block*SP+
           (1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')


#Payoff

m1<-lmer(Payoff~Block*BAS_Score+
           Block*BIS+
           Block*FFFS+
           Block*MAE_Score+ (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1, xvar = 'MAE_Score', by='Block')
visreg(m1, xvar = 'BIS', by='Block')

m2<-lmer(Payoff~Block*BAS_Rew_Int+ 
           Block*BAS_Rew_Reac+ 
           Block*BAS_Goal_Drive+
           Block*BAS_Impulsiv+ (1|VP), data=Data_reg, REML=F)
anova(m2)

visreg(m2)

m3<-lmer(Payoff~Block*PE+
           Block*AC+
           Block*SP+(1|VP), data=Data_reg, REML=F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')


#RT

m4<-lmer(RT_log~Block + (1|VP), data = Data_reg, REML = F)
anova(m4)
emmeans(m4, pairwise ~ Block, adjust='Bonferroni')

visreg(m4)

m1<-lmer(RT_log ~ 
           Block*BAS_Score +
           Block*BIS + 
           Block*FFFS +
           Block*MAE_Score + (1|VP), data=Data_reg, REML = F)
anova(m1)

visreg(m1)

m2<-lmer(RT_log ~ 
           Block*BAS_Rew_Int + 
           Block*BAS_Rew_Reac + 
           Block*BAS_Goal_Drive + 
           Block*BAS_Impulsiv + (1|VP), data=Data_reg, REML = F)
anova(m2)

visreg(m2, xvar = 'BAS_Rew_Int', by='Block')


m3<-lmer(RT_log ~ 
           Block*PE +
           Block*AC +
           Block*SP + (1|VP), data=Data_reg, REML = F)
anova(m3)

visreg(m3, xvar = 'SP', by='Block')

m1<-lmer(RT_log ~  Cond * Card + Card * Block + Cond * Block+ (1|VP), data = Data_loss_win, REML=F)
anova(m1)

emmeans(m1, pairwise ~ Block | Cond, adjust='Bonferroni')
emmip(m1, ~ Cond | Block, CIs=T
emmip(m1, ~ Card | Block | Cond, CIs=T)
