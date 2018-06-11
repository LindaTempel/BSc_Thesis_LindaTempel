###Descriptives 

## Mittelwert und Histogramm 
mean(Data_full$RT)

hist(Data_full$RT)

mean(Data_full$VPayoff)

hist(Data_full$Payoff)

hist(Data_full$RT, xlab = "RT", ylab = "Häufigkeit", main = "Verteilung RT") ##Bezeichnungen der Achsen ändern

hist(Data_full$Payoff, xlab = "payoff", ylab = "Häufigkeit", main = "Verteilung payoff")

boxplot(Data_full$Payoff)      ##Boxplot ausgeben

table(Data_full$VP)       ##Häufigkeiten