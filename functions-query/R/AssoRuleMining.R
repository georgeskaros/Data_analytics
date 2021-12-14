#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
library(class)
#creating a new matrix for the process 
parkinsons_ARM <- parkinsons


#making sex column as a logical one 
parkinsons_ARM$sex <- as.logical(parkinsons_ARM$sex)

#making numeric columns as factorw using the cut function ,cuting them into bins 
#I asumed the number of bins for each one by 
parkinsons_ARM$age <-as.factor(cut(parkinsons_ARM$age, 9 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9")))
parkinsons_ARM$test_time <-as.factor(cut(parkinsons_ARM$test_time, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$motor_UPDRS <-as.factor(cut(parkinsons_ARM$motor_UPDRS, 20 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")))
parkinsons_ARM$total_UPDRS <-as.factor(cut(parkinsons_ARM$total_UPDRS, 20 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")))
parkinsons_ARM$Jitter... <-as.factor(cut(parkinsons_ARM$Jitter..., 5 , include.lowest = TRUE, labels=c("1","2","3","4","5")))
parkinsons_ARM$Jitter.Abs. <-as.factor(cut(parkinsons_ARM$Jitter.Abs., 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Jitter.RAP <-as.factor(cut(parkinsons_ARM$Jitter.RAP, 5 , include.lowest = TRUE, labels=c("1","2","3","4","5")))
parkinsons_ARM$Jitter.PPQ5 <-as.factor(cut(parkinsons_ARM$Jitter.PPQ5, 5 , include.lowest = TRUE, labels=c("1","2","3","4","5")))
parkinsons_ARM$Jitter.DDP <-as.factor(cut(parkinsons_ARM$Jitter.DDP, 5 , include.lowest = TRUE, labels=c("1","2","3","4","5")))
parkinsons_ARM$Shimmer <-as.factor(cut(parkinsons_ARM$Shimmer, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Shimmer.dB. <-as.factor(cut(parkinsons_ARM$Shimmer.dB., 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Shimmer.APQ3 <-as.factor(cut(parkinsons_ARM$Shimmer.APQ3, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Shimmer.APQ5 <-as.factor(cut(parkinsons_ARM$Shimmer.APQ5, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Shimmer.APQ11 <-as.factor(cut(parkinsons_ARM$Shimmer.APQ11, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$Shimmer.DDA <-as.factor(cut(parkinsons_ARM$Shimmer.DDA, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$NHR <-as.factor(cut(parkinsons_ARM$NHR, 5 , include.lowest = TRUE, labels=c("1","2","3","4","5")))
parkinsons_ARM$HNR <-as.factor(cut(parkinsons_ARM$HNR, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$RPDE <-as.factor(cut(parkinsons_ARM$RPDE, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$DFA <-as.factor(cut(parkinsons_ARM$DFA, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))
parkinsons_ARM$PPE <-as.factor(cut(parkinsons_ARM$PPE, 10 , include.lowest = TRUE, labels=c("1","2","3","4","5","6","7","8","9","10")))

#the new matrix
summary(parkinsons_ARM)

#making it as a data frame
parkinsons_ARM <- data.frame(sapply(parkinsons_ARM, as.factor))

#maing a transactions matrix out of the data set
trans <- as(parkinsons_ARM, "transactions")

#an item frecuency histogram from the top 50 
itemFrequencyPlot(trans, topN=50,  cex.names=.5)


#assiciation rule making 
#decited to choose these parameters in order to be fast and acurate , changed the max so it can generate all columns variations
parkinsons_rules <- apriori(trans ,parameter = list(support = 0.05, confidence = 0.8, maxlen = 20, maxtime = 20))


inspect(head(sort(parkinson_rules, by = "lift"), 150))
########################



