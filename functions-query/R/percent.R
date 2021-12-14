#making the % of the data set values for 20-40-60-80
ind1 <- sample(2, nrow(parkinsons), replace = T ,prob = c(0.8, 0.2))
parkinsons80 <- parkinsons[ind1 == 1,] 
parkinsons20 <- parkinsons[ind1 == 2,] 

ind2 <- sample(2, nrow(parkinsons), replace = T ,prob = c(0.6, 0.4))
parkinsons60 <- parkinsons[ind2 == 1,] 
parkinsons40 <- parkinsons[ind2 == 2,] 

trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)
###########20%
##lm
model_motor <- lm(motor_UPDRS ~ age+sex+test_time+total_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson20)

model_total <- lm(total_UPDRS ~ age+sex+test_time+motor_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson20)

predicted20_M <- predict(model_motor,test)
predicted20_T<- predict(model_total,test)

lm_20_T <-RMSE(test$total_UPDRS, predicted20_T)
lm_20_M <-RMSE(test$motor_UPDRS, predicted20_M)
##knn
fit_20M <- train(motor_UPDRS ~.,
               data = parkinson20,
               tuneGrid = expand.grid(k=1:40),
               method = 'knn',
               metric = 'RMSE',
               trControl = trControl)
predicting_20 <- predict(fit_20M,test)
knnM20 <-accuracy(predicting_20,test$motor_UPDRS)

fit_20T <- train(total_UPDRS ~.,
               data = parkinson20,
               tuneGrid = expand.grid(k=1:40),
               method = 'knn',
               metric = 'RMSE',
               trControl = trControl)
predicting_20 <- predict(fit_20T,test)


knnT20 <-accuracy(predicting_20,test$total_UPDRS)
##hier
parkinsons_hier_T_20 <- parkinsons20[,-5]
parkinsons_hier_score_T_20 <- parkinsons20[,5]
parkinsons_hier_M_20 <- parkinsons20[,-4]
parkinsons_hier_score_M_20 <- parkinsons20[,4]

d_T_20 <- dist(parkinsons_hier_T_20, method = "euclidean")
d_M_20 <- dist(parkinsons_hier_M_20, method = "euclidean")


H_fit_T_20 <- hclust(d_T_20, method = "ward.D")
H_fit_M_20 <- hclust(d_M_20, method = "ward.D")

plot(H_fit_T_20)
plot(H_fit_M_20)
rect.hclust(H_fit_T_20, k=3, border="red")
rect.hclust(H_fit_M_20, k=3, border="red")

groups_T_20 <- cutree(H_fit_T_20, k=3)
groups_M_20 <- cutree(H_fit_M_20, k=3)

parkinsons_hier_score_M_20 <-cut(parkinsons20$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_hier_score_T_20 <-cut(parkinsons20$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))

scoreTableM20 <- table(parkinsons_hier_score_M_20, groups_M_20)/nrow(parkinson20)
scoreTableM20 <- table(parkinsons_hier_score_T_20, groups_T_20)/nrow(parkinson20)

scoreTableM20
scoreTableM20 


################################################
###########40%
##lm
model_motor <- lm(motor_UPDRS ~ age+sex+test_time+total_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson40)

model_total <- lm(total_UPDRS ~ age+sex+test_time+motor_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson40)

predicted40_M <- predict(model_motor,test)
predicted40_T<- predict(model_total,test)

lm_40_T <- RMSE(test$total_UPDRS, predicted40_T)
lm_40_M <-RMSE(test$motor_UPDRS, predicted40_M)
##knn
fit_40M <- train(motor_UPDRS ~.,
                data = parkinson40,
                tuneGrid = expand.grid(k=1:40),
                method = 'knn',
                metric = 'RMSE',
                trControl = trControl)
predicting_40 <- predict(fit_40M,test)
knnM40 <-accuracy(predicting_40,test$motor_UPDRS)
knnM40
fit_40T <- train(total_UPDRS ~.,
                 data = parkinson40,
                 tuneGrid = expand.grid(k=1:40),
                 method = 'knn',
                 metric = 'RMSE',
                 trControl = trControl)
predicting_40 <- predict(fit_40T,test)


knnT40 <-accuracy(predicting_40,test$total_UPDRS)

##hier
parkinsons_hier_T_40 <- parkinsons40[,-5]
parkinsons_hier_score_T_40 <- parkinsons40[,5]
parkinsons_hier_M_40 <- parkinsons40[,-4]
parkinsons_hier_score_M_40 <- parkinsons40[,4]

d_T_40 <- dist(parkinsons_hier_T_40, method = "euclidean")
d_M_40 <- dist(parkinsons_hier_M_40, method = "euclidean")


H_fit_T_40 <- hclust(d_T_40, method = "ward.D")
H_fit_M_40 <- hclust(d_M_40, method = "ward.D")

plot(H_fit_T_40)
plot(H_fit_M_40)
rect.hclust(H_fit_T_40, k=3, border="red")
rect.hclust(H_fit_M_40, k=3, border="red")

groups_T_40 <- cutree(H_fit_T_40, k=3)
groups_M_40 <- cutree(H_fit_M_40, k=3)

parkinsons_hier_score_M_40 <-cut(parkinsons40$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_hier_score_T_40 <-cut(parkinsons40$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))

scoreTableM40 <- table(parkinsons_hier_score_M_40, groups_M_40)/nrow(parkinson40)
scoreTableM40 <- table(parkinsons_hier_score_T_40, groups_T_40)/nrow(parkinson40)

scoreTableM40
scoreTableM40 


#################################################
###########60%
##lm
model_motor <- lm(motor_UPDRS ~ age+sex+test_time+total_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson60)

model_total <- lm(total_UPDRS ~ age+sex+test_time+motor_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson60)

predicted60_M <- predict(model_motor,test)
predicted60_T<- predict(model_total,test)

lm_60_T <- RMSE(test$total_UPDRS, predicted60_T)
lm_60_M <-RMSE(test$motor_UPDRS, predicted60_M)
##knn
fit_60M <- train(motor_UPDRS ~.,
                data = parkinson60,
                tuneGrid = expand.grid(k=1:40),
                method = 'knn',
                metric = 'RMSE',
                trControl = trControl)
predicting_60 <- predict(fit_60M,test)
knnM60 <-accuracy(predicting_60,test$motor_UPDRS)
knnM60
fit_60T <- train(total_UPDRS ~.,
                 data = parkinson60,
                 tuneGrid = expand.grid(k=1:40),
                 method = 'knn',
                 metric = 'RMSE',
                 trControl = trControl)
predicting_60 <- predict(fit_60T,test)


knnT60 <-accuracy(predicting_60,test$total_UPDRS)

##hier
parkinsons_hier_T_60 <- parkinsons60[,-5]
parkinsons_hier_score_T_60 <- parkinsons60[,5]
parkinsons_hier_M_60 <- parkinsons60[,-4]
parkinsons_hier_score_M_60 <- parkinsons60[,4]

d_T_60 <- dist(parkinsons_hier_T_60, method = "euclidean")
d_M_60 <- dist(parkinsons_hier_M_60, method = "euclidean")


H_fit_T_60 <- hclust(d_T_60, method = "ward.D")
H_fit_M_60 <- hclust(d_M_60, method = "ward.D")

plot(H_fit_T_60)
plot(H_fit_M_60)
rect.hclust(H_fit_T_60, k=3, border="red")
rect.hclust(H_fit_M_60, k=3, border="red")

groups_T_60 <- cutree(H_fit_T_60, k=3)
groups_M_60 <- cutree(H_fit_M_60, k=3)

parkinsons_hier_score_M_60 <-cut(parkinsons60$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_hier_score_T_60 <-cut(parkinsons60$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))

scoreTableM60 <- table(parkinsons_hier_score_M_60, groups_M_60)/nrow(parkinson60)
scoreTableM60 <- table(parkinsons_hier_score_T_60, groups_T_60)/nrow(parkinson60)

scoreTableM60
scoreTableM60 


####################################################
###########80%
##lm
model_motor <- lm(motor_UPDRS ~ age+sex+test_time+total_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson80)

model_total <- lm(total_UPDRS ~ age+sex+test_time+motor_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=parkinson80)

predicted80_M <- predict(model_motor,test)
predicted80_T<- predict(model_total,test)

lm_80_T <- RMSE(test$total_UPDRS, predicted80_T)
lm_80_M <-RMSE(test$motor_UPDRS, predicted80_M)
##knn
fit_80M <- train(motor_UPDRS ~.,
                data = parkinson80,
                tuneGrid = expand.grid(k=1:40),
                method = 'knn',
                metric = 'RMSE',
                trControl = trControl)
predicting_80 <- predict(fit_80M,test)
knnM80 <-accuracy(predicting_80,test$motor_UPDRS)

fit_80T <- train(total_UPDRS ~.,
                 data = parkinson80,
                 tuneGrid = expand.grid(k=1:40),
                 method = 'knn',
                 metric = 'RMSE',
                 trControl = trControl)
predicting_80 <- predict(fit_80T,test)

knnT80 <-accuracy(predicting_80,test$total_UPDRS)
##hier
parkinsons_hier_T_80 <- parkinsons80[,-5]
parkinsons_hier_score_T_80 <- parkinsons80[,5]
parkinsons_hier_M_80 <- parkinsons80[,-4]
parkinsons_hier_score_M_80 <- parkinsons80[,4]

d_T_80 <- dist(parkinsons_hier_T_80, method = "euclidean")
d_M_80 <- dist(parkinsons_hier_M_80, method = "euclidean")


H_fit_T_80 <- hclust(d_T_80, method = "ward.D")
H_fit_M_80 <- hclust(d_M_80, method = "ward.D")

plot(H_fit_T_80)
plot(H_fit_M_80)
rect.hclust(H_fit_T_80, k=3, border="red")
rect.hclust(H_fit_M_80, k=3, border="red")

groups_T_80 <- cutree(H_fit_T_80, k=3)
groups_M_80 <- cutree(H_fit_M_80, k=3)

parkinsons_hier_score_M_80 <-cut(parkinsons80$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_hier_score_T_80 <-cut(parkinsons80$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))

scoreTableM80 <- table(parkinsons_hier_score_M_80, groups_M_80)/nrow(parkinson80)
scoreTableM80 <- table(parkinsons_hier_score_T_80, groups_T_80)/nrow(parkinson80)

scoreTableM80
scoreTableM80 

per<-c("20","40","60","80")
percentageknnT<-c(knnT20,knnT40,knnT60,knnT80) 

plot(per,percentageknnT,type = "l", xlab = "percentage%",ylab = "accuracy value", main = "knn Total accuracy of each %")



per<-c("20","40","60","80")
percentageknnM<-c(knnM20,knnM40,knnM60,knnM80) 

plot(per,percentageknnM,type = "l", xlab = "percentage%",ylab = "accuracy value", main = "knn Motor accuracy of each %")



