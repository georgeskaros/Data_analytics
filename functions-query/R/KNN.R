#install.packages("Metrics")
library(class)
library(caret)
library(pROC)
library(mlbench)
library(Metrics)
#parkinsons <- parkinsons[,-1]

set.seed(1234)

ind <- sample(2, nrow(parkinsons), replace = T ,prob = c(0.8, 0.2))
train <- parkinsons[ind == 1,] 
test <- parkinsons[ind == 2,] 

parkinsons_motor <- parkinsons[,-5]
parkinsons_total <- parkinsons[,-4]

mot <- sample(2, nrow(parkinsons_motor), replace = T ,prob = c(0.8, 0.2))
tot <- sample(2, nrow(parkinsons_total), replace = T ,prob = c(0.8, 0.2))

train_mot <- parkinsons_motor[mot == 1,] 
test_mot <- parkinsons_motor[mot == 2,] 

train_tot <- parkinsons_total[tot == 1,] 
test_tot <- parkinsons_total[tot == 2,] 



test_labels_mot <- test_mot[,4]
train_labels_mot <- train_mot[,4]

test_labels_tot <- test_tot[,4]
train_labels_tot <- train_tot[,4]
##########################################
#knn regression with both target columns
##########################################
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)

fit_m <- train(motor_UPDRS ~.,
              data = train,
              tuneGrid = expand.grid(k=1:40),
              method = 'knn',
              metric = 'RMSE',
              trControl = trControl)
fit_m
plot(fit_m)
varImp(fit_m)
predicting_m <- predict(fit_m,test)


accuracy(predicting_m,test$motor_UPDRS)
RMSE(predicting_m, test_labels_mot)
plot(predicting_m ~ test$motor_UPDRS)


############################################


fit_t <- train(total_UPDRS ~.,
              data = train,
              tuneGrid = expand.grid(k=1:40),
              method = 'knn',
              metric = 'RMSE',
              trControl = trControl)
fit_t
plot(fit_t)
varImp(fit_t)
predicting_t <- predict(fit_t,test)


accuracy(predicting_t,test$total_UPDRS)
RMSE(predicting_t,test$total_UPDRS)
plot(predicting_t ~ test$total_UPDRS)

##########################################
#knn regression with one target column 
##########################################

motor_fit <- train(motor_UPDRS ~.,
                  data = train_mot,
                  tuneGrid = expand.grid(k=1:40),
                  method = 'knn',
                  metric = 'RMSE',
                  trControl = trControl)

total_fit <- train(total_UPDRS ~.,
                  data = train_tot,
                  tuneGrid = expand.grid(k=1:40),
                  method = 'knn',
                  metric = 'RMSE',
                  trControl = trControl)

motor_fit
summary(motor_fit)
plot(motor_fit)
varImp(motor_fit)

total_fit
summary(total_fit)
plot(total_fit)
varImp(total_fit)

pred_total <- predict(total_fit, test_tot)
pred_motor <- predict(motor_fit, test_mot)

accuracy(pred_motor,test_labels_mot)
accuracy(pred_total,test_labels_tot)

RMSE(pred_motor, test_labels_mot)
plot(pred_motor ~ test_labels_mot)


RMSE(pred_total, test_labels_tot)
plot(pred_total ~ test_labels_tot)



###################################################

 