install.packages("keras")
library(Metrics)
library(class)
library(caret)
library(pROC)
library(mlbench)

#splitting the data set to test and train 
set.seed(1234)
ind <- sample(2, nrow(parkinsons), replace = T ,prob = c(0.8, 0.2))
train <- parkinsons[ind == 1,] 
test <- parkinsons[ind == 2,] 



#bilding a model of a multiple linear rigression
m_motor <- lm(motor_UPDRS ~ age+sex+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
              +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=train)

m_total <- lm(total_UPDRS ~ age+sex+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=train)

#bilding a model of a multiple linear rigression with both the target values

model_motor <- lm(motor_UPDRS ~ age+sex+test_time+total_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=train)

model_total <- lm(total_UPDRS ~ age+sex+test_time+motor_UPDRS+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+
                    +Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=train)
summary(model_motor)
summary(model_total)
summary(m_motor)
summary(m_total)



#finding the predicted values using the model that we made 
predicted_values_M <- predict(m_motor,test)
predicted_values_M_full <- predict(model_motor,test)
predicted_values_T<- predict(m_total,test)
predicted_values_T_full<- predict(model_total,test)

#evaluating results
RMSE(test$motor_UPDRS,predicted_values_M)
plot(predicted_values_M ~ test$motor_UPDRS)
cor(test$motor_UPDRS,predicted_values_M)

RMSE(test$motor_UPDRS,predicted_values_M_full)
plot(predicted_values_M_full ~ test$motor_UPDRS)
cor(test$motor_UPDRS,predicted_values_M_full)

RMSE(test$total_UPDRS,predicted_values_T)
plot(predicted_values_T ~ test$total_UPDRS)
cor(test$total_UPDRS,predicted_values_T)

RMSE(test$total_UPDRS, predicted_values_T_full)
plot(predicted_values_T_full ~ test$total_UPDRS)
cor(predicted_values_T_full , test$total_UPDRS)









