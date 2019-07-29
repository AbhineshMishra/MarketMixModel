###################################################################
######################## Multiplicative Model ########################
###################################################################

######################## CameraAccessory ########################

mm_camaccessory <- CameraAccessory
#View(mm_camaccessory)

#Log(0) is not defined. Set all 0's to 0.001
mm_camaccessory[mm_camaccessory==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_camaccessory <- subset(mm_camaccessory,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_camaccessory$NPS <- as.numeric(mm_camaccessory$NPS)

#Take log of all numeric values
mm_camaccessory <- log(mm_camaccessory[,1:ncol(mm_camaccessory)])

#Check for NA values
which(is.na(mm_camaccessory))

#Remove NA values
mm_camaccessory <- na.omit(mm_camaccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
train_indices <- sample(1:nrow(mm_camaccessory),0.7*nrow(mm_camaccessory))
train_mm_camaccessory <- mm_camaccessory[train_indices,]
test_mm_camaccessory <- mm_camaccessory[-train_indices,]

mm_cam_1 <- lm(sum_gmv~.,data=train_mm_camaccessory)
summary(mm_cam_1)

mm_cam_2 <- stepAIC(mm_cam_1, direction = "both")
summary(mm_cam_2)

#After removing all variables with high VIF or low significance
mm_cam_3 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp  + 
                 SEM_adStock + 
                 sale_typeIndependence.Sale,
               data = train_mm_camaccessory)

summary(mm_cam_3)
vif(mm_cam_3)

#Prediction
predict_mm_ca <- predict(mm_cam_3,test_mm_camaccessory[,-1])

test_mm_camaccessory$test_gmv <- predict_mm_ca
r <- cor(test_mm_camaccessory$sum_gmv,test_mm_camaccessory$test_gmv)
rsq <- cor(test_mm_camaccessory$sum_gmv,test_mm_camaccessory$test_gmv)^2
rsq

# Performing 5-fold cross validation
fit.lm_mm_ca <- train(sum_gmv~., data=train_mm_camaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mm_ca)
#R square - 1

fit.lm_mm_ca$finalModel

#predict with cross validation
predict_mm_ca1 <- predict(fit.lm_mm_ca,test_mm_camaccessory[,-1])
test_mm_camaccessory$test_gmv1 <- predict_mm_ca1
r_mm_ca_cv <- cor(test_mm_camaccessory$sum_gmv,test_mm_camaccessory$test_gmv1)
rsq_mm_ca_cv <- cor(test_mm_camaccessory$sum_gmv,test_mm_camaccessory$test_gmv1)^2
rsq_mm_ca_cv
## rsquare is 1

############### Calculate Elasticity ##############################
elas_mm_cam <- calculate_elasticity(mm_cam_3,mm_camaccessory)
plot_elasticity(elas_mm_cam,"Camera Accessory MM Model")

######################## HomeAudio ########################
mm_homeaudio <- HomeAudio
#View(mm_homeaudio)

#Log(0) is not defined. Set all 0's to 0.001
mm_homeaudio[mm_homeaudio==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_homeaudio <- subset(mm_homeaudio,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_homeaudio$NPS <- as.numeric(mm_homeaudio$NPS)

#Take log of all numeric values
mm_homeaudio <- log(mm_homeaudio[,1:ncol(mm_homeaudio)])

#Check for NA values
which(is.na(mm_homeaudio))

#Remove NA values
mm_homeaudio <- na.omit(mm_homeaudio)

#setting the seed
set.seed(100)

mm_ha_indices <- sample(1:nrow(mm_homeaudio),0.7*nrow(mm_homeaudio))

train_mm_ha <- mm_homeaudio[mm_ha_indices,]
test_mm_ha <- mm_homeaudio[-mm_ha_indices,]

ha_mm_1 <- lm(sum_gmv~.,data=train_mm_ha)
summary(ha_mm_1)

ha_mm_2 <- stepAIC(ha_mm_1, direction = "both")
summary(ha_mm_2)

#After removing all variables with high VIF or low significance
ha_mm_3 <- lm(formula = sum_gmv ~ mean_gmv + sum_mrp + mean_mrp, 
                 data = train_mm_ha)
summary(ha_mm_3)

#Prediction
predict_mm_ha <- predict(ha_mm_3,test_mm_ha[,-1])

test_mm_ha$test_gmv <- predict_mm_ha
r <- cor(test_mm_ha$sum_gmv,test_mm_ha$test_gmv)
rsq <- cor(test_mm_ha$sum_gmv,test_mm_ha$test_gmv)^2
rsq

# Performing 5-fold cross validation
fit.lm_mm_ha <- train(sum_gmv~., data=train_mm_ha, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mm_ha)
#R square - 1

fit.lm_mm_ha$finalModel

#predict with cross validation
predict_mm_ha_1 <- predict(fit.lm_mm_ha,test_mm_ha[,-1])

test_mm_ha$test_gmv1 <- predict_mm_ha_1
r_mm_ha_cv <- cor(test_mm_ha$sum_gmv,test_mm_ha$test_gmv1)
rsq_mm_ha_cv <- cor(test_mm_ha$sum_gmv,test_mm_ha$test_gmv1)^2
rsq_mm_ha_cv
##1

############### Calculate Elasticity ##############################
elas_mm_ha <- calculate_elasticity(ha_mm_3,mm_homeaudio)
plot_elasticity(elas_mm_ha,"Home Audio MM Model")

######################## GamingAccessory ########################

mm_ga <- GamingAccessory
#View(mm_ga)

#Log(0) is not defined. Set all 0's to 0.001
mm_ga[mm_ga==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_ga <- subset(mm_ga,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_ga$NPS <- as.numeric(mm_ga$NPS)

#Take log of all numeric values
mm_ga <- log(mm_ga[,1:ncol(mm_ga)])

#Check for NA values
which(is.na(mm_ga))

#Remove NA values
mm_ga <- na.omit(mm_ga)

#setting the seed
set.seed(100)
mm_ga_indices <- sample(1:nrow(mm_ga),0.7*nrow(mm_ga))

train_mm_ga <- mm_ga[mm_ga_indices,]
test_mm_ga <- mm_ga[-mm_ga_indices,]

mm_ga_1 <- lm(sum_gmv~.,data=train_mm_ga)
summary(ga_model_1)

mm_ga_2 <- stepAIC(mm_ga_1, direction = "both")
summary(mm_ga_2)
#vif(mm_ga_2)

#After removing all variables with high VIF or low significance
mm_ga_3 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + mean_mrp,
     data = train_mm_ga)

summary(mm_ga_3)
vif(mm_ga_3)

#Prediction
predict_mm_ga <- predict(mm_ga_3,test_mm_ga[,-1])

test_mm_ga$test_gmv <- predict_mm_ga
r <- cor(test_mm_ga$sum_gmv,test_mm_ga$test_gmv)
rsq <- cor(test_mm_ga$sum_gmv,test_mm_ga$test_gmv)^2
rsq

# Performing 5-fold cross validation
fit.lm_mm_ga <- train(sum_gmv~., data=train_mm_ga, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mm_ga)
#R square - 1

fit.lm_mm_ga$finalModel
# predict with cross validation
predict_mm_ga_1 <- predict(fit.lm_mm_ga,test_mm_ga[,-1])

test_mm_ga$test_gmv1 <- predict_mm_ga_1
r_mm_ga_cv <- cor(test_mm_ga$sum_gmv,test_mm_ga$test_gmv1)
rsq_mm_ga_cv <- cor(test_mm_ga$sum_gmv,test_mm_ga$test_gmv1)^2

rsq_mm_ga_cv
##1


############### Calculate Elasticity ##############################
elas_mm_ga <- calculate_elasticity(mm_ga_3,mm_ga)
plot_elasticity(elas_mm_ga,"Game Accessory MM Model")
