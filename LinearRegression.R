###################################################################
######################## Linear Regression ########################
###################################################################
######################## CameraAccessory ########################
#setting the seed
set.seed(100)
train_indices <- sample(1:nrow(CameraAccessory),0.7*nrow(CameraAccessory))

train_CameraAccessory <- CameraAccessory[train_indices,]
test_CameraAccessory <- CameraAccessory[-train_indices,]

model_1 <- lm(sum_gmv~.,data=train_CameraAccessory)
summary(model_1)

model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)
vif(model_2)
model_3 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_customers + tot_list_price + 
                mean_list_price + promotion + payment_prepaid + payment_cod + 
                TV + Online.marketing + SEM + Radio + Other + Frequency + 
                TV_adStock + ContentMarketing_adStock + OnlineMarketing_adStock + 
                Affiliates_adStock + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraBatteryGrip + 
                product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + sale_typeBSD.5,data=train_CameraAccessory)
summary(model_3)
vif(model_3)

model_4 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + tot_customers + 
                tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod + Frequency + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + Affiliates, data = train_CameraAccessory)
summary(model_4)
vif(model_4)

model_5 <- lm(formula = sum_gmv ~ sum_mrp + tot_customers + 
                tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod + Frequency + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + Affiliates, data = train_CameraAccessory)
summary(model_5)
vif(model_5)


model_6 <- lm(formula = sum_gmv ~ tot_customers + 
                tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod + Frequency + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + Affiliates, data = train_CameraAccessory)
summary(model_6)
vif(model_6)

model_7 <- lm(formula = sum_gmv ~ tot_customers + 
                tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod  + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + Affiliates, data = train_CameraAccessory)
summary(model_7)
vif(model_7)

model_8 <- lm(formula = sum_gmv ~ tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod  + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens + Affiliates, data = train_CameraAccessory)
summary(model_8)
vif(model_8)

model_9 <- lm(formula = sum_gmv ~ tot_list_price + mean_list_price + promotion + payment_prepaid + 
                payment_cod  + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraRemoteControl + product_analytic_verticalCameraTripod + 
                product_analytic_verticalFilter + product_analytic_verticalFlash + 
                product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_9)
vif(model_9)

model_10 <- lm(formula = sum_gmv ~ tot_list_price + mean_list_price + promotion + payment_prepaid + 
                 payment_cod  + sale_typeBSD.5 + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraTripod + 
                 product_analytic_verticalFilter + product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_10)
vif(model_10)

model_11 <- lm(formula = sum_gmv ~ tot_list_price + mean_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraTripod + 
                 product_analytic_verticalFilter + product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_11)
vif(model_11)

model_12 <- lm(formula = sum_gmv ~ tot_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraTripod + 
                 product_analytic_verticalFilter + product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_12)
vif(model_12)

model_13 <- lm(formula = sum_gmv ~ tot_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalFilter + product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_13)
vif(model_13)

model_14 <- lm(formula = sum_gmv ~ tot_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalFilter + product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_14)
vif(model_14)

model_15 <- lm(formula = sum_gmv ~ tot_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_15)
vif(model_15)

model_16 <- lm(formula = sum_gmv ~ tot_list_price + promotion + payment_prepaid + 
                 payment_cod + product_analytic_verticalCameraBag + 
                 product_analytic_verticalFlash + 
                 product_analytic_verticalLens , data = train_CameraAccessory)
summary(model_16)
vif(model_16)

#R-squared:  0.9999,	Adjusted R-squared:  0.9999

####### Prediction
predict_ca <- predict(model_16,test_CameraAccessory[,-4])

test_CameraAccessory$test_gmv <- predict_ca
r <- cor(test_CameraAccessory$sum_gmv,test_CameraAccessory$test_gmv)
rsq <- cor(test_CameraAccessory$sum_gmv,test_CameraAccessory$test_gmv)^2
rsq
#0.9998989

#### apply cross validation
trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

set.seed(100)

# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.lm_ca <- train(sum_gmv~., data=train_CameraAccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_ca)

##R squared value - .9999613
fit.lm_ca$finalModel

# predict with cross validation

predict_ca_cv<- predict(fit.lm_ca, test_CameraAccessory[,-4])
test_CameraAccessory$test_gmv1 <- predict_ca_cv

r_ca_cv <- cor(test_CameraAccessory$sum_gmv,test_CameraAccessory$test_gmv1)
rsq_ca_cv <- cor(test_CameraAccessory$sum_gmv,test_CameraAccessory$test_gmv1)^2
rsq_Ca_cv
#0.9999824

#View(test_CameraAccessory)

############### Calculate Elasticity ##############################
elas_ca <- calculate_elasticity(model_16,CameraAccessory)
plot_elasticity(elas_ca,"Camera Accessory Linear Reg")

######################## HomeAudio ########################

#setting the seed
set.seed(100)
ha_train_indices <- sample(1:nrow(HomeAudio),0.7*nrow(HomeAudio))

train_HomeAudio <- HomeAudio[ha_train_indices,]
test_HomeAudio <- HomeAudio[-ha_train_indices,]

ha_model_1 <- lm(sum_gmv~.,data=train_HomeAudio)
summary(ha_model_1)

ha_model_2 <- stepAIC(ha_model_1, direction = "both")
summary(ha_model_2)
vif(ha_model_2)

ha_model_3 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + tot_list_price + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing + Online.marketing + SEM + 
                   Radio + Other + Frequency + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_3)
vif(ha_model_3)

ha_model_4 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + tot_list_price + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing + Online.marketing + 
                   Radio + Other + Frequency + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_4)
vif(ha_model_4)

ha_model_5 <- lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + tot_list_price + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing + Online.marketing + 
                   Radio + Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_5)
vif(ha_model_5)

ha_model_6 <- lm(formula = sum_gmv ~ sum_mrp + tot_list_price + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing + Online.marketing + 
                   Radio + Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_6)
vif(ha_model_6)

ha_model_7 <- lm(formula = sum_gmv ~ sum_mrp  + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing + Online.marketing + 
                   Radio + Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_7)
vif(ha_model_7)

ha_model_8 <- lm(formula = sum_gmv ~ sum_mrp  + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing  + 
                   Radio + Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_8)
vif(ha_model_8)


ha_model_9 <- lm(formula = sum_gmv ~ sum_mrp  + 
                   mean_list_price + promotion + payment_prepaid + payment_cod + 
                   TV + Digital + Content.Marketing  + 
                   Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                   product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                   Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_9)
vif(ha_model_9)

ha_model_10 <- lm(formula = sum_gmv ~ mean_list_price + promotion + payment_prepaid + payment_cod + 
                    TV + Digital + Content.Marketing  + 
                    Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                    product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                    Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_10)
vif(ha_model_10)

ha_model_11 <- lm(formula = sum_gmv ~ mean_list_price + promotion + payment_prepaid + payment_cod + 
                    TV + Content.Marketing  + 
                    Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                    product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                    Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_11)
vif(ha_model_11)

ha_model_12 <- lm(formula = sum_gmv ~ mean_list_price + promotion + payment_prepaid + payment_cod + 
                    TV  + 
                    Other + sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                    product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker + 
                    Month + sale_typeChristmas...New.Year.Sale, data = train_HomeAudio)
summary(ha_model_12)
vif(ha_model_12)

ha_model_13 <- lm(formula = sum_gmv ~ mean_list_price + promotion + payment_prepaid + payment_cod + 
                    product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker 
                  , data = train_HomeAudio)
summary(ha_model_13)
vif(ha_model_13)

ha_model_14 <- lm(formula = sum_gmv ~ payment_prepaid + payment_cod + 
                    product_analytic_verticalFMRadio + product_analytic_verticalHomeAudioSpeaker 
                  , data = train_HomeAudio)
summary(ha_model_14)
vif(ha_model_14)
#Multiple R-squared:  0.98,	Adjusted R-squared:  0.979

####### Prediction
predict_ha <- predict(ha_model_14,test_HomeAudio[,-4])

test_HomeAudio$test_gmv <- predict_ha
r <- cor(test_HomeAudio$sum_gmv,test_HomeAudio$test_gmv)
rsq <- cor(test_HomeAudio$sum_gmv,test_HomeAudio$test_gmv)^2
rsq
#0.982

# Performing 5-fold cross validation
fit.lm_ha <- train(sum_gmv~., data=train_HomeAudio, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_ha)

##R squared value - .9999745

# predict with cross validation
predict_ha_cv <- predict(fit.lm_ha,test_HomeAudio[,-4])

test_HomeAudio$test_gmv1 <- predict_ha_cv

r_ha_cv <- cor(test_HomeAudio$sum_gmv,test_HomeAudio$test_gmv1)
rsq_ha_cv <- cor(test_HomeAudio$sum_gmv,test_HomeAudio$test_gmv1)^2
rsq_ha_cv
#0.9999861


############### Calculate Elasticity ##############################
elas_ha <- calculate_elasticity(ha_model_14,HomeAudio)
plot_elasticity(elas_ha,"Home Audio Linear Reg")

######################## GamingAccessory ########################
#setting the seed
set.seed(100)
ga_train_indices <- sample(1:nrow(GamingAccessory),0.7*nrow(GamingAccessory))

train_GamingAccessory <- GamingAccessory[ga_train_indices,]
test_GamingAccessory <- GamingAccessory[-ga_train_indices,]

ga_model_1 <- lm(sum_gmv~.,data=train_GamingAccessory)
summary(ga_model_1)

ga_model_2 <- stepAIC(ga_model_1, direction = "both")
summary(ga_model_2)
vif(ga_model_2)

ga_model_3 <-lm(formula = sum_gmv ~ mean_gmv + sum_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                  Online.marketing + Affiliates + SEM + Frequency + sale_typeBig.Diwali.Sale + 
                  sale_typeEid...Rathayatra.sale + sale_typeValentine.s.Day + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_3)
vif(ga_model_3)

ga_model_4 <-lm(formula = sum_gmv ~ mean_gmv + sum_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid  + 
                  Online.marketing + Affiliates + SEM + Frequency + sale_typeBig.Diwali.Sale + 
                  sale_typeEid...Rathayatra.sale + sale_typeValentine.s.Day + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_4)
vif(ga_model_4)

ga_model_5 <-lm(formula = sum_gmv ~ sum_mrp + tot_customers + 
                  tot_list_price + payment_prepaid  + 
                  Online.marketing + Affiliates + SEM + Frequency + sale_typeBig.Diwali.Sale + 
                  sale_typeEid...Rathayatra.sale + sale_typeValentine.s.Day + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_5)
vif(ga_model_5)

ga_model_6 <-lm(formula = sum_gmv ~ sum_mrp + tot_customers + 
                  tot_list_price + payment_prepaid  + 
                  Online.marketing + Affiliates + SEM + sale_typeBig.Diwali.Sale + 
                  sale_typeValentine.s.Day + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_6)
vif(ga_model_6)

ga_model_7 <-lm(formula = sum_gmv ~ sum_mrp + 
                  tot_list_price + payment_prepaid  + 
                  Online.marketing + Affiliates + SEM + sale_typeBig.Diwali.Sale + 
                  sale_typeValentine.s.Day + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_7)
vif(ga_model_7)

ga_model_8 <-lm(formula = sum_gmv ~ sum_mrp + 
                  tot_list_price + payment_prepaid  + 
                  Online.marketing + Affiliates + SEM  + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_8)
vif(ga_model_8)


ga_model_9 <-lm(formula = sum_gmv ~ sum_mrp + 
                  tot_list_price + payment_prepaid  + 
                  Affiliates + SEM  + 
                  product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_9)
vif(ga_model_9)

ga_model_10 <-lm(formula = sum_gmv ~ sum_mrp + 
                   tot_list_price + payment_prepaid  + 
                   Affiliates   + 
                   product_analytic_verticalGamingKeyboard, data = train_GamingAccessory)

summary(ga_model_10)
vif(ga_model_10)


ga_model_11 <-lm(formula = sum_gmv ~ sum_mrp + 
                   tot_list_price + payment_prepaid,
                 data = train_GamingAccessory)

summary(ga_model_11)
vif(ga_model_11)

#Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998

####### Prediction
predict_ga <- predict(ga_model_11,test_GamingAccessory[,-4])
test_GamingAccessory$test_gmv <- predict_ga
r <- cor(test_GamingAccessory$sum_gmv,test_GamingAccessory$test_gmv)
rsq <- cor(test_GamingAccessory$sum_gmv,test_GamingAccessory$test_gmv)^2
rsq


# Performing 5-fold cross validation
fit.lm_ga <- train(sum_gmv~., data=train_GamingAccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_ga)

# predict with cross validation
predict_ga_cv <- predict(fit.lm_ga,test_GamingAccessory[,-4])

test_GamingAccessory$test_gmv1 <- predict_ga_cv
r_ga_cv <- cor(test_GamingAccessory$sum_gmv,test_GamingAccessory$test_gmv1)
rsq_ga_cv <- cor(test_GamingAccessory$sum_gmv,test_GamingAccessory$test_gmv1)^2
rsq_ga_cv
#.9997607

############### Calculate Elasticity ##############################
elas_ga <- calculate_elasticity(ga_model_11,GamingAccessory)
plot_elasticity(elas_ga,"Game Accessory Linear Reg")
                   
                   

