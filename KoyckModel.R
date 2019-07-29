###################################################################
######################## Koyck Model ####################
###################################################################

########################## Prepare Data ###########################

#colnames(consolidated5)

koyck_consolidated <- consolidated5 %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_GMV=lag(sum_gmv)) %>% mutate(Prev_mean_gmv=lag(mean_gmv))
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_units=lag(units))
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_sum_mrp=lag(sum_mrp)) %>% mutate(Prev_mean_mrp=lag(mean_mrp))
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_cust=lag(tot_customers))
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_list_price=lag(tot_list_price)) %>% mutate(Prev_mean_list_price=lag(mean_list_price))
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TV=lag(TV)) %>% mutate(Prev_Digital=lag(Digital)) %>% mutate(Prev_Sponsorship=lag(Sponsorship)) %>% mutate(Prev_Content.Marketing=lag(Content.Marketing)) %>% mutate(Prev_Online.Marketing=lag(Online.marketing)) %>% mutate(Prev_Affiliates=lag(Affiliates)) %>% mutate(Prev_SEM=lag(SEM)) %>% mutate(Prev_Radio=lag(Radio)) %>% mutate(Prev_Other=lag(Other)) 
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TVadStock=lag(TV_adStock)) %>% mutate(Prev_Digital_adStock=lag(Digital_adStock)) %>% mutate(Prev_ContentMarketing_adStock=lag(ContentMarketing_adStock)) %>% mutate(Prev_OnlineMarketing_adStock=lag(OnlineMarketing_adStock)) %>% mutate(Prev_Affiliates_adStock=lag(Affiliates_adStock)) %>% mutate(Prev_SEM_adStock=lag(SEM_adStock)) %>% mutate(Prev_Radio_adStock=lag(Radio_adStock)) %>% mutate(Prev_Other_adStock=lag(Other_adStock)) 
koyck_consolidated <- koyck_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_NPS=lag(NPS))
#View(koyck_consolidated)

#Divide the master data frame into three categories
koyck_CameraAccessory <- koyck_consolidated[which(koyck_consolidated$product_analytic_sub_category=="CameraAccessory"),]
koyck_CameraAccessory <- subset(koyck_CameraAccessory,select = -c(product_analytic_sub_category))
#View(koyck_CameraAccessory)

koyck_HomeAudio <- koyck_consolidated[which(koyck_consolidated$product_analytic_sub_category=="HomeAudio"),]
koyck_HomeAudio <- subset(koyck_HomeAudio,select = -c(product_analytic_sub_category))
#View(koyck_HomeAudio)

koyck_GamingAccessory <- koyck_consolidated[which(koyck_consolidated$product_analytic_sub_category=="GamingAccessory"),]
koyck_GamingAccessory <- subset(koyck_GamingAccessory,select = -c(product_analytic_sub_category))
#View(koyck_GamingAccessory)

######################## CameraAccessory ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
koyck_CameraAccessory <- subset(koyck_CameraAccessory,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(koyck_CameraAccessory))

#Remove NA values
koyck_CameraAccessory <- na.omit(koyck_CameraAccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
koyck_train_indices <- sample(1:nrow(koyck_CameraAccessory),0.7*nrow(koyck_CameraAccessory))
train_koyck_camaccessory <- koyck_CameraAccessory[koyck_train_indices,]
test_koyck_camaccessory <- koyck_CameraAccessory[-koyck_train_indices,]

koyck_cam_1 <- lm(sum_gmv~.,data=train_koyck_camaccessory)
summary(koyck_cam_1)

koyck_cam_2 <- stepAIC(koyck_cam_1, direction = "both")
summary(koyck_cam_2)

#After removing all variables with high VIF or low significance
koyck_cam_3 = lm(formula = sum_gmv ~ mean_gmv  + sum_mrp + mean_mrp,
                  data = train_koyck_camaccessory)

summary(koyck_cam_3)
#Multiple R-squared:  0.908,	Adjusted R-squared:  0.908 

###Prediction
koyck_cam_pred <- predict(koyck_cam_3, test_koyck_camaccessory[,-1])
test_koyck_camaccessory$gmv_pred <- koyck_cam_pred

dlag_ga_r <- cor(test_koyck_camaccessory$sum_gmv, test_koyck_camaccessory$gmv_pred)
dlag_ga_r_sq <- dlag_ga_r^2
dlag_ga_r_sq     
#0.905

# Performing 5-fold cross validation
fit.lm_kyck_ca <- train(sum_gmv~., data=train_koyck_camaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_kyck_ca)
#R square.9999636

fit.lm_kyck_ca$finalModel

###Prediction using cross validation
koyck_cam_pred1 <- predict(fit.lm_kyck_ca, test_koyck_camaccessory[,-1])
test_koyck_camaccessory$gmv_pred1 <- koyck_cam_pred1

dlag_ga_r1 <- cor(test_koyck_camaccessory$sum_gmv, test_koyck_camaccessory$gmv_pred1)
dlag_ga_r_sq1 <- dlag_ga_r1^2
dlag_ga_r_sq1    
#0.9999305


############### Calculate Elasticity ##############################
elas_k_cam <- calculate_elasticity(koyck_cam_3,koyck_CameraAccessory)
plot_elasticity(elas_k_cam,"Camera Accessory Koyck Model")


######################## HomeAudio ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
koyck_HomeAudio <- subset(koyck_HomeAudio,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(koyck_HomeAudio))

#Remove NA values
koyck_HomeAudio <- na.omit(koyck_HomeAudio)

#Setting seed value
set.seed(100)

#Divide data into train and test data
koyck_train_indices <- sample(1:nrow(koyck_HomeAudio),0.7*nrow(koyck_HomeAudio))
train_koyck_homeaudio <- koyck_HomeAudio[koyck_train_indices,]
test_koyck_homeaudio <- koyck_HomeAudio[-koyck_train_indices,]

koyck_ha_1 <- lm(sum_gmv~.,data=train_koyck_homeaudio)
summary(koyck_ha_1)

koyck_ha_2 <- stepAIC(koyck_ha_1, direction = "both")
summary(koyck_ha_2)

#After removing all variables with high VIF or low significance
koyck_ha_3 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                 mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                 Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                 SEM + Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + 
                 sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                 product_analytic_verticalDJController + product_analytic_verticalDock + 
                 product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                 product_analytic_verticalKaraokePlayer + Prev_tot_cust + 
                 Prev_tot_list_price + Prev_mean_list_price, data = train_koyck_homeaudio)

summary(koyck_ha_3)

koyck_ha_4 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                 mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                 Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                 SEM + Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + 
                 sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                 product_analytic_verticalDJController + product_analytic_verticalDock + 
                 product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                 product_analytic_verticalKaraokePlayer,
                 data = train_koyck_homeaudio)

summary(koyck_ha_4)
vif(koyck_ha_4)

koyck_ha_5 = lm(formula = sum_gmv ~ mean_gmv + units + 
                 mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                 Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                 SEM + Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + 
                 sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                 product_analytic_verticalDJController + product_analytic_verticalDock + 
                 product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                 product_analytic_verticalKaraokePlayer,
               data = train_koyck_homeaudio)

summary(koyck_ha_5)
vif(koyck_ha_5)

koyck_ha_6 = lm(formula = sum_gmv ~ mean_gmv + units + 
                 mean_list_price + payment_prepaid + payment_cod + TV  + 
                 Sponsorship + Content.Marketing + Online.marketing  + 
                 SEM + Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + 
                 sale_typeRepublic.Day + product_analytic_verticalBoomBox + 
                 product_analytic_verticalDJController + product_analytic_verticalDock + 
                 product_analytic_verticalDockingStation + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker,
               data = train_koyck_homeaudio)

summary(koyck_ha_6)
vif(koyck_ha_6)
  
koyck_ha_7 = lm(formula = sum_gmv ~
                payment_prepaid + payment_cod +
                product_analytic_verticalFMRadio,
               data = train_koyck_homeaudio)

summary(koyck_ha_7)
vif(koyck_ha_7)
#Multiple R-squared:  0.985,	Adjusted R-squared:  0.984 

### Prediction
koyck_ha_pred <- predict(koyck_ha_7, test_koyck_homeaudio[,-1])
test_koyck_homeaudio$gmv_pred <- koyck_ha_pred

dlag_ga_r <- cor(test_koyck_homeaudio$sum_gmv, test_koyck_homeaudio$gmv_pred)
dlag_ga_r_sq <- dlag_ga_r^2
dlag_ga_r_sq  
#0.973

# Performing 5-fold cross validation
fit.lm_kyck_ha <- train(sum_gmv~., data=train_koyck_homeaudio, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_kyck_ha)
#R square.9999442

fit.lm_kyck_ha$finalModel

### Prediction using cross validation
koyck_ha_pred1 <- predict(fit.lm_kyck_ha, test_koyck_homeaudio[,-1])
test_koyck_homeaudio$gmv_pred1 <- koyck_ha_pred1

koyck_ha_r1 <- cor(test_koyck_homeaudio$sum_gmv, test_koyck_homeaudio$gmv_pred1)
koyck_ha_r_sq1 <- koyck_ha_r1^2
koyck_ha_r_sq1 
#0.9999823

############### Calculate Elasticity ##############################
elas_k_ha <- calculate_elasticity(koyck_ha_7,koyck_HomeAudio)
plot_elasticity(elas_k_ha,"Home Audio Koyck Model")


######################## GamingAccessory ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
koyck_GamingAccessory <- subset(koyck_GamingAccessory,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(koyck_GamingAccessory))

#Remove NA values
koyck_GamingAccessory <- na.omit(koyck_GamingAccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
koyck_train_indices <- sample(1:nrow(koyck_GamingAccessory),0.7*nrow(koyck_GamingAccessory))
train_koyck_gamingaccessory <- koyck_GamingAccessory[koyck_train_indices,]
test_koyck_gamingaccessory <- koyck_GamingAccessory[-koyck_train_indices,]

koyck_ga_1 <- lm(sum_gmv~.,data=train_koyck_gamingaccessory)
summary(koyck_ga_1)

koyck_ga_2 <- stepAIC(koyck_ga_1, direction = "both")
summary(koyck_ga_2)
vif(koyck_ga_2)

koyck_ga_3 <- lm(formula = sum_gmv ~ mean_gmv + units + mean_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                  TV + Digital + Content.Marketing  + Affiliates + 
                  SEM + Radio + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + NPS + sale_typePacman + sale_typeRepublic.Day + 
                  product_analytic_verticalCoolingPad + product_analytic_verticalGamePad + 
                  product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                  product_analytic_verticalGamingGun + product_analytic_verticalGamingHeadset + 
                  product_analytic_verticalGamingKeyboard + product_analytic_verticalGamingMouse + 
                  product_analytic_verticalGamingMousePad + product_analytic_verticalGamingSpeaker + 
                  Prev_sum_mrp + Prev_TV + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_SEM + 
                  Prev_Radio + Prev_Other + Prev_TVadStock + Prev_Digital_adStock + 
                  Prev_ContentMarketing_adStock + Prev_OnlineMarketing_adStock + 
                  Prev_NPS, data = train_koyck_gamingaccessory)
summary(koyck_ga_3)
vif(koyck_ga_3)

koyck_ga_4 <- lm(formula = sum_gmv ~ mean_gmv + units + mean_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                  TV + Digital + Content.Marketing  + Affiliates + 
                  SEM + Radio + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + NPS + sale_typePacman + sale_typeRepublic.Day + 
                  product_analytic_verticalCoolingPad + product_analytic_verticalGamePad + 
                  product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                  product_analytic_verticalGamingGun + product_analytic_verticalGamingHeadset + 
                  product_analytic_verticalGamingKeyboard + product_analytic_verticalGamingMouse + 
                  product_analytic_verticalGamingMousePad + product_analytic_verticalGamingSpeaker + 
                  Prev_sum_mrp + Prev_TV + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_SEM + 
                  Prev_Radio  + Prev_TVadStock + Prev_Digital_adStock + 
                  Prev_ContentMarketing_adStock + Prev_OnlineMarketing_adStock + 
                  Prev_NPS, data = train_koyck_gamingaccessory)
summary(koyck_ga_4)
vif(koyck_ga_4)

koyck_ga_5 <- lm(formula = sum_gmv ~ mean_mrp + 
                  tot_list_price + mean_list_price + payment_prepaid  + 
                  TV + Content.Marketing + 
                  Radio + Frequency  + Digital_adStock + 
                  NPS + sale_typePacman + sale_typeRepublic.Day + 
                  product_analytic_verticalCoolingPad + product_analytic_verticalGamePad + 
                  product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                  product_analytic_verticalGamingGun + product_analytic_verticalGamingHeadset + 
                  product_analytic_verticalGamingKeyboard + product_analytic_verticalGamingMouse + 
                  product_analytic_verticalGamingMousePad + product_analytic_verticalGamingSpeaker + 
                  Prev_sum_mrp + 
                  Prev_Radio  + Prev_TVadStock + Prev_Digital_adStock,
                  data = train_koyck_gamingaccessory)
summary(koyck_ga_5)
vif(koyck_ga_5)

koyck_ga_6 <- lm(formula = sum_gmv ~ mean_mrp + 
                  tot_list_price + payment_prepaid  + 
                  TV + Content.Marketing + 
                  Radio + Frequency  + 
                  NPS + sale_typePacman + sale_typeRepublic.Day + 
                  product_analytic_verticalCoolingPad + product_analytic_verticalGamePad + 
                  product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingChargingStation + 
                  product_analytic_verticalGamingGun + product_analytic_verticalGamingHeadset + 
                  product_analytic_verticalGamingKeyboard + product_analytic_verticalGamingMouse + 
                  product_analytic_verticalGamingMousePad + product_analytic_verticalGamingSpeaker + 
                  Prev_sum_mrp + 
                  Prev_Radio  + Prev_TVadStock + Prev_Digital_adStock,
                data = train_koyck_gamingaccessory)
summary(koyck_ga_6)
vif(koyck_ga_6)

koyck_ga_7 <- lm(formula = sum_gmv ~ tot_list_price  + 
                  Content.Marketing + 
                  product_analytic_verticalGamePad + 
                  product_analytic_verticalGamingHeadset + 
                  Prev_sum_mrp,
                data = train_koyck_gamingaccessory)
summary(koyck_ga_7)
vif(koyck_ga_7)

koyck_ga_8 <- lm(formula = sum_gmv ~ tot_list_price  + 
                  product_analytic_verticalGamePad,
                data = train_koyck_gamingaccessory)
summary(koyck_ga_8)
vif(koyck_ga_8)

###Prediction
koyck_ga_pred <- predict(koyck_ga_8, test_koyck_gamingaccessory[,-1])
test_koyck_gamingaccessory$gmv_pred <- koyck_ga_pred

koyck_ga_r <- cor(test_koyck_gamingaccessory$sum_gmv, test_koyck_gamingaccessory$gmv_pred)
koyck_ga_r_sq <- koyck_ga_r^2
koyck_ga_r_sq   
# 0.9997417

# Performing 5-fold cross validation
fit.lm_kyck_ga <- train(sum_gmv~., data=train_koyck_gamingaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_kyck_ga)
#R square.9998452

fit.lm_kyck_ga$finalModel

### Prediction using cross validation
koyck_ga_pred1 <- predict(fit.lm_kyck_ga, test_koyck_gamingaccessory[,-1])
test_koyck_gamingaccessory$gmv_pred1 <- koyck_ga_pred1

koyck_ga_r1 <- cor(test_koyck_gamingaccessory$sum_gmv, test_koyck_gamingaccessory$gmv_pred1)
koyck_ga_r_sq1 <- koyck_ga_r1^2
koyck_ga_r_sq1 
#0.9995812

############### Calculate Elasticity ##############################
elas_k_ga <- calculate_elasticity(koyck_ga_8,koyck_GamingAccessory)
plot_elasticity(elas_k_ga,"Game Accessory Koyck Model")

