###################################################################
######################## Distributed Lag Model ####################
###################################################################

########################## Prepare Data ###########################

#colnames(consolidated5)

dlag_consolidated <- consolidated5 %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_GMV=lag(sum_gmv)) %>% mutate(Prev_mean_gmv=lag(mean_gmv)) %>% mutate(Prev2_GMV=lag(sum_gmv,2)) %>% mutate(Prev2_mean_gmv=lag(mean_gmv,2))
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_units=lag(units)) %>% mutate(Prev2_units=lag(units,2))
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_sum_mrp=lag(sum_mrp)) %>% mutate(Prev_mean_mrp=lag(mean_mrp)) %>% mutate(Prev2_sum_mrp=lag(sum_mrp,2)) %>% mutate(Prev2_mean_mrp=lag(mean_mrp,2))
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_cust=lag(tot_customers)) %>% mutate(Prev2_tot_cust=lag(tot_customers,2))
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_list_price=lag(tot_list_price)) %>% mutate(Prev_mean_list_price=lag(mean_list_price)) %>% mutate(Prev2_tot_list_price=lag(tot_list_price,2)) %>% mutate(Prev2_mean_list_price=lag(mean_list_price,2))
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TV=lag(TV)) %>% mutate(Prev_Digital=lag(Digital)) %>% mutate(Prev_Sponsorship=lag(Sponsorship)) %>% mutate(Prev_Content.Marketing=lag(Content.Marketing)) %>% mutate(Prev_Online.Marketing=lag(Online.marketing)) %>% mutate(Prev_Affiliates=lag(Affiliates)) %>% mutate(Prev_SEM=lag(SEM)) %>% mutate(Prev_Radio=lag(Radio)) %>% mutate(Prev_Other=lag(Other)) %>% mutate(Prev2_TV=lag(TV,2)) %>% mutate(Prev2_Digital=lag(Digital,2)) %>% mutate(Prev2_Sponsorship=lag(Sponsorship,2)) %>% mutate(Prev2_Content.Marketing=lag(Content.Marketing,2)) %>% mutate(Prev2_Online.Marketing=lag(Online.marketing,2)) %>% mutate(Prev2_Affiliates=lag(Affiliates,2)) %>% mutate(Prev2_SEM=lag(SEM,2)) %>% mutate(Prev2_Radio=lag(Radio,2)) %>% mutate(Prev2_Other=lag(Other,2))  
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TVadStock=lag(TV_adStock)) %>% mutate(Prev_Digital_adStock=lag(Digital_adStock)) %>% mutate(Prev_ContentMarketing_adStock=lag(ContentMarketing_adStock)) %>% mutate(Prev_OnlineMarketing_adStock=lag(OnlineMarketing_adStock)) %>% mutate(Prev_Affiliates_adStock=lag(Affiliates_adStock)) %>% mutate(Prev_SEM_adStock=lag(SEM_adStock)) %>% mutate(Prev_Radio_adStock=lag(Radio_adStock)) %>% mutate(Prev_Other_adStock=lag(Other_adStock)) %>% mutate(Prev2_TVadStock=lag(TV_adStock,2)) %>% mutate(Prev2_Digital_adStock=lag(Digital_adStock,2)) %>% mutate(Prev2_ContentMarketing_adStock=lag(ContentMarketing_adStock,2)) %>% mutate(Prev2_OnlineMarketing_adStock=lag(OnlineMarketing_adStock,2)) %>% mutate(Prev2_Affiliates_adStock=lag(Affiliates_adStock,2)) %>% mutate(Prev2_SEM_adStock=lag(SEM_adStock,2)) %>% mutate(Prev2_Radio_adStock=lag(Radio_adStock,2)) %>% mutate(Prev2_Other_adStock=lag(Other_adStock,2))  
dlag_consolidated <- dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_NPS=lag(NPS)) %>% mutate(Prev2_NPS=lag(NPS,2))
#View(dlag_consolidated)

#Divide the master data frame into three categories
dlag_CameraAccessory <- dlag_consolidated[which(dlag_consolidated$product_analytic_sub_category=="CameraAccessory"),]
dlag_CameraAccessory <- subset(dlag_CameraAccessory,select = -c(product_analytic_sub_category))
#View(dlag_CameraAccessory)

dlag_HomeAudio <- dlag_consolidated[which(dlag_consolidated$product_analytic_sub_category=="HomeAudio"),]
dlag_HomeAudio <- subset(dlag_HomeAudio,select = -c(product_analytic_sub_category))
#View(dlag_HomeAudio)

dlag_GamingAccessory <- dlag_consolidated[which(dlag_consolidated$product_analytic_sub_category=="GamingAccessory"),]
dlag_GamingAccessory <- subset(dlag_GamingAccessory,select = -c(product_analytic_sub_category))
#View(dlag_GamingAccessory)

######################## CameraAccessory ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
dlag_CameraAccessory <- subset(dlag_CameraAccessory,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(dlag_CameraAccessory))

#Remove NA values
dlag_CameraAccessory <- na.omit(dlag_CameraAccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
dlag_train_indices <- sample(1:nrow(dlag_CameraAccessory),0.7*nrow(dlag_CameraAccessory))
train_dlag_camaccessory <- dlag_CameraAccessory[dlag_train_indices,]
test_dlag_camaccessory <- dlag_CameraAccessory[-dlag_train_indices,]

dlag_cam_1 <- lm(sum_gmv~.,data=train_dlag_camaccessory)
summary(dlag_cam_1)

dlag_cam_2 <- stepAIC(dlag_cam_1, direction = "both")
summary(dlag_cam_2)

dlag_cam_3 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                  Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeBSD.5 + 
                  sale_typeEid...Rathayatra.sale + sale_typeRakshabandhan.Sale + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFilter + product_analytic_verticalFlash + 
                  Prev2_GMV + Prev_units + Prev2_units + Prev_tot_cust + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other + Prev2_TV + Prev2_Digital + 
                  Prev2_Sponsorship + Prev2_Online.Marketing + Prev2_SEM + 
                  Prev2_Other + Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_3)
vif(dlag_cam_3)  

dlag_cam_4 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                  Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeBSD.5 + 
                  sale_typeEid...Rathayatra.sale + sale_typeRakshabandhan.Sale + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFilter + product_analytic_verticalFlash + 
                  Prev_units + Prev2_units + Prev_tot_cust + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other + Prev2_TV + Prev2_Digital + 
                  Prev2_Sponsorship + Prev2_Online.Marketing + Prev2_SEM + 
                  Prev2_Other + Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_4)
vif(dlag_cam_4)  

dlag_cam_5 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                  Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeBSD.5 + 
                  sale_typeRakshabandhan.Sale + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFilter + product_analytic_verticalFlash + 
                  Prev_units + Prev2_units + Prev_tot_cust + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other + Prev2_TV + Prev2_Digital + 
                  Prev2_Sponsorship + Prev2_Online.Marketing + Prev2_SEM + 
                  Prev2_Other + Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_5)
vif(dlag_cam_5)  

dlag_cam_6 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                  Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFilter + product_analytic_verticalFlash + 
                  Prev_units + Prev2_units + Prev_tot_cust + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other +
                  Prev2_Sponsorship + Prev2_Online.Marketing + Prev2_SEM + 
                  Prev2_Other + Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_6)
vif(dlag_cam_6) 
  
dlag_cam_7 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + TV + Digital + 
                  Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFlash + 
                  Prev_units + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing + Prev_Online.Marketing + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other +
                  Prev2_Sponsorship  + Prev2_SEM + 
                  Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_7)
vif(dlag_cam_7) 

dlag_cam_8 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFlash + 
                  Prev_units + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing  + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other +
                  Prev2_Sponsorship  + Prev2_SEM + 
                  Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_8)
vif(dlag_cam_8) 

dlag_cam_9 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFlash + 
                  Prev_units + Prev2_tot_cust + 
                  Prev2_tot_list_price + Prev_Digital + Prev_Sponsorship + 
                  Prev_Content.Marketing  + Prev_Affiliates + 
                  Prev_SEM + Prev_Radio + Prev_Other +
                  Prev2_Sponsorship  + Prev2_SEM + 
                  Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_TVadStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_9)
vif(dlag_cam_9) 

dlag_cam_10 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  Radio + Other + Frequency + TV_adStock + Digital_adStock + 
                  ContentMarketing_adStock + OnlineMarketing_adStock + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                  product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                  product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                  product_analytic_verticalFlash + 
                  Prev_units + Prev2_tot_cust + 
                  Prev2_tot_list_price + 
                  Prev_Affiliates + 
                  Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                  Prev_OnlineMarketing_adStock + Prev2_ContentMarketing_adStock + 
                  Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_10)
vif(dlag_cam_10) 

dlag_cam_11 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                   mean_list_price + payment_prepaid + payment_cod + 
                   Frequency + TV_adStock + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryGrip + 
                   product_analytic_verticalCameraEyeCup + product_analytic_verticalCameraMicrophone + 
                   product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                   product_analytic_verticalFlash + 
                   Prev_units + Prev2_tot_cust + 
                   Prev2_tot_list_price + 
                   Prev_Affiliates + 
                   Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                   Prev_OnlineMarketing_adStock + Prev2_ContentMarketing_adStock + 
                   Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_11)
vif(dlag_cam_11) 

dlag_cam_12 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                   mean_list_price + payment_prepaid + payment_cod + 
                   Frequency + 
                   product_analytic_verticalCameraBattery + 
                   product_analytic_verticalExtensionTube + 
                   Prev_units + Prev2_tot_cust + 
                   Prev2_tot_list_price + 
                   Prev_Affiliates + 
                   Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                   Prev_OnlineMarketing_adStock + Prev2_ContentMarketing_adStock + 
                   Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_12)
vif(dlag_cam_12) 

dlag_cam_13 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                   mean_list_price + payment_prepaid + payment_cod + 
                   product_analytic_verticalCameraBattery + 
                   Prev_Digital_adStock + Prev_ContentMarketing_adStock + 
                   Prev_OnlineMarketing_adStock + Prev2_ContentMarketing_adStock + 
                   Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_13)
vif(dlag_cam_13) 

dlag_cam_14 = lm(formula = sum_gmv ~ mean_gmv + units + tot_list_price + 
                   mean_list_price + payment_prepaid + payment_cod + 
                   product_analytic_verticalCameraBattery + 
                   Prev2_OnlineMarketing_adStock, data = train_dlag_camaccessory)

summary(dlag_cam_14)
vif(dlag_cam_14) 

dlag_cam_15 = lm(formula = sum_gmv ~ tot_list_price + 
                   payment_prepaid + payment_cod + 
                   product_analytic_verticalCameraBattery,
                   data = train_dlag_camaccessory)

summary(dlag_cam_15)
vif(dlag_cam_15)

###Prediction
predict_dlag_ca <- predict(dlag_cam_15,test_dlag_camaccessory[,-1])
test_dlag_camaccessory$test_gmv <- predict_dlag_ca
r <- cor(test_dlag_camaccessory$sum_gmv,test_dlag_camaccessory$test_gmv)
rsq <- cor(test_dlag_camaccessory$sum_gmv,test_dlag_camaccessory$test_gmv)^2
rsq


# Performing 5-fold cross validation
fit.lm_dlag_ca <- train(sum_gmv~., data=train_dlag_camaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_dlag_ca)
#R square - .999959

fit.lm_dlag_ca$finalModel

##prediction using cv model
predict_dlag_ca1 <- predict(fit.lm_dlag_ca,test_dlag_camaccessory[,-1])

test_dlag_camaccessory$test_gmv1 <- predict_dlag_ca1
r_dlagca_cv <- cor(test_dlag_camaccessory$sum_gmv,test_dlag_camaccessory$test_gmv1)
rsq_dlagca_cv <- cor(test_dlag_camaccessory$sum_gmv,test_dlag_camaccessory$test_gmv1)^2
rsq_dlagca_cv

# .9999336

############### Calculate Elasticity ##############################
elas_dlag_cam <- calculate_elasticity(dlag_cam_15,dlag_CameraAccessory)
plot_elasticity(elas_dlag_cam,"Camera Accessory DLAG Model")

######################## HomeAudio ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
dlag_HomeAudio <- subset(dlag_HomeAudio,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(dlag_HomeAudio))

#Remove NA values
dlag_HomeAudio <- na.omit(dlag_HomeAudio)

#Setting seed value
set.seed(100)

#Divide data into train and test data
dlag_train_indices <- sample(1:nrow(dlag_HomeAudio),0.7*nrow(dlag_HomeAudio))
train_dlag_homeaudio <- dlag_HomeAudio[dlag_train_indices,]
test_dlag_homeaudio <- dlag_HomeAudio[-dlag_train_indices,]

dlag_ha_1 <- lm(sum_gmv~.,data=train_dlag_homeaudio)
summary(dlag_ha_1)

dlag_ha_2 <- stepAIC(dlag_ha_1, direction = "both")
summary(dlag_ha_2)

dlag_ha_3 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_customers + 
                 tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                 Affiliates + SEM + Radio + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeRepublic.Day + 
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                 product_analytic_verticalHomeAudioSpeaker + Prev2_GMV + Prev_units + 
                 Prev2_units + Prev_tot_list_price + Prev2_tot_list_price + 
                 Prev_Sponsorship + Prev_Content.Marketing + Prev2_TV  + 
                 Prev_ContentMarketing_adStock + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_3)
vif(dlag_ha_3)


dlag_ha_4 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + tot_customers + 
                 tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                 Affiliates + SEM + Radio + Frequency + TV_adStock + Digital_adStock + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeRepublic.Day + 
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalHiFiSystem + 
                 product_analytic_verticalHomeAudioSpeaker + Prev2_GMV + 
                 Prev2_units + Prev2_tot_list_price + 
                 Prev_Sponsorship + Prev2_TV  + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_4)
vif(dlag_ha_4)


dlag_ha_5 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + 
                 tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 ContentMarketing_adStock + OnlineMarketing_adStock + sale_typeRepublic.Day + 
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock + product_analytic_verticalDockingStation + 
                 product_analytic_verticalHiFiSystem + 
                 product_analytic_verticalHomeAudioSpeaker + Prev2_GMV + 
                 Prev2_units + Prev2_tot_list_price + 
                 Prev_Sponsorship + Prev2_TV  + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_5)
vif(dlag_ha_5)

dlag_ha_6 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + 
                 tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 sale_typeRepublic.Day + 
                 Prev_Sponsorship + Prev2_TV  + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_6)
vif(dlag_ha_6)

dlag_ha_7 = lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + 
                 tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 sale_typeRepublic.Day + 
                 Prev2_TV  + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_7)
vif(dlag_ha_7)

dlag_ha_8 = lm(formula = sum_gmv ~ mean_gmv + sum_mrp + 
               tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                 sale_typeRepublic.Day + 
                 Prev2_TV  + 
                 Prev2_TVadStock + Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_8)
vif(dlag_ha_8)

dlag_ha_9 = lm(formula = sum_gmv ~ mean_list_price + payment_prepaid + payment_cod + 
                 Prev2_OnlineMarketing_adStock, 
               data = train_dlag_homeaudio)

summary(dlag_ha_9)
vif(dlag_ha_9)

###Prediction
predict_dlag_ha <- predict(dlag_ha_9,test_dlag_homeaudio[,-1])
test_dlag_homeaudio$test_gmv <- predict_dlag_ha
r <- cor(test_dlag_homeaudio$sum_gmv,test_dlag_homeaudio$test_gmv)
rsq <- cor(test_dlag_homeaudio$sum_gmv,test_dlag_homeaudio$test_gmv)^2
rsq
#0.968

# Performing 5-fold cross validation
fit.lm_dlag_ha <- train(sum_gmv~., data=train_dlag_homeaudio, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_dlag_ha)
#R square - .9994167

fit.lm_dlag_ha$finalModel

##prediction using cv model
predict_dlag_ha1 <- predict(fit.lm_dlag_ha,test_dlag_homeaudio[,-1])
test_dlag_homeaudio$test_gmv1 <- predict_dlag_ha1
r_dlag_ha <- cor(test_dlag_homeaudio$sum_gmv,test_dlag_homeaudio$test_gmv1)
rsq_dlag_ha <- cor(test_dlag_homeaudio$sum_gmv,test_dlag_homeaudio$test_gmv1)^2
rsq_dlag_ha

# .9999901


############### Calculate Elasticity ##############################
elas_dlag_ha <- calculate_elasticity(dlag_ha_9,dlag_HomeAudio)
plot_elasticity(elas_dlag_ha,"Home Audio DLAG Model")


######################## GamingAccessory ########################

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
dlag_GamingAccessory <- subset(dlag_GamingAccessory,select=-c(Month,Year,weekOrder_no))

#Check for NA values
which(is.na(dlag_GamingAccessory))

#Remove NA values
dlag_GamingAccessory <- na.omit(dlag_GamingAccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
dlag_train_indices <- sample(1:nrow(dlag_GamingAccessory),0.7*nrow(dlag_GamingAccessory))
train_dlag_gamingaccessory <- dlag_GamingAccessory[dlag_train_indices,]
test_dlag_gamingaccessory <- dlag_GamingAccessory[-dlag_train_indices,]

dlag_ga_1 <- lm(sum_gmv~.,data=train_dlag_gamingaccessory)
summary(dlag_ga_1)

dlag_ga_2 <- stepAIC(dlag_ga_1, direction = "both")
summary(dlag_ga_2)
vif(dlag_ga_2)

dlag_ga_3 <- lm(formula = sum_gmv ~ sum_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                  TV + Sponsorship + SEM + TV_adStock + Digital_adStock + ContentMarketing_adStock + 
                  OnlineMarketing_adStock + sale_typeEid...Rathayatra.sale + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse + 
                  Prev_sum_mrp + Prev_TV + sale_typeIndependence.Sale + sale_typeNo.Sale, 
                data = train_dlag_gamingaccessory)
summary(dlag_ga_3)
vif(dlag_ga_3)


dlag_ga_4 <- lm(formula = sum_gmv ~ sum_mrp + tot_customers + 
                  tot_list_price + mean_list_price + payment_prepaid + payment_cod + 
                  Sponsorship + SEM + Digital_adStock + ContentMarketing_adStock + 
                  OnlineMarketing_adStock + sale_typeEid...Rathayatra.sale + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse + 
                  Prev_sum_mrp + Prev_TV + sale_typeIndependence.Sale + sale_typeNo.Sale, 
                data = train_dlag_gamingaccessory)
summary(dlag_ga_4)
vif(dlag_ga_4)

dlag_ga_5 <- lm(formula = sum_gmv ~ sum_mrp + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  SEM + Digital_adStock + ContentMarketing_adStock + 
                  OnlineMarketing_adStock + sale_typeEid...Rathayatra.sale + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse + 
                  Prev_sum_mrp + Prev_TV + sale_typeIndependence.Sale + sale_typeNo.Sale, 
                data = train_dlag_gamingaccessory)
summary(dlag_ga_5)
vif(dlag_ga_5)

dlag_ga_6 <- lm(formula = sum_gmv ~ sum_mrp + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  SEM + Digital_adStock + 
                  OnlineMarketing_adStock + sale_typeEid...Rathayatra.sale + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse + 
                  Prev_sum_mrp + Prev_TV + sale_typeIndependence.Sale + sale_typeNo.Sale, 
                data = train_dlag_gamingaccessory)
summary(dlag_ga_6)
vif(dlag_ga_6)

dlag_ga_7 <- lm(formula = sum_gmv ~ sum_mrp + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  OnlineMarketing_adStock + sale_typeEid...Rathayatra.sale + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse + 
                  Prev_sum_mrp + Prev_TV + sale_typeIndependence.Sale + sale_typeNo.Sale, 
                data = train_dlag_gamingaccessory)
summary(dlag_ga_7)
vif(dlag_ga_7)

dlag_ga_8 <- lm(formula = sum_gmv ~ sum_mrp + 
                  mean_list_price + payment_prepaid + payment_cod + 
                  OnlineMarketing_adStock  + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse,
                data = train_dlag_gamingaccessory)
summary(dlag_ga_8)
vif(dlag_ga_8)

dlag_ga_9 <- lm(formula = sum_gmv ~ mean_list_price + payment_prepaid + payment_cod + 
                  OnlineMarketing_adStock  + 
                  product_analytic_verticalGamingHeadset + product_analytic_verticalGamingMouse,
                data = train_dlag_gamingaccessory)
summary(dlag_ga_9)
vif(dlag_ga_9)

###Prediction
dlag_ga_pred <- predict(dlag_ga_9, test_dlag_gamingaccessory[,-1])
test_dlag_gamingaccessory$gmv_pred <- dlag_ga_pred

dlag_ga_r <- cor(test_dlag_gamingaccessory$sum_gmv, test_dlag_gamingaccessory$gmv_pred)
dlag_ga_r_sq <- dlag_ga_r^2
dlag_ga_r_sq   
# 0.859

# Performing 5-fold cross validation
fit.lm_dlag_ga <- train(sum_gmv~., data=train_dlag_gamingaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_dlag_ga)
#R square - .9997757

fit.lm_dlag_ga$finalModel

##prediction using cv 
dlag_ga_cv <- predict(fit.lm_dlag_ga, test_dlag_gamingaccessory[,-1])
test_dlag_gamingaccessory$gmv_pred1 <- dlag_ga_cv

dlag_ga_r_cv1 <- cor(test_dlag_gamingaccessory$sum_gmv, test_dlag_gamingaccessory$gmv_pred1)
dlag_ga_r_sq_cv1 <- dlag_ga_r_cv1^2
dlag_ga_r_sq_cv1   
##0.999609

############### Calculate Elasticity ##############################
elas_dlag_ga <- calculate_elasticity(dlag_ga_8,dlag_GamingAccessory)
plot_elasticity(elas_dlag_ga,"Game Accessory DLAG Model")
