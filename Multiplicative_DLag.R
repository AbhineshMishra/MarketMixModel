########################################################################################################
######################## Combination of Multiplicative & Distributed Lag Model ########################
#######################################################################################################

########################## Prepare Data ###########################

#colnames(consolidated5)

mm_dlag_consolidated <- consolidated5 %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_GMV=lag(sum_gmv)) %>% mutate(Prev_mean_gmv=lag(mean_gmv)) %>% mutate(Prev2_GMV=lag(sum_gmv,2)) %>% mutate(Prev2_mean_gmv=lag(mean_gmv,2))
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_units=lag(units)) %>% mutate(Prev2_units=lag(units,2))
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_sum_mrp=lag(sum_mrp)) %>% mutate(Prev_mean_mrp=lag(mean_mrp)) %>% mutate(Prev2_sum_mrp=lag(sum_mrp,2)) %>% mutate(Prev2_mean_mrp=lag(mean_mrp,2))
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_cust=lag(tot_customers)) %>% mutate(Prev2_tot_cust=lag(tot_customers,2))
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_tot_list_price=lag(tot_list_price)) %>% mutate(Prev_mean_list_price=lag(mean_list_price)) %>% mutate(Prev2_tot_list_price=lag(tot_list_price,2)) %>% mutate(Prev2_mean_list_price=lag(mean_list_price,2))
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TV=lag(TV)) %>% mutate(Prev_Digital=lag(Digital)) %>% mutate(Prev_Sponsorship=lag(Sponsorship)) %>% mutate(Prev_Content.Marketing=lag(Content.Marketing)) %>% mutate(Prev_Online.Marketing=lag(Online.marketing)) %>% mutate(Prev_Affiliates=lag(Affiliates)) %>% mutate(Prev_SEM=lag(SEM)) %>% mutate(Prev_Radio=lag(Radio)) %>% mutate(Prev_Other=lag(Other)) %>% mutate(Prev2_TV=lag(TV,2)) %>% mutate(Prev2_Digital=lag(Digital,2)) %>% mutate(Prev2_Sponsorship=lag(Sponsorship,2)) %>% mutate(Prev2_Content.Marketing=lag(Content.Marketing,2)) %>% mutate(Prev2_Online.Marketing=lag(Online.marketing,2)) %>% mutate(Prev2_Affiliates=lag(Affiliates,2)) %>% mutate(Prev2_SEM=lag(SEM,2)) %>% mutate(Prev2_Radio=lag(Radio,2)) %>% mutate(Prev2_Other=lag(Other,2))  
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_TVadStock=lag(TV_adStock)) %>% mutate(Prev_Digital_adStock=lag(Digital_adStock)) %>% mutate(Prev_ContentMarketing_adStock=lag(ContentMarketing_adStock)) %>% mutate(Prev_OnlineMarketing_adStock=lag(OnlineMarketing_adStock)) %>% mutate(Prev_Affiliates_adStock=lag(Affiliates_adStock)) %>% mutate(Prev_SEM_adStock=lag(SEM_adStock)) %>% mutate(Prev_Radio_adStock=lag(Radio_adStock)) %>% mutate(Prev_Other_adStock=lag(Other_adStock)) %>% mutate(Prev2_TVadStock=lag(TV_adStock,2)) %>% mutate(Prev2_Digital_adStock=lag(Digital_adStock,2)) %>% mutate(Prev2_ContentMarketing_adStock=lag(ContentMarketing_adStock,2)) %>% mutate(Prev2_OnlineMarketing_adStock=lag(OnlineMarketing_adStock,2)) %>% mutate(Prev2_Affiliates_adStock=lag(Affiliates_adStock,2)) %>% mutate(Prev2_SEM_adStock=lag(SEM_adStock,2)) %>% mutate(Prev2_Radio_adStock=lag(Radio_adStock,2)) %>% mutate(Prev2_Other_adStock=lag(Other_adStock,2))  
mm_dlag_consolidated <- mm_dlag_consolidated %>% arrange(weekOrder_no) %>% group_by(product_analytic_sub_category) %>% mutate(Prev_NPS=lag(NPS)) %>% mutate(Prev2_NPS=lag(NPS,2))
#View(mm_dlag_consolidated)

#Divide the master data frame into three categories
mm_dlag_CameraAccessory <- mm_dlag_consolidated[which(mm_dlag_consolidated$product_analytic_sub_category=="CameraAccessory"),]
mm_dlag_CameraAccessory <- subset(mm_dlag_CameraAccessory,select = -c(product_analytic_sub_category))
#View(mm_dlag_CameraAccessory)

mm_dlag_HomeAudio <- mm_dlag_consolidated[which(mm_dlag_consolidated$product_analytic_sub_category=="HomeAudio"),]
mm_dlag_HomeAudio <- subset(mm_dlag_HomeAudio,select = -c(product_analytic_sub_category))
#View(mm_dlag_HomeAudio)

mm_dlag_GamingAccessory <- mm_dlag_consolidated[which(mm_dlag_consolidated$product_analytic_sub_category=="GamingAccessory"),]
mm_dlag_GamingAccessory <- subset(mm_dlag_GamingAccessory,select = -c(product_analytic_sub_category))
#View(mm_dlag_GamingAccessory)


######################## CameraAccessory ########################

mm_dlag_camaccessory <- mm_dlag_CameraAccessory
#Log(0) is not defined. Set all 0's to 0.001
mm_dlag_camaccessory[mm_dlag_camaccessory==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_dlag_camaccessory <- subset(mm_dlag_camaccessory,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_dlag_camaccessory$NPS <- as.numeric(mm_dlag_camaccessory$NPS)

#Take log of all numeric values
mm_dlag_camaccessory <- log(mm_dlag_camaccessory[,1:ncol(mm_dlag_camaccessory)])

#Check for NA values
which(is.na(mm_dlag_camaccessory))

#Remove NA values
mm_dlag_camaccessory <- na.omit(mm_dlag_camaccessory)

#Setting seed value
set.seed(100)

#Divide data into train and test data
mm_dlag_train_indices <- sample(1:nrow(mm_dlag_camaccessory),0.7*nrow(mm_dlag_camaccessory))
train_mm_dlag_camaccessory <- mm_dlag_camaccessory[mm_dlag_train_indices,]
test_mm_dlag_camaccessory <- mm_dlag_camaccessory[-mm_dlag_train_indices,]

mm_dlag_cam_1 <- lm(sum_gmv~.,data=train_mm_dlag_camaccessory)
summary(mm_dlag_cam_1)

mm_dlag_cam_2 <- stepAIC(mm_dlag_cam_1, direction = "both")
summary(mm_dlag_cam_2)

mm_dlag_cam_3 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + mean_mrp + 
                      tot_customers + tot_list_price + mean_list_price + Frequency + TV_adStock + 
                      Digital_adStock + ContentMarketing_adStock + OnlineMarketing_adStock + 
                      Affiliates_adStock + SEM_adStock + Radio_adStock + Other_adStock + 
                      Prev_Digital_adStock + Prev_ContentMarketing_adStock + Prev_OnlineMarketing_adStock + 
                      Prev_Affiliates_adStock + Prev_SEM_adStock + Prev_Radio_adStock + 
                      Prev_Other_adStock + Prev2_TVadStock + Prev2_Digital_adStock + 
                      Prev2_ContentMarketing_adStock, data = train_mm_dlag_camaccessory)

summary(mm_dlag_cam_3)
vif(mm_dlag_cam_3)

mm_dlag_cam_4 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + mean_mrp + 
                      tot_customers + 
                    Prev2_ContentMarketing_adStock, data = train_mm_dlag_camaccessory)

summary(mm_dlag_cam_4)
vif(mm_dlag_cam_4)

mm_dlag_cam_5 <- lm(formula = sum_gmv ~ sum_mrp ,
                      data = train_mm_dlag_camaccessory)

summary(mm_dlag_cam_5)
vif(mm_dlag_cam_5)

#Prediction
predict_mm_dlag_ca <- predict(mm_dlag_cam_5,test_mm_dlag_camaccessory[,-1])

test_mm_dlag_camaccessory$test_gmv <- predict_mm_dlag_ca
r <- cor(test_mm_dlag_camaccessory$sum_gmv,test_mm_dlag_camaccessory$test_gmv)
rsq <- cor(test_mm_dlag_camaccessory$sum_gmv,test_mm_dlag_camaccessory$test_gmv)^2
rsq
#.984


# Performing 5-fold cross validation
fit.lm_mmdlag_ca <- train(sum_gmv~., data=train_mm_dlag_camaccessory, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mmdlag_ca)
#R square - 1

fit.lm_mmdlag_ca$finalModel

##prediction using cv 
mmdlag_ca_cv <- predict(fit.lm_mmdlag_ca, test_mm_dlag_camaccessory[,-1])
test_mm_dlag_camaccessory$gmv_pred1 <- mmdlag_ca_cv

mmdlag_ca_r_cv1 <- cor(test_mm_dlag_camaccessory$sum_gmv, test_mm_dlag_camaccessory$gmv_pred1)
mmdlag_ca_r_sq_cv1 <- mmdlag_ca_r_cv1^2
mmdlag_ca_r_sq_cv1   

#1


############### Calculate Elasticity ##############################
elas_mm_dlag_ca <- calculate_elasticity(mm_dlag_cam_5,mm_dlag_camaccessory)
plot_elasticity(elas_mm_dlag_ca,"Camera Accessory MM + DLAG Model")


######################## HomeAudio ########################
mm_dlag_homeaudio <- mm_dlag_HomeAudio
#View(mm_dlag_homeaudio)

#Log(0) is not defined. Set all 0's to 0.001
mm_dlag_homeaudio[mm_dlag_homeaudio==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_dlag_homeaudio <- subset(mm_dlag_homeaudio,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_dlag_homeaudio$NPS <- as.numeric(mm_dlag_homeaudio$NPS)

#Take log of all numeric values
mm_dlag_homeaudio <- log(mm_dlag_homeaudio[,1:ncol(mm_dlag_homeaudio)])

#Check for NA values
which(is.na(mm_dlag_homeaudio))

#Remove NA values
mm_dlag_homeaudio <- na.omit(mm_dlag_homeaudio)

#setting the seed
set.seed(100)

mm_dlag_ha_indices <- sample(1:nrow(mm_dlag_homeaudio),0.7*nrow(mm_dlag_homeaudio))

train_mm_dlag_ha <- mm_dlag_homeaudio[mm_dlag_ha_indices,]
test_mm_dlag_ha <- mm_dlag_homeaudio[-mm_dlag_ha_indices,]

ha_mm_dlag_1 <- lm(sum_gmv~.,data=train_mm_dlag_ha)
summary(ha_mm_dlag_1)

ha_mm_dlag_2 <- stepAIC(ha_mm_dlag_1, direction = "both")
summary(ha_mm_dlag_2)

ha_mm_dlag_3 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + mean_mrp + 
                     tot_customers + tot_list_price + mean_list_price + promotion + 
                     payment_prepaid + payment_cod + mean_delivery_status + TV + 
                     Digital + Sponsorship + Content.Marketing + Online.marketing + 
                     Affiliates + SEM + Radio + Other + Frequency + TV_adStock + 
                     Digital_adStock + ContentMarketing_adStock + OnlineMarketing_adStock + 
                     Affiliates_adStock + SEM_adStock + Radio_adStock + Other_adStock + 
                     Prev2_Affiliates_adStock + Prev2_SEM_adStock, data = train_mm_dlag_ha)
summary(ha_mm_dlag_3)

ha_mm_dlag_4 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + mean_mrp + 
                     tot_customers + tot_list_price + mean_list_price + promotion + 
                     Prev2_SEM_adStock, data = train_mm_dlag_ha)
summary(ha_mm_dlag_4)
vif(ha_mm_dlag_4)

ha_mm_dlag_5 <- lm(formula = sum_gmv ~ mean_gmv + sum_mrp + mean_mrp ,
                     data = train_mm_dlag_ha)
summary(ha_mm_dlag_5)
vif(ha_mm_dlag_5)

ha_mm_dlag_6 <- lm(formula = sum_gmv ~ sum_mrp + mean_mrp ,
                   data = train_mm_dlag_ha)
summary(ha_mm_dlag_6)
vif(ha_mm_dlag_6)

#Prediction
predict_mm_dlag_ha <- predict(ha_mm_dlag_6,test_mm_dlag_ha[,-1])

test_mm_dlag_ha$test_gmv <- predict_mm_dlag_ha
r <- cor(test_mm_dlag_ha$sum_gmv,test_mm_dlag_ha$test_gmv)
rsq <- cor(test_mm_dlag_ha$sum_gmv,test_mm_dlag_ha$test_gmv)^2
rsq
#.986

# Performing 5-fold cross validation
fit.lm_mmdlag_ha <- train(sum_gmv~., data=train_mm_dlag_ha, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mmdlag_ha)
#R square 1

fit.lm_mmdlag_ha$finalModel

##prediction using cv 
mmdlag_ha_cv <- predict(fit.lm_mmdlag_ha, test_mm_dlag_ha[,-1])
test_mm_dlag_ha$gmv_pred1 <- mmdlag_ha_cv

mmdlag_ha_r_cv1 <- cor(test_mm_dlag_ha$sum_gmv, test_mm_dlag_ha$gmv_pred1)
mmdlag_ha_r_sq_cv1 <- mmdlag_ha_r_cv1^2
mmdlag_ha_r_sq_cv1   

#1


############### Calculate Elasticity ##############################
elas_mm_dlag_ha <- calculate_elasticity(ha_mm_dlag_6,mm_dlag_HomeAudio)
plot_elasticity(elas_mm_dlag_ha,"Home Audio MM + DLAG Model")

######################## GamingAccessory ########################

mm_dlag_ga <- mm_dlag_GamingAccessory
#View(mm_dlag_ga)

#Log(0) is not defined. Set all 0's to 0.001
mm_dlag_ga[mm_dlag_ga==0] <- 0.001

#Remove Month, Year and weekOrder_no columns
#As their log will not yield any result
mm_dlag_ga <- subset(mm_dlag_ga,select=-c(Month,Year,weekOrder_no))

#Convert NPS score to numeric
mm_dlag_ga$NPS <- as.numeric(mm_dlag_ga$NPS)

#Take log of all numeric values
mm_dlag_ga <- log(mm_dlag_ga[,1:ncol(mm_dlag_ga)])

#Check for NA values
which(is.na(mm_dlag_ga))

#Remove NA values
mm_dlag_ga <- na.omit(mm_dlag_ga)

#setting the seed
set.seed(100)
mm_dlag_ga_indices <- sample(1:nrow(mm_dlag_ga),0.7*nrow(mm_dlag_ga))

train_mm_dlag_ga <- mm_dlag_ga[mm_dlag_ga_indices,]
test_mm_dlag_ga <- mm_dlag_ga[-mm_dlag_ga_indices,]

mm_dlag_ga_1 <- lm(sum_gmv~.,data=train_mm_dlag_ga)
summary(ga_model_1)

mm_dlag_ga_2 <- stepAIC(mm_dlag_ga_1, direction = "both")
summary(mm_dlag_ga_2)
#vif(mm_dlag_ga_2)

#After removing all variables with high VIF or low significance
mm_dlag_ga_3 <- lm(formula = sum_gmv ~ mean_gmv + units + sum_mrp + mean_mrp + 
                     tot_customers + tot_list_price + mean_list_price + promotion + 
                     payment_prepaid + payment_cod + mean_delivery_status + TV + 
                     Prev2_Other_adStock + Prev_NPS, data = train_mm_dlag_ga)

summary(mm_dlag_ga_3)

mm_dlag_ga_4 <- lm(formula = sum_gmv ~ units + sum_mrp + mean_mrp + 
                     tot_customers + mean_list_price + promotion + 
                     payment_prepaid + payment_cod + mean_delivery_status + TV + 
                     Prev2_Other_adStock + Prev_NPS, data = train_mm_dlag_ga)

summary(mm_dlag_ga_4)
vif(mm_dlag_ga_4)

mm_dlag_ga_5 <- lm(formula = sum_gmv ~ units + sum_mrp + mean_mrp + 
                     mean_list_price +
                     Prev2_Other_adStock + Prev_NPS, data = train_mm_dlag_ga)

summary(mm_dlag_ga_5)
vif(mm_dlag_ga_5)

mm_dlag_ga_6 <- lm(formula = sum_gmv ~ units + sum_mrp + mean_mrp + 
                     mean_list_price,
                     data = train_mm_dlag_ga)

summary(mm_dlag_ga_6)
vif(mm_dlag_ga_6)

mm_dlag_ga_7 <- lm(formula = sum_gmv ~ sum_mrp  + 
                     mean_list_price,
                   data = train_mm_dlag_ga)

summary(mm_dlag_ga_7)
vif(mm_dlag_ga_7)

#Prediction
predict_mm_dlag_ga <- predict(mm_dlag_ga_7,test_mm_dlag_ga[,-1])

test_mm_dlag_ga$test_gmv <- predict_mm_dlag_ga
r <- cor(test_mm_dlag_ga$sum_gmv,test_mm_dlag_ga$test_gmv)
rsq <- cor(test_mm_dlag_ga$sum_gmv,test_mm_dlag_ga$test_gmv)^2
rsq
#.962


# Performing 5-fold cross validation
fit.lm_mmdlag_ga <- train(sum_gmv~., data=train_mm_dlag_ga, method="lm",  trControl=trainControl)

# Printing cross validation result
print(fit.lm_mmdlag_ga)
#R square 1

fit.lm_mmdlag_ga$finalModel

##prediction using cv 
mmdlag_ga_cv <- predict(fit.lm_mmdlag_ga, test_mm_dlag_ga[,-1])
test_mm_dlag_ga$gmv_pred1 <- mmdlag_ga_cv

mmdlag_ga_r_cv1 <- cor(test_mm_dlag_ga$sum_gmv, test_mm_dlag_ga$gmv_pred1)
mmdlag_ga_r_sq_cv1 <- mmdlag_ga_r_cv1^2
mmdlag_ga_r_sq_cv1   

#1


############### Calculate Elasticity ##############################
elas_mm_dlag_ga <- calculate_elasticity(mm_dlag_ga_7,mm_dlag_GamingAccessory)
plot_elasticity(elas_mm_dlag_ga,"Game Accessory MM + DLAG Model")

