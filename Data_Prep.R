## Set the working directory here
setwd("~/Documents/IIIT/Capstone")

######################## Include/Install packages ########################
include_packages <- c("dplyr","tidyr","lubridate","ggplot2","tibble","readxl","caret","MASS","car","stringr","BBmisc","gdata","DAAG")

for (pckgs in include_packages) {
  tryCatch({
    print(paste("Loading library", pckgs))
    library(pckgs,character.only = TRUE)
  }, error = function(err) {
    print(paste("Package not present. Installing package", pckgs))
    install.packages(pckgs,character.only = TRUE)
    library(pckgs,character.only = TRUE)
  })
}

######################## Read Consumeer Electronics Data ########################
consumerElectronics <- read.csv("ConsumerElectronics.csv",stringsAsFactors = F)
nrow(consumerElectronics)
#View(consumerElectronics)
consumerElectronics$order_date <- as.POSIXct(consumerElectronics$order_date)

#Change the name of s1_fact.order_payment_type to order_payment_type
colnames(consumerElectronics)[colnames(consumerElectronics) == "s1_fact.order_payment_type"] <- "order_payment_type"

#Verify the min and max dates
min(consumerElectronics$order_date) #2015-05-19 13:42:09 IST
max(consumerElectronics$order_date) #2016-07-25 01:19:45 IST

#Select Data between 1st July 2015 to 1st July 2016
consumerElectronics <- 
  consumerElectronics[which(consumerElectronics$order_date>=as.POSIXct("2015-07-01 00:00:00") & consumerElectronics$order_date<as.POSIXct("2016-07-01 00:00:00")),] 
nrow(consumerElectronics) #1648215

#Omit NA values
consumerElectronics <- na.omit(consumerElectronics)
nrow(consumerElectronics) #1643311  #4904 values removed

#Remove datapoints where GMV ==0 or GMV < 0
nrow(filter(consumerElectronics,consumerElectronics$gmv==0)) #1349
nrow(filter(consumerElectronics,consumerElectronics$gmv<0))  #0
consumerElectronics <- subset(consumerElectronics,consumerElectronics$gmv > 0)
nrow(consumerElectronics) #1641962

#Exploring datapoint where units ordered are 0 or less than 0
nrow(filter(consumerElectronics,consumerElectronics$units ==0)) #0
nrow(filter(consumerElectronics,consumerElectronics$units <0))  #0

#Exploring values of deliverybdays
unique(consumerElectronics$deliverybdays)
#If deliverybdays < 0, then assign 0 to it.
consumerElectronics$deliverybdays <- ifelse(consumerElectronics$deliverybdays < 0 ,0,consumerElectronics$deliverybdays)
consumerElectronics$deliverybdays <- as.numeric(consumerElectronics$deliverybdays)

#Exploring values of deliverycdays
unique(consumerElectronics$deliverycdays)
#If deliverycdays < 0, then assign 0 to it.
consumerElectronics$deliverycdays <- ifelse(consumerElectronics$deliverycdays < 0 ,0,consumerElectronics$deliverycdays)
consumerElectronics$deliverycdays <- as.numeric(consumerElectronics$deliverycdays)

#Find order payment type
unique(consumerElectronics$order_payment_type) #COD and #Prepaid

#Find unique values of sla
unique(consumerElectronics$sla) 
#Few Outliers at 1004 and 1006 

#Exploring values of cust_ID
unique(consumerElectronics$cust_id)
#Lot of negative values. CUST_ID will give no valuable
#insight in any mode. Better to get rid of this column
#consumerElectronics <- subset(consumerElectronics,select = -c(cust_id))

#Pin code wont give any valuable insight in any mode
#Better to get rid of it
consumerElectronics <- subset(consumerElectronics,select = -c(pincode))

#Exploring values of product_analytic_super_category
unique(consumerElectronics$product_analytic_super_category) #CE

#Exploring values of product_analyticcategory
unique(consumerElectronics$product_analytic_category) 
#5 categories

#Exploring values of product_analytic_sub_category
unique(consumerElectronics$product_analytic_sub_category)
length(unique(consumerElectronics$product_analytic_sub_category)) 
#14 categories

#Exploring values of product_analytic_vertical
unique(consumerElectronics$product_analytic_vertical)
length(unique(consumerElectronics$product_analytic_vertical))
#74 values

#Exploring product mrp
nrow(filter(consumerElectronics,consumerElectronics$product_mrp <= 0))

#Remove the datapoints with MRP <= 0 
consumerElectronics <- subset(consumerElectronics,consumerElectronics$product_mrp > 0)

#Total number of rowns now
nrow(consumerElectronics)  #1637036

#Exploring procurement SLA
nrow(filter(consumerElectronics,consumerElectronics$product_procurement_sla < 0))
#70306 values

#Set all the negative SLAs to 0
consumerElectronics$product_procurement_sla <- ifelse(consumerElectronics$product_procurement_sla < 0,0,consumerElectronics$product_procurement_sla)

#Create week order date
#weekOrder_date <- floor_date(consumerElectronics$order_date, unit="week")
#consumerElectronics <- cbind(consumerElectronics,weekOrder_date)

#Get week number from the weekOrder_Date
consumerElectronics$weekOrder_no <- strftime( consumerElectronics$order_date ,format="%V")

#Starting value of week number
consumerElectronics$weekOrder_no[which(consumerElectronics$order_date == (min(consumerElectronics$order_date)))]
#27

#Last value of week in 2015
consumerElectronics$weekOrder_no[which(consumerElectronics$order_date == max(consumerElectronics$order_date[which(consumerElectronics$Year==2015)]))]
#53

#Modify week number so as to start with week number 1
#Modify week numbers of 2016 so as to maintain the sequence of incrementing week number
#Add 26 to the week numbers of 2016
consumerElectronics$weekOrder_no <- as.numeric(consumerElectronics$weekOrder_no)
consumerElectronics$weekOrder_no <- ifelse(consumerElectronics$Year==2016,consumerElectronics$weekOrder_no+26,consumerElectronics$weekOrder_no)
#Subtract 26 from the week numbers of 2015
consumerElectronics$weekOrder_no <- ifelse(consumerElectronics$Year==2015,consumerElectronics$weekOrder_no-26,consumerElectronics$weekOrder_no)

#Remove order_date
consumerElectronics <- subset(consumerElectronics,select = -c(order_date))

##three product sub-categories  - camera accessory, home audio and gaming accessory.
summary(factor(consumerElectronics$product_analytic_sub_category))
consumerElectronics <- consumerElectronics[which(consumerElectronics$product_analytic_sub_category=='CameraAccessory' | consumerElectronics$product_analytic_sub_category=='HomeAudio' | consumerElectronics$product_analytic_sub_category=='GamingAccessory'),]

##Additional variables
#(1)
list_price = consumerElectronics$gmv/consumerElectronics$units
consumerElectronics <- cbind(consumerElectronics,list_price)

#(2)
Promotion <-((consumerElectronics$product_mrp-consumerElectronics$list_price)/consumerElectronics$product_mrp)*100
consumerElectronics <- cbind(consumerElectronics,Promotion)

#(3) Add delivery status variables. Whether it was on time or delayed
consumerElectronics$ds <- consumerElectronics$sla - (consumerElectronics$deliverybdays + consumerElectronics$deliverycdays)
consumerElectronics$delivery_status <- 1
consumerElectronics$delivery_status[which(consumerElectronics$ds < 0)] <- 0
# 0 = Delayed
# 1 = On or Before Time

#Remove tempprary column consumerElectronics$ds
consumerElectronics <- subset(consumerElectronics, select = -c(ds))

#Create dummy variable to type of payment
consumerElectronics$cod <- ifelse (consumerElectronics$order_payment_type =="COD", 1,0)
consumerElectronics$prepaid <- ifelse (consumerElectronics$order_payment_type =="Prepaid", 1,0)

# Group by based on weekly data
consumerElectronics_groups <- group_by(consumerElectronics,Year,Month,product_analytic_category,product_analytic_sub_category,product_analytic_vertical, weekOrder_no)

consumerElectronics_weekly <- summarise(consumerElectronics_groups, sum_gmv=sum(gmv),mean_gmv=mean(gmv),units=sum(units),sum_mrp=sum(product_mrp),mean_mrp=mean(product_mrp),tot_customers = length(unique(cust_id)),tot_list_price=sum(list_price),mean_list_price=mean(list_price),promotion=mean(Promotion),payment_prepaid=sum(prepaid),payment_cod=sum(cod),mean_delivery_status=mean(delivery_status))

#View(consumerElectronics_weekly)

###############  Exploratory Data Analysis  ########################
######analyzing gmv 
# histogram
hist(consumerElectronics_weekly$sum_gmv)

#box plot for gmv over categories
ggplot(consumerElectronics_weekly,aes(x=as.factor(product_analytic_category),y=units)) + geom_boxplot()

#gmv by sub category
ggplot(consumerElectronics_weekly,aes(x=as.factor(product_analytic_category),y=sum_gmv)) + geom_bar(stat="identity")

# gmv by payment type
ggplot(consumerElectronics,aes(x=order_payment_type,y=gmv)) + geom_bar(stat="identity")

# gmv by product sub category & week
ggplot(consumerElectronics_weekly,aes(x=weekOrder_no,y=sum_gmv,fill=product_analytic_sub_category)) + geom_bar(stat="identity")

#gmv vs units
ggplot(consumerElectronics_weekly,aes(x=units,y=sum_gmv)) + geom_point(aes(col=product_analytic_sub_category))

#### analyzing units
# histogram
hist(consumerElectronics_weekly$units)

# units by payment type
ggplot(consumerElectronics,aes(x=order_payment_type,y=units)) + geom_bar(stat="identity")

#units by product sub category
ggplot(consumerElectronics_weekly,aes(x=as.factor(product_analytic_category),y=units)) + geom_bar(stat="identity")

#units by product sub category & week
ggplot(consumerElectronics_weekly,aes(x=weekOrder_no,y=units,fill=product_analytic_sub_category)) + geom_bar(stat="identity")

# promotion % vs gmv
ggplot(consumerElectronics_weekly,aes(x=promotion,y=sum_gmv)) + geom_point(aes(col=product_analytic_sub_category))

#promotion % vs gmv for Camera accessory catgeory products
CameraAccessory_weekly <- subset(consumerElectronics_weekly,product_analytic_sub_category=="CameraAccessory")
ggplot(CameraAccessory_weekly,aes(x=promotion,y=sum_gmv)) + geom_point(aes(col=product_analytic_sub_category))+geom_smooth()+facet_wrap(~product_analytic_vertical)

#promotion % vs gmv for Home audio catgeory
HomeAudio_weekly <- subset(consumerElectronics_weekly,product_analytic_sub_category=="HomeAudio")
ggplot(HomeAudio_weekly,aes(x=promotion,y=sum_gmv)) + geom_point(aes(col=product_analytic_sub_category))+geom_smooth()+facet_wrap(~product_analytic_vertical)

#promotion % vs gmv for Gaming accessory catgeory
GamingAccesory_weekly <- subset(consumerElectronics_weekly,product_analytic_sub_category=="GamingAccessory")
ggplot(GamingAccesory_weekly,aes(x=promotion,y=sum_gmv)) + geom_point(aes(col=product_analytic_sub_category))+geom_smooth()+facet_wrap(~product_analytic_vertical)

######################## Read Product List information ########################
# product_list <- read_excel("Media data and other information.xlsx", sheet = "Product List")
product_list <- read.xls("Media data and other information.xlsx", sheet = 1,  header = TRUE)
summary(product_list)
nrow(product_list)
#View(product_list)
product_list$Frequency <- as.numeric(product_list$Frequency)

######################## Read Media Investment information ########################
media_investment <- read.xls("Media data and other information.xlsx", sheet = 2, header = TRUE,skip = 1)
#View(media_investment)
str(media_investment)

#Adjust col names
colnames(media_investment) <- c("Year","Month","Total.Investment","TV","Digital","Sponsorship","Content.Marketing","Online.marketing","Affiliates","SEM","Radio","Other")  

#Assign 0 for NA values in Radio
media_investment$Radio[which(is.na(media_investment$Radio))]  <- 0

#Assign 0 for NA values in Other
media_investment$Other[which(is.na(media_investment$Other))]  <- 0

#Remove the Total investments column 
media_investment <- subset(media_investment, select = -c(Total.Investment))
#View(media_investment)

######################## Exploratory Data Analysis ######################## 
## media investment by sub-category and month year    
media_investment_long <- media_investment %>% gather("Media Type","Invetsment",3:11)
ggplot(media_investment_long,aes(x=paste(Year,Month,sep = " "),y=Invetsment,fill=`Media Type`))+geom_bar(stat="identity")


######################## Read Special Sale Calendar information ########################
special_sale <- read.xls("Media data and other information.xlsx", sheet = 3, header = TRUE, stringsAsFactors = FALSE)
#View(special_sale)
str(special_sale)

#Modify column name as "Year"
colnames(special_sale)[colnames(special_sale) == "X"] <- "Year"

#Top 6 values of Year should be 2015
special_sale$Year[1:6] <- 2015

#Bottom 6 values of Year should be 2016
special_sale$Year[7:12] <- 2016

#Create column with sale_date
special_sale$sale_date <- sapply(special_sale$Sales.Calendar, function(val) trim(explode(val,sep="\\(")[2]))
special_sale$sale_date <- sapply(special_sale$sale_date,function(val) trim(explode(val,sep="\\)")[1]))

special_sale$start_month <- sapply(special_sale$sale_date,function(val) trim(explode(val,sep=" ")[2]))
special_sale$start_month <- sapply(special_sale$start_month,function(val) trim(explode(val,sep="'")[1]))
special_sale$start_month <- sapply(special_sale$start_month, function(val) match(val,month.abb))
special_sale$start_month[which(is.na(special_sale$start_month))] <- 7

special_sale$start_day <- sapply(special_sale$sale_date,function(val) trim(explode(val,sep=" ")[1]))
special_sale$start_day <- sapply(special_sale$start_day,function(val) trim(explode(val,sep="-")[1]))
special_sale$start_day <- sapply(special_sale$start_day,function(val) trim(explode(val,sep="th")[1]))

#Convert to Date Format
special_sale$start_date <- paste0(special_sale$Year,"-",special_sale$start_month,"-",special_sale$start_day)

special_sale$end_month <- sapply(special_sale$sale_date,function(val) trim(explode(val,sep=" ")[2]))
special_sale$end_month <- sapply(special_sale$end_month,function(val) trim(explode(val,sep="'")[1]))
special_sale$end_month[which(special_sale$end_month == "Dec")] <- "Jan"
special_sale$end_month <- sapply(special_sale$end_month, function(val) match(val,month.abb))
special_sale$end_month[which(is.na(special_sale$end_month))] <- 7

special_sale$end_day <- sapply(special_sale$sale_date,function(val) trim(explode(val,sep=" ")[1]))
special_sale$end_day <- sapply(special_sale$end_day,function(val) trim(explode(val,sep="-")[2]))
special_sale$end_day[which(is.na(special_sale$end_day))] <- 3
special_sale$end_day <- sapply(special_sale$end_day,function(val) trim(explode(val,sep="th")[1]))

#Convert to Date Format
special_sale$end_date <- paste0(special_sale$Year,"-",special_sale$end_month,"-",special_sale$end_day)

#Convert to date type
special_sale$start_date <- as.character(special_sale$start_date)
special_sale$start_date <- as.Date(special_sale$start_date)
special_sale$end_date <- as.character(special_sale$end_date)
special_sale$end_date <- as.Date(special_sale$end_date)

#Convert the start_date to start_week_number 
special_sale$sale_start_week <- strftime( special_sale$start_date ,format="%V")
special_sale$sale_start_week <- as.numeric(special_sale$sale_start_week)
#Convert the end_date to start_week_number 
special_sale$sale_end_week <- strftime( special_sale$end_date ,format="%V")
special_sale$sale_end_week <- as.numeric(special_sale$sale_end_week)

#Do the same manipulation on week number so as to inline them with consumerElectronics 
#week sequence convention
special_sale$sale_start_week<- ifelse(special_sale$Year==2016,special_sale$sale_start_week+26,special_sale$sale_start_week)
#Subtract 26 from the week numbers of 2015
special_sale$sale_start_week <- ifelse(special_sale$Year==2015,special_sale$sale_start_week-26,special_sale$sale_start_week)

special_sale$sale_end_week<- ifelse(special_sale$Year==2016,special_sale$sale_end_week+26,special_sale$sale_end_week)
#Subtract 26 from the week numbers of 2015
special_sale$sale_end_week <- ifelse(special_sale$Year==2015,special_sale$sale_end_week-26,special_sale$sale_end_week)
#Cover the special case of 25th Dec - 3rd Jan sale
special_sale$sale_end_week <- ifelse(special_sale$sale_end_week < 0, 27,special_sale$sale_end_week)

#Create column with sale_type
special_sale$sale_type <- sapply(special_sale$Sales.Calendar, function(val) trim(explode(val,sep="\\(")[1]))

#Clean the DF. Remove unnecessary columns
special_sale <- subset(special_sale, select = -c(Year,Sales.Calendar,sale_date,start_month,start_day,end_month,end_day))

#Remove the start_date and end_date columns
special_sale <- subset(special_sale, select = -c(start_date,end_date))

######################## Read Monthly NPS Score information ########################

nps_score_raw <- read.xls("Media data and other information.xlsx", sheet = 4, header = FALSE, stringsAsFactor = FALSE)
str(nps_score_raw)
#View(nps_score_raw)

#Flip the rows and columns
nps_score <- data.frame(t(nps_score_raw[-1]))
colnames(nps_score) <- nps_score_raw[,1]

#Remove NA values
nps_score <- na.omit(nps_score)

#Assign colname to the first column
colnames(nps_score)[1] <- "Date"
str(nps_score)

#Split Date into Year and Month
nps_score$Date <- as.character(nps_score$Date)
nps_score$Month <- sapply(nps_score$Date,function(val) trim(explode(val,sep="'")[1]))
nps_score$Year <- sapply(nps_score$Date,function(val) paste0("20",trim(explode(val,sep="'")[2])))

#Remove Date column
nps_score <- subset(nps_score,select=-c(Date))

#Convert month names to month numbers
nps_score$Month <- strtrim(nps_score$Month,3)
nps_score$Month <- match(nps_score$Month,month.abb)
#View(nps_score)

######################## Start merging all the datasets to form a master dataset ########################

# Merge tables media_investment and product_list
add_investment <- merge(media_investment,product_list)
colnames(add_investment)[colnames(add_investment) == "Products"] <- "product_analytic_vertical"
#View(add_investment)
colnames(add_investment)[colnames(add_investment) == "X"] <- "product_analytic_vertical"
colnames(add_investment)[colnames(add_investment) == "X.Affiliates"] <- "Affiliates"
add_investment <- add_investment[-c(which(add_investment$product_analytic_vertical=="\\N")),]

#Calculate the investment on different media for each product. 
#Value was in crore, converted to single units
add_investment$TV <- add_investment$TV * add_investment$Percent  /100
add_investment$Digital <- add_investment$Digital * add_investment$Percent  /100
add_investment$Sponsorship <- add_investment$Sponsorship * add_investment$Percent  /100
add_investment$Content.Marketing <- add_investment$Content.Marketing * add_investment$Percent  /100
add_investment$Online.marketing <- add_investment$Online.marketing * add_investment$Percent  /100
add_investment$Affiliates <- add_investment$Affiliates * add_investment$Percent  /100
add_investment$SEM <- add_investment$SEM * add_investment$Percent  /100
add_investment$Radio <- add_investment$Radio * add_investment$Percent  /100
add_investment$Other <- add_investment$Other * add_investment$Percent  /100

# Divide the values by 4 so as to get weekly data in add_investment
add_investment$TV <- add_investment$TV / 4
add_investment$Digital <- add_investment$Digital / 4
add_investment$Sponsorship <- add_investment$Sponsorship / 4
add_investment$Content.Marketing <- add_investment$Content.Marketing / 4
add_investment$Online.marketing <- add_investment$Online.marketing / 4
add_investment$Affiliates <- add_investment$Affiliates / 4
add_investment$SEM <- add_investment$SEM / 4
add_investment$Radio <- add_investment$Radio / 4
add_investment$Other <- add_investment$Other / 4
add_investment$Frequency <- add_investment$Frequency / 4
add_investment$Percent <- add_investment$Percent / 4

##Prepare AdStock KPIs
adstock_rate = 0.50

adstocked_advertising <- function(kpi)
{
  adstocked_advertising = numeric(length(kpi))
  adstocked_advertising[1] = kpi[1]
  for(i in 2:length(kpi)){
    adstocked_advertising[i] = kpi[i] + adstock_rate * kpi[i-1]
  }
  adstocked_advertising
}
add_investment$TV_adStock <- adstocked_advertising(add_investment$TV)
add_investment$Digital_adStock <- adstocked_advertising(add_investment$Digital)
add_investment$ContentMarketing_adStock <- adstocked_advertising(add_investment$Content.Marketing)
add_investment$OnlineMarketing_adStock <- adstocked_advertising(add_investment$Online.marketing)
add_investment$Affiliates_adStock <- adstocked_advertising(add_investment$Affiliates)
add_investment$SEM_adStock <- adstocked_advertising(add_investment$SEM)
add_investment$Radio_adStock <- adstocked_advertising(add_investment$Radio)
add_investment$Other_adStock <- adstocked_advertising(add_investment$Other)


##Dataset available to be merged:
#consumerElectronics
#add_investment
#special_sale
#nps_score

consolidated1 <- merge(consumerElectronics_weekly,add_investment,by=c("Year","Month","product_analytic_vertical"))
#View(consumerElectronics_weekly)
#View(consolidated1)

#No need of percent column anymore
consolidated1 <- subset(consolidated1, select=-c(Percent))

#Merge NPS score with the consolidated table
consolidated2 <- merge(consolidated1,nps_score,by=c("Month","Year"))

# Create sale_type column and add details
consolidated2$sale_type <- NULL
for(seq in 1:nrow(special_sale)) {
  for(week_no in special_sale[seq,1]:special_sale[seq,2]) {
    consolidated2[which(consolidated2$weekOrder_no==week_no),"sale_type"]  <-   special_sale[seq,3]
  }
}

#Convert NAs to "No Sale"
consolidated2$sale_type[which(is.na(consolidated2$sale_type))] <- "No Sale"
dummy_1 <- data.frame(model.matrix(~sale_type,data=consolidated2)) 
dummy_1 <-dummy_1[,-1] 
#View(dummy_1)

#Drop column sale_type
consolidated2 <- subset(consolidated2,select=-c(sale_type))
consolidated3 <- cbind(consolidated2,dummy_1)
#View(consolidated2)

#Drop the categories column as we are concerned with the sub categories
consolidated4 <- subset(consolidated3,select = -c(product_analytic_category))

#Drop column sale_type
#consolidated4 <- subset(consolidated4,select = -c(sale_type))

#Create dummy variables for product_analytic_vertical
dummy_2 <- data.frame(model.matrix(~product_analytic_vertical,data=consolidated4)) 
dummy_2 <-dummy_2[,-1]

#Combine dummy variables to the DF
consolidated4 <- subset(consolidated4,select=-c(product_analytic_vertical))
consolidated5 <- cbind(consolidated4,dummy_2)
#View(consolidated5)

#Convert NPS to numeric
consolidated5$NPS <- as.numeric(consolidated5$NPS)

########################## Exploratory Data Analysis ############################

## change this to include all adstocks
consolidated1_category_adstocks <- consolidated1[,c(5:7,29:34)]

## change this to include all columns till end
consolidated1_category_adstocks_long <- consolidated1_category_adstocks %>% gather("Media Type","Adstocks",4:9)
ggplot(consolidated1_category_adstocks_long,aes(x=sum_gmv,y=Adstocks))+geom_point()+geom_smooth()+facet_wrap(~`Media Type`)

## camera accessories adstocks by gmv & Media Type 
camera_adstocks <- subset(consolidated1_category_adstocks_long,product_analytic_sub_category=='CameraAccessory')
ggplot(camera_adstocks,aes(x=Adstocks,y=sum_gmv))+geom_point()+geom_smooth()+facet_wrap(~`Media Type`)

## gaming accessory adstocks by gmv & Media Type   
GamingAccessory_adstocks <- subset(consolidated1_category_adstocks_long,product_analytic_sub_category=='GamingAccessory')
ggplot(GamingAccessory_adstocks,aes(x=Adstocks,y=sum_gmv))+geom_point()+geom_smooth()+facet_wrap(~`Media Type`)

## Home Audio by gmv & Media Type   
HomeAudio_adstocks <- subset(consolidated1_category_adstocks_long,product_analytic_sub_category=='HomeAudio')
ggplot(HomeAudio_adstocks,aes(x=Adstocks,y=sum_gmv))+geom_point()+geom_smooth()+facet_wrap(~`Media Type`)

#Divide the master data frame into three categories
CameraAccessory <- consolidated5[which(consolidated5$product_analytic_sub_category=="CameraAccessory"),]
CameraAccessory <- subset(CameraAccessory,select = -c(product_analytic_sub_category))
#View(CameraAccessory)

HomeAudio <- consolidated5[which(consolidated5$product_analytic_sub_category=="HomeAudio"),]
HomeAudio <- subset(HomeAudio,select = -c(product_analytic_sub_category))
#View(HomeAudio)

GamingAccessory <- consolidated5[which(consolidated5$product_analytic_sub_category=="GamingAccessory"),]
GamingAccessory <- subset(GamingAccessory,select = -c(product_analytic_sub_category))
#View(GamingAccessory)



