#Loading nessesary packages
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyr)
library(Information)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(caTools)
library(caret)
library(ROCR)

#Loading Data
historical <- read.csv("historical_user_logs.csv",header = TRUE, na.strings = c("NA",""))
train <- read.csv("train.csv",header = TRUE, na.strings = c("NA",""))
test <- read.csv("test.csv",header = TRUE, na.strings = c("NA",""))

table(train$is_click)
# 0      1 
# 431960  31331 
# the target variablet is only 6.7% True for the whole data

################### exploring both the data ###########################
summary(historical)
summary(train)

########################## Checking the data for Quality Issues ######################
############# And Treating the Data for all the Quality issues Found #################
# from the summary, we find that the session id is the unique key for our dataset
# checking the session ID for duplicate values in train data
sum(duplicated(train$session_id))
# 0 indicte no duplicate values

# checking the historical dataset for the NA values
sapply(historical, function(x) sum(is.na(x)))
# historical data has no NA values

# checking the dataset for blank values
sapply(historical, function(x) sum( trimws(x) == "",na.rm = TRUE))
sapply(train, function(x) sum( trimws(x) == "",na.rm = TRUE))
# there are no blank values present

# checking the train dataset for the NA values
sapply(train, function(x) sum(is.na(x)))
# Few columns contain NA data
# user_group_id, age_level, user_depth have 18243 NA values each which is approximately 4% of the total data
# Removing 4% NA values which are present in that 3 columns
train <- train[complete.cases(train[, "user_group_id"]),]
sapply(train, function(x) sum(is.na(x)))
# product_category_2 has 351379 and city_development_index has 106886 NA values

# test <- test[complete.cases(test[, "user_group_id"]),]
# sapply(test, function(x) sum(is.na(x)))

# Checking the rows having NA values for both product_category_2 and city_development_index
sum(apply(train, MARGIN = 1, function(x) sum(is.na(x))) >= 2)
# 85877 rows contain NA values for both the columns which are 19% of the train data

# Using Chi-Square to find the relation between product_category_2 and product_category_1
chisq.test(table(train$product_category_1,train$product_category_2))
# Very low p-value is indicating high correlation between the two variables
# So Removing product_category_2 from the train data
train$product_category_2 <- NULL
test$product_category_2 <- NULL

# Now only city_development_index is left with 106886 NA values which is 24% of the total train data
summary(train$city_development_index)
table(train$city_development_index)
# We can replace the NA values with the median which is 2
train <- train %>% replace_na(list(city_development_index = 2))
test <- test %>% replace_na(list(city_development_index = 2))

############################################################################################
########################### Date Time treatment for the analysis ###########################
## Extract day of the week and hour from Datetime 
train$DateTime <- as.POSIXct(train$DateTime)
test$DateTime <- as.POSIXct(test$DateTime)

train$hour <- strftime(train$DateTime, format="%H")
test$hour <- strftime(test$DateTime, format="%H")

train$day <- weekdays(strptime(train$DateTime, format="%Y-%m-%d %H:%M:%S"))
test$day <- weekdays(strptime(test$DateTime, format="%Y-%m-%d %H:%M:%S"))

#removing Datetime column
train$DateTime <- NULL
test$DateTime <- NULL

########################### Treatment of User_id Variable #############################
# one way, the user_id can be converted to numeric according to their frequency in the data
# higher the frequency higher the user_id
dt = data.table(train)
train <- as.data.frame(dt[, `user_ID` := .N, by = user_id])
remove(dt)

dt_1 = data.table(test)
test <- as.data.frame(dt_1[, `user_ID` := .N, by = user_id])
remove(dt_1)

# dropping user_id column
train$user_id <- NULL
test$user_id <- NULL

########################### Analysis on Historical Data ###############################
## Bivariate analysis for product and the action taken
ggplot(historical, aes(historical$product, ..count..)) + geom_bar(aes(fill = historical$action), position = "dodge")
# Graph shows the the highest viewership and interest is taken in product H
# Then 2nd and 3rd highest viewership & interest is inproduct B and D
# Product A,C,F,G,I are at same level of viewership but no interest
# Product E shows few viewership and product J is almost null
# so providing product_rank index based on this action in train data
train$product_rank <- ifelse(train$product == 'H', 1,
                         ifelse(train$product == 'B', 2,
                                  ifelse(((train$product == 'A') | (train$product == 'D') | (train$product == 'C') | (train$product == 'F') | (train$product == 'G') | (train$product == 'I')), 3,
                                         ifelse(train$product == 'E', 4,
                                                ifelse(train$product == 'J', 5,6)))))


test$product_rank <- ifelse(test$product == 'H', 1,
                             ifelse(test$product == 'B', 2,
                                    ifelse(((test$product == 'A') | (test$product == 'D') | (test$product == 'C') | (test$product == 'F') | (test$product == 'G') | (test$product == 'I')), 3,
                                           ifelse(test$product == 'E', 4,
                                                  ifelse(test$product == 'J', 5,6)))))



############################################################################################
#################################### Classifying Hours #####################################
train$hour <- as.numeric(train$hour)
train$hours <- ifelse((train$hour>3) & (train$hour<=9),'Morning',
                      ifelse((train$hour>9) & (train$hour<=15),'Noon',
                        ifelse((train$hour>15) & (train$hour<=21),'Evening',
                          ifelse( ((train$hour>21) & (train$hour<=24)) | ((train$hour>=0) & (train$hour<=3)),'Night','Other'))))
train$hour <- NULL


test$hour <- as.numeric(test$hour)
test$hours <- ifelse((test$hour>3) & (test$hour<=9),'Morning',
                      ifelse((test$hour>9) & (test$hour<=15),'Noon',
                             ifelse((test$hour>15) & (test$hour<=21),'Evening',
                                    ifelse( ((test$hour>21) & (test$hour<=24)) | ((test$hour>=0) & (test$hour<=3)),'Night','Other'))))
test$hour <- NULL
############################################################################################
############# Segregating the data into categorical and continuous features ################
cat <- c("product","campaign_id","webpage_id","product_category_1"
         ,"user_group_id","gender","var_1","is_click","day","hours")
cont <- c("age_level","user_depth","city_development_index","user_ID","product_rank")
main_cat <- train[,cat]
main_cont <- train[,cont]

cat_test <- c("product","campaign_id","webpage_id","product_category_1"
         ,"user_group_id","gender","var_1","day","hours")
cont_test <- c("age_level","user_depth","city_development_index","user_ID","product_rank")

# before moving further, replacing all the NA values with 0
test$gender[is.na(test$gender)] <- "Male"
test[is.na(test)] <- 0

test_cat <- test[,cat_test]
test_cont <- test[,cont_test]
SID <- as.data.frame(test[,"session_id"])
colnames(SID)<- c("session_id")

# scaling the main continuous data
main_cont <- scale(main_cont)
test_cont <- scale(test_cont)

remove(cat,cont)

######### Checking Outliers in the continous variable ###############
# checking for any outliers in the continuous data using quantiles and boxplots
quantiles_df <- sapply(main_cont, 
                       function(x) quantile(x,seq(0,1,.1)))
summary(quantiles_df)

# converting categorical attributes to factor
main_cat<- data.frame(sapply(main_cat, function(x) factor(x)))
str(main_cat)
remove(quantiles_df)

test_cat <- data.frame(sapply(test_cat, function(x) factor(x)))
######################################################################################
############################## Univariate Analysis ###################################

# Histogram and Boxplots for continuous variables 
box_theme_x<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())

plot_fun_continuous <- function(cont_col_name,var_name){
  
  plot_grid(ggplot(main_cont, aes(cont_col_name))+ geom_histogram(binwidth = 10) +  labs(x = var_name),
            ggplot(main_cont, aes(x="",y=cont_col_name))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, 
            align = "v",ncol = 1)
  
}

plot_fun_continuous(main_cont$age_level,"Age")
plot_fun_continuous(main_cont$user_depth,"User Depth")
plot_fun_continuous(main_cont$city_development_index,"CDI")
plot_fun_continuous(main_cont$user_ID,"User ID Level")
plot_fun_continuous(main_cont$product_rank,"Product Rank")


# for categorical variables
univariate_cat <- function(col_name,var_name) {
  ggplot(main_cat,aes(x = col_name)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1))
}

plot_grid(univariate_cat(main_cat$product,"Product"),univariate_cat(main_cat$campaign_id,"Campaign"),
          univariate_cat(main_cat$webpage_id,"Webpage"),univariate_cat(main_cat$user_group_id,"User Grp ID"),align = "h")

plot_grid(univariate_cat(main_cat$gender,"Gender"),univariate_cat(main_cat$var_1,"var_1"),
          univariate_cat(main_cat$day,"Day"),univariate_cat(main_cat$hours,"Hours"),align = "h")


##### Some EDA Analysis outcome and pattern ####
# 1. Continous variable sees few outcomes in the Age, User Depth and Product Rank(can be ignored)
# 2. New User ID created based on frequency sees very high outliers as most are of very low frequency(can be ignored)
# 3. No outliers in the city development index
# 4. For  categorical variables specially among contains ID, Product-C,H,I , Campaign- 359520,405490,
      # Webpage-13787,60305 , User Grp ID -2,3 are far high in no. thaan others
# 5. Male are dominating in internet surfing than females
# 6. Data seems uniform thought the week
# 7. Evening and noon sees high user engagement than night

#####################################################################################
############################ WOE/IV analysis ############################

# following is the interpretation of the IV value
#< 0.02	useless for prediction
#0.02 to 0.1	Weak predictor
#0.1 to 0.3	Medium predictor
#0.3 to 0.5	Strong predictor

IV <- create_infotables(data=train, y="is_click", bins=10, parallel=T)
head(IV)

IV_table <- data.frame(IV$Summary)

IV_table_sorted <- IV_table[order(-IV_table$IV), ]

# plotting the IV values of variables in order of their significance of predictive power
ggplot(IV_table_sorted, aes(x = reorder(Variable,IV), y = IV))+
  geom_bar(width = 0.5, stat = "identity") +ggtitle("Information Value") +
  theme(plot.title = element_text(size = 10) ,axis.text.x = element_text(angle = 90))

# All the IDs column seems to be the good predictors 

#####################################################################################
################# Preparing the DataSet for Model Designing #########################

# removing the is_click from the main_cat data as we do not want a dummy for that
main_cat$is_click <- NULL

# creating the dummy variables
dummies<- data.frame(sapply(main_cat,function(x) data.frame(model.matrix(~x-1,data =main_cat))[,-1]))
test_dummies<- data.frame(sapply(test_cat,function(x) data.frame(model.matrix(~x-1,data =test_cat))[,-1]))

#taking the performance tag variable from main_data
is_click <- as.factor(train$is_click)

# creating the final data set
main_final <- cbind(is_click,dummies,main_cont)
test_final <- cbind(test_dummies,test_cont,SID)
remove(IV,IV_table,IV_table_sorted,main_cat,main_cont,plot_fun_continuous,univariate_cat,box_theme_x)

######################################################################################
################# Creating the Normal Test and Train Dataset #########################
set.seed(100)
backup_main <- train

trainindices= sample(1:nrow(main_final), 0.10*nrow(main_final))
train_sample = main_final[trainindices,]
model_test = main_final[-trainindices,]

train_smote <- SMOTE(is_click ~ ., train_sample, perc.over = 100, perc.under=200)
######################################################################################
######################### Logistic Regressing ########################################
# the very first model containing all the variables against target variable
model_1 <- glm(is_click~.,data=train_smote,family = "binomial")
summary(model_1)

# using STEPAIC to find to remove insignificant features
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
sort(vif(model_2))

# removing user_group_id.x3 based on high vif value
model_3 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + user_group_id.x1 + 
                 user_group_id.x10 + user_group_id.x11 + user_group_id.x12 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x6 + user_group_id.x7 + 
                 user_group_id.x8 + user_group_id.x9 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_3)
sort(vif(model_3))

# the vif value of all the variables is well within the range
# removing user_group_id.x12 based on p-value
model_4 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + user_group_id.x1 + 
                 user_group_id.x10 + user_group_id.x11 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x6 + user_group_id.x7 + 
                 user_group_id.x8 + user_group_id.x9 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_4)
sort(vif(model_4))

# removing user_group_id.x9 based on p-value
model_5 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + user_group_id.x1 + 
                 user_group_id.x10 + user_group_id.x11 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x6 + user_group_id.x7 + 
                 user_group_id.x8 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_5)
sort(vif(model_5))

# removing user_group_id.x6 based on p-value
model_6 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + user_group_id.x1 + 
                 user_group_id.x10 + user_group_id.x11 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x7 + 
                 user_group_id.x8 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_6)
sort(vif(model_6))

# removing user_group_id.x1 based on p-value
model_7 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + 
                 user_group_id.x10 + user_group_id.x11 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x7 + 
                 user_group_id.x8 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_7)
sort(vif(model_7))


# removing user_group_id.x10 based on p-value
model_8 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + 
                 user_group_id.x11 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x7 + 
                 user_group_id.x8 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_8)
sort(vif(model_8))


# removing user_group_id.x11 based on p-value
model_9 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x7 + 
                 user_group_id.x8 + var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_9)
sort(vif(model_9))


# removing user_group_id.x8 based on p-value
model_10 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                 product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                 campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                 campaign_id.x414149 + product_category_1.x5 + 
                 user_group_id.x2 + user_group_id.x4 + 
                 user_group_id.x5 + user_group_id.x7 + 
                 var_1 + day.xSunday + 
                 day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
               family = "binomial", data = train_smote)

summary(model_10)
sort(vif(model_10))


# removing user_group_id.x5 based on p-value
model_11 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                  campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  user_group_id.x7 + 
                  var_1 + day.xSunday + 
                  day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_11)
sort(vif(model_11))


# removing user_group_id.x7 based on p-value
model_12 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + campaign_id.x359520 + 
                  campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + day.xSunday + 
                  day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_12)
sort(vif(model_12))

# removing campaign_id.x359520 based on p-value
model_13 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x404347 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + day.xSunday + 
                  day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_13)
sort(vif(model_13))


# removing campaign_id.x404347 based on p-value
model_14 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + day.xSunday + 
                  day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_14)
sort(vif(model_14))


# removing day.xSunday based on p-value
model_15 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + 
                  day.xThursday + day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_15)
sort(vif(model_15))


# removing day.xThursday based on p-value
model_16 <- glm(formula = is_click ~ product.xC + product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + 
                  day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_16)
sort(vif(model_16))


# removing product.xC based on p-value
model_17 <- glm(formula = is_click ~ product.xD + product.xF + 
                  product.xH + product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + 
                  day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_17)
sort(vif(model_17))


# removing product.xH based on p-value
model_18 <- glm(formula = is_click ~ product.xD + product.xF + 
                  product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + 
                  day.xWednesday + user_ID + product_category_1.x4, 
                family = "binomial", data = train_smote)

summary(model_18)
sort(vif(model_18))


# removing product_category_1.x4 based on p-value
model_19 <- glm(formula = is_click ~ product.xD + product.xF + 
                  product.xI + campaign_id.x118601 + 
                  campaign_id.x360936 + campaign_id.x405490 + 
                  campaign_id.x414149 + product_category_1.x5 + 
                  user_group_id.x2 + user_group_id.x4 + 
                  var_1 + 
                  user_ID, 
                family = "binomial", data = train_smote)

summary(model_19)
sort(vif(model_19))

final_model <- model_19

# predicted probabilities of default for test data
test_pred = predict(final_model, type = "response",newdata = model_test[,-1])

# Let's see the summary 
summary(test_pred)
model_test$prob <- test_pred
View(model_test)

test_actual_def <- factor(ifelse(model_test$is_click==1,"Yes","No"))

#######################################################################
# finding the optimal cutoff value
perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
cutoff
# optimal cutoff obtained by tuning is 0.5047

test_cutoff_def <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 54.06%
# Sensitivity = 58.8%
# Specificity = 53.7%

################################# Model Evaluation##################################
########################## KS -statistic - Test Data ###############################

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.125
# KS- Stat value is 12.5%

# finding the area under the ROC curve
ROC <- performance(pred_object_test, measure = "auc")
area <- ROC@y.values[[1]]
area 
# 0.562

# plotting the ROC curve
tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model") +
  theme(axis.text.x=element_text(hjust=1))


####################################################################
# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_def, test_pred, groups = 10)
default_decile

# Plotting Gain Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_line(data=default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Logistic Model's Gain Chart")


# Plotting Lift Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+ geom_line(data=default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Logistic Model's Lift Chart")

##############################################################################################
# Performing the cross validation on the final Logistic Model
# Load data
# Select statistically significant variables
cross_data <- subset(main_final,select=c("Performance.Tag","Profession.xSE","No.of.times.30.DPD.or.worse.in.last.6.months",
                                         "Avgas.CC.Utilization.in.last.12.months","No.of.PL.trades.opened.in.last.12.months",
                                         "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."))

cross_data <- main_final

# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Number of iterations
k <- 500

# Accuracy
acc <- NULL
sens <- NULL
spec <- NULL
set.seed(100)

for(i in 1:k)
{
  # Train-test splitting
  # 80% of samples -> fitting
  # 20% of samples -> testing
  smp_size <- floor(0.80 * nrow(cross_data))
  index <- sample(seq_len(nrow(cross_data)),size=smp_size)
  train_cross <- cross_data[index, ]
  test_cross <- cross_data[-index, ]
  
  # Predict results
  results_prob <- predict(final_model_logistic,newdata = test_cross[,-1],type='response')
  
  # using the best cutoff for the probabilities
  results <- factor(ifelse(results_prob > cutoff,1,0),levels = 0:1)
  
  # Actual answers
  answers <- factor(test_cross$Performance.Tag,levels = 0:1)
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  acc[i] <- cm$overall[1]
  sens[i] <- cm$byClass[1]
  spec[i] <- cm$byClass[2]
  fpr[i] <- cm$table[2]/(nrow(cross_data)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(cross_data)-smp_size)
}

# Average accuracy,sensitivity and specificity of the model
mean(acc) # 66.9%
mean(sens) # 67.5%
mean(spec) # 54.9
par(mfcol=c(1,2))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Boxplot-Accuracy')

# plots of fpr and fnr
mean(fpr) #31.1%
mean(fnr) # 1.9%

hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

##########################################################################################
################# Predicting score for test dataset ###############################
#########################################################################################
test_final$is_click  <- predict(final_model, type = "response", newdata = test_final)
df_to_upload <- test_final[,c("session_id","is_click")]
write.csv(df_to_upload,"upload.csv",row.names=FALSE)
