library(dplyr)
library(lubridate)
library(caret)
library(stringr)
library(xgboost)
library(readxl)

##### Load Data #####
data <- purrr::map_dfr(
  list.files("../dataset/", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)
  )

data_actual <- purrr::map_dfr(
  list.files("../dataset/test/", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)) %>% 
  rename(article_name = artikel_name)


head(data_actual)

setwd("../dataset/lockdown_holiday_variables")

##### Import Covid Lockdown File #####
df.covid <- read_xlsx("covid_restrictions.xlsx")
df.covid$Date <- as.Date(df.covid$Date, format="%Y-%m-%d")
View(df.covid)

##### Add Covid Dummy Variable #####
covid.added <- df.covid[which(df.covid$`Restrictions added or reduced` == "added"),]
covid.reduced <- df.covid[which(df.covid$`Restrictions added or reduced` == "reduced"),]

restriction.added <- c()
restriction.reduced <- c()

for (i in 1:nrow(covid.added)) {
  a <- covid.added$Date[i]
  for (j in 1:nrow(data)) {
    b <- interval(data$yrwk_start[j], data$yrwk_end[j])
    if (a %within% b) {
      restriction.added <- c(restriction.added, j)
    }
  }
}

for (i in 1:nrow(covid.reduced)) {
  a <- covid.reduced$Date[i]
  for (j in 1:nrow(data)) {
    b <- interval(data$yrwk_start[j], data$yrwk_end[j])
    if (a %within% b) {
      restriction.reduced <- c(restriction.reduced, j)
    }
  }
}

restriction.added <- unique(restriction.added)
restriction.reduced <- unique(restriction.reduced)

data$covid_restriction_added <- NA
data$covid_restriction_added[restriction.added] <- 1

data$covid_restriction_reduced <- NA
data$covid_restriction_reduced[restriction.reduced] <- 1

data[is.na(data)] <- 0

data_actual$covid_restriction_added <- 0
data_actual$covid_restriction_reduced <- 0

##### Import Holidays File #####
df.holidays <- data.frame()

for (i in 1:length(c(2016:2021))) {
  holidays <- read_xlsx("holidays 2016-2021.xlsx", sheet=i, col_names=TRUE) 
  df.holidays <- bind_rows(df.holidays, holidays)
}

head(df.holidays)

df.holidays$Day <- factor(df.holidays$Day)
df.holidays$Date <- as.Date(df.holidays$Date, format="%Y-%m-%d")
df.holidays <- df.holidays[which(df.holidays$Type == "National Holiday"),]
glimpse(df.holidays)
View(df.holidays)
##### Add Holiday Dummy Variable #####
holiday <- c()

for (i in 1:nrow(df.holidays)) {
  a <- df.holidays$Date[i]
  for (j in 1:nrow(data)) {
    b <- interval(data$yrwk_start[j], data$yrwk_end[j])
    if(a %within% b) {
      holiday <- c(holiday, j)
    }
  }
}

holiday <- unique(holiday)


data$holidays <- NA
data$holidays[holiday] <- 1
data[is.na(data)] <- 0

holiday.new <- c()

for (i in 1:nrow(df.holidays)) {
  a <- df.holidays$Date[i]
  for (j in 1:nrow(data_actual)) {
    b <- interval(data_actual$yrwk_start[j], data_actual$yrwk_end[j])
    if(a %within% b) {
      holiday.new <- c(holiday.new, j)
    }
  }
}

holiday.new <- unique(holiday.new)

data_actual$holidays <- NA
data_actual$holidays[holiday.new] <- 1
data_actual[is.na(data_actual)] <- 0

View(data_actual)
# Data Analysis

boxplot(data$sales ~ data$holidays,xlab="Hoilday",ylab="Sales")
boxplot(data$sales ~ data$covid_restriction_added,xlab="Restriction Added",ylab="Sales")
boxplot(data$sales ~ data$covid_restriction_reduced,xlab="Restriction Reduced", ylab="Sales")

# LogSales
data$logSales <- log(data$sales)

View(data)
# Create train and test sets -> IMPORTANT split by date because there is time dependency
train <- data[data$yrwk_start < "2021-01-01",]
test <- data[data$yrwk_start >= "2021-01-01",]
View(train)

# Create simple Linear Regression Model
lm.mod1 <- lm(logSales ~ article_name + week + month + holidays + covid_restriction_added + covid_restriction_reduced, data = train[logSales > 0,])
summary(lm.mod1) #Multiple R-squared:  0.9087,	Adjusted R-squared:  0.9082 

#remove covid_restriction_added
lm.mod2 <- lm(logSales ~ article_name + week + month + holidays + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod2) #Multiple R-squared:  0.9082,	Adjusted R-squared:  0.9078 

#remove month
lm.mod2.1 <- lm(logSales ~ article_name + week + holidays + covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod2.1) #Multiple R-squared:  0.9087,	Adjusted R-squared:  0.9083 

#remove week
lm.mod2.2 <- lm(logSales ~ article_name + month + holidays + covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod2.2) #Multiple R-squared:  0.909,	Adjusted R-squared:  0.9085     

#add interaction between week and month 
lm.mod3 <- lm(logSales ~ article_name + week * month + holidays + covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod3) #Multiple R-squared:  0.909,	Adjusted R-squared:  0.9085 

#add interaction between month and holidays
lm.mod4 <- lm(logSales ~ article_name + week + month * holidays + covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod4) #Multiple R-squared:  0.9094,	Adjusted R-squared:  0.9089   

#add interaction between week and holidays
lm.mod5 <- lm(logSales ~ article_name + month + week * holidays + covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod5) #Multiple R-squared:  0.9093,	Adjusted R-squared:  0.9089 

#add interaction between holidays and covid_restriction_added
lm.mod6 <- lm(logSales ~ article_name + month + week + holidays * covid_restriction_added + covid_restriction_reduced, data = train[data$logSales > 0,])
summary(lm.mod6) #interaction NA

#add promos
lm.mod7 <- lm(logSales ~ article_name + week + month + holidays + covid_restriction_added + covid_restriction_reduced + promo_01 + promo_03 + promo_04 + promo_05, data = train[data$logSales > 0,])
summary(lm.mod7) #Promo_04 NA ; Multiple R-squared:  Multiple R-squared:  0.9142,	Adjusted R-squared:  0.9138  

#remove promo_04
lm.mod8 <- lm(logSales ~ article_name + week + month + holidays + covid_restriction_added + covid_restriction_reduced + promo_01 + promo_03 + promo_05, data = train[data$logSales > 0,])
summary(lm.mod8) #Multiple R-squared:  0.9142,	Adjusted R-squared:  0.9138

#add interactions for promos
lm.mod9 <- lm(logSales ~ article_name + week + month + holidays + covid_restriction_added + covid_restriction_reduced + promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod9) #Multiple R-squared:  0.9146,	Adjusted R-squared:  0.9141

#add interactions between holidays and promos
lm.mod10 <- lm(logSales ~ article_name + week + month + covid_restriction_added + covid_restriction_reduced + holidays * promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod10) #Multiple R-squared:  0.9148,	Adjusted R-squared:  0.9142 

#add interactions among holidays, promos and month
lm.mod11 <- lm(logSales ~ article_name + week + covid_restriction_added + covid_restriction_reduced + month *holidays * promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod11) #Multiple R-squared:  0.9157,	Adjusted R-squared:  0.9149 

#add interactions among week,holidays, promos and month
lm.mod12 <- lm(logSales ~ article_name + covid_restriction_added + month * covid_restriction_reduced + holidays * promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod12) #Multiple R-squared:  0.9151,	Adjusted R-squared:  0.9145 

#add interactions among covid_restriction_reduced, promos, month and article_name
lm.mod13 <- lm(logSales ~  week + covid_restriction_added + covid_restriction_reduced +article_name * month *holidays * promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod13) #Multiple R-squared:   0.9278,	Adjusted R-squared:  0.9153 

#add interactions among covid_restriction_reduced, promos, month, article_name and week
lm.mod14 <- lm( logSales ~  covid_restriction_added + covid_restriction_reduced +article_name * week * month *holidays * promo_01*promo_03*promo_05, data = train[data$logSales > 0,])
summary(lm.mod14) #Multiple R-squared:  0.9343,	Adjusted R-squared:  0.9282 


# Validate simple Model

#model 1
lm.mod1.pred <- predict(lm.mod1, newdata = test)
residuals.1 <- exp(lm.mod1.pred) - test$sales
ape.1 <- abs(residuals.1) / test$sales # Percentage Error
mean(ape.1) # Mean absolute percentage error 0.3885734

#model 2
lm.mod2.pred <- predict(lm.mod2, newdata = test)
residuals.2 <- exp(lm.mod2.pred) - test$sales
ape.2 <- abs(residuals.2) / test$sales # Percentage Error
mean(ape.2) #0.3858588

#model 10
lm.mod10.pred <- predict(lm.mod10, newdata = test)
residuals.10 <- exp(lm.mod10.pred) - test$sales
ape.10 <- abs(residuals.10) / test$sales # Percentage Error
mean(ape.10) # 0.394699

#model 11
lm.mod11.pred <- predict(lm.mod11, newdata = test)
residuals.11 <- exp(lm.mod11.pred) - test$sales
ape.11 <- abs(residuals.11) / test$sales # Percentage Error
mean(ape.11) #0.3854157

#model 13
lm.mod13.pred <- predict(lm.mod13, newdata = test)
residuals.13 <- exp(lm.mod13.pred) - test$sales
ape.13 <- abs(residuals.13) / test$sales # Percentage Error
mean(ape.13) #0.1261138


#model 14
lm.mod14.pred <- predict(lm.mod14, newdata = test)
residuals.14 <- exp(lm.mod14.pred) - test$sales
ape.14 <- abs(residuals.14) / test$sales # Percentage Error
mean(ape.14) # 0.1305829

test$sales_pred <- exp(lm.mod13.pred)

#Plot prediction for the test dataset
ggplot(test, mapping = aes(x= yrwk_start, sales, colour= article_name))+
  geom_line()+ 
  geom_line(data = test, mapping = aes(x= yrwk_start, sales_pred),color = "black") +
  facet_wrap(vars(article_name), scales = "free")


#Use all data as train
lm.mod.final <- lm(logSales ~  week + covid_restriction_added + covid_restriction_reduced +article_name * month *holidays * promo_01*promo_03*promo_05, data = data[data$logSales > 0,])
summary(lm.mod.final)

#Fit actual data to the model
lm.mod.pred.new <- predict(lm.mod.final, newdata = data_actual)
data_actual$sales <- exp(lm.mod.pred.new)

# Plot time series incl. prediction
ggplot(data, mapping = aes(x= yrwk_start, sales, colour= article_name))+
  geom_line()+ 
  geom_line(data = data_actual, color = "black") +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  facet_wrap(vars(article_name), scales = "free", labeller=label_wrap_gen(width=18, multi_line=TRUE))

# Create csv file
write.csv(data_actual,"../lm_prediction_agnes.csv", row.names = FALSE)
