# Migros Challenge #
setwd("C:/Users/patri/OneDrive - Hochschule Luzern/Desktop/HSLU/CDA/Group project/data_hs21")
library(magrittr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xts)
library(tidyverse)
library(data.table)
library(caret)
library(forecast)
library(randomForest)
library(tidyquant)
library(readxl)

# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r

setwd("C:/Users/patri/OneDrive - Hochschule Luzern/Desktop/HSLU/CDA/Group project/data_hs21/test_data_challenge_1_fixed")

# Import Files new test data
df.yoghurt.new <-
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.)) 

names(df.yoghurt.new)[names(df.yoghurt.new) == "artikel_name"] <- "article_name"


glimpse(df.yoghurt.new)

setwd("C:/Users/patri/OneDrive - Hochschule Luzern/Desktop/HSLU/CDA/Group project/data_hs21")

# Import Files
df.yoghurt <-
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

colnames(df.yoghurt)[1] <- "week ID" 

glimpse(df.yoghurt)

# Join df.yoghurt and df.yoghurt.new
df.yoghurt <- rbind(df.yoghurt, df.yoghurt.new, fill = TRUE)


# Import Holiday information
sheet.names<- excel_sheets("holidays 2016-2021.xlsx")
sheet.names
list.all <- lapply(sheet.names, function(x) {as.data.frame(read_excel("holidays 2016-2021.xlsx", sheet = x)) } )
names(list.all) <- sheet.names  
df.holiday <- do.call(rbind.data.frame, list.all) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Type == "National Holiday") %>%
  filter(!(Day == "Sunday"))

glimpse(df.holiday)

# Data Preparation
df.yoghurt$quarter <- quarter(df.yoghurt$yrwk_start)
df.yoghurt$year <- as.factor(df.yoghurt$year)
df.yoghurt$quarter <- as.factor(df.yoghurt$quarter)
df.yoghurt$month <- as.factor(df.yoghurt$month)
df.yoghurt$article_name <- as.factor(df.yoghurt$article_name)
df.yoghurt$category_1 <- as.factor(df.yoghurt$category_1)
df.yoghurt$category_2 <- as.factor(df.yoghurt$category_2)
df.yoghurt$promo_01 <- as.factor(df.yoghurt$promo_01)
df.yoghurt$promo_02 <- as.factor(df.yoghurt$promo_02)
df.yoghurt$promo_03 <- as.factor(df.yoghurt$promo_03)
df.yoghurt$promo_04 <- as.factor(df.yoghurt$promo_04)
df.yoghurt$promo_05 <- as.factor(df.yoghurt$promo_05)
df.yoghurt$yrwk_start <- as.Date(df.yoghurt$yrwk_start, format = "%Y-%m-%d")
df.yoghurt$yrwk_end <- as.Date(df.yoghurt$yrwk_end, format = "%Y-%m-%d")
df.yoghurt$holidays[1:length(df.yoghurt$holidays)] <- as.numeric(0)
df.yoghurt$workdays[1:length(df.yoghurt$workdays)] <- as.numeric(6)
df.yoghurt[yrwk_start < ymd("2020-03-17"), coronameasures := "no pandemic"]
df.yoghurt[yrwk_start >= ymd("2020-03-17") & yrwk_start < ymd("2020-04-27"), coronameasures := "lockdown"]
df.yoghurt[yrwk_start >= ymd("2020-04-27") & yrwk_start < ymd("2020-12-23"), coronameasures := "no lockdown"]
df.yoghurt[yrwk_start >= ymd("2020-12-23") & yrwk_start < ymd("2021-03-01"), coronameasures := "lockdown"]
df.yoghurt[yrwk_start >= ymd("2021-03-01"), coronameasures := "no lockdown"]
df.yoghurt$coronameasures <- as.factor(df.yoghurt$coronameasures)
for (i in 2016:2022){
  df.yoghurt[yrwk_start >= ymd(as.Date(paste(i-1,"-12-01", sep = ""))) & yrwk_start < ymd(as.Date(paste(i,"-03-01", sep = ""))), season := "winter"]
  df.yoghurt[yrwk_start >= ymd(as.Date(paste(i,"-03-01", sep = ""))) & yrwk_start < ymd(as.Date(paste(i,"-06-01", sep = ""))), season := "spring"]
  df.yoghurt[yrwk_start >= ymd(as.Date(paste(i,"-06-01", sep = ""))) & yrwk_start < ymd(as.Date(paste(i,"-09-01", sep = ""))), season := "summer"]
  df.yoghurt[yrwk_start >= ymd(as.Date(paste(i,"-09-01", sep = ""))) & yrwk_start < ymd(as.Date(paste(i,"-12-01", sep = ""))), season := "autumn"]
  df.yoghurt$season <- as.factor(df.yoghurt$season)
}

glimpse(df.yoghurt)

# Add holiday information
for(i in 1:length(df.holiday)){
  df.yoghurt$holidays[as.numeric(df.yoghurt$yrwk_start) <= as.numeric(df.holiday$Date[i]) & as.numeric(df.yoghurt$yrwk_end) >= as.numeric(df.holiday$Date[i])] <- 1
}

df.yoghurt %>%
  mutate(workdays = workdays - holidays)

df.yoghurt$workdays <- as.integer(df.yoghurt$workdays)
df.yoghurt$holidays <- as.integer(df.yoghurt$holidays)

glimpse(df.yoghurt)


# Data Partitioning #
df.yoghurt[yrwk_start < ymd("2021-01-01"), partitioningtrain := "train"]
df.yoghurt$partitioningtrain = as.factor(df.yoghurt$partitioningtrain)
df.yoghurt[yrwk_start >= ymd("2020-12-25") & yrwk_start < ymd("2021-06-28"), partitioningtest := "test"]
df.yoghurt$partitioningtest = as.factor(df.yoghurt$partitioningtest)
df.yoghurt[yrwk_start >= ymd("2021-06-14"), partitioningpred := "prediction"]
df.yoghurt$partitioningpred = as.factor(df.yoghurt$partitioningpred)

set.seed(100) 
train = df.yoghurt[df.yoghurt$partitioningtrain == 'train',]
test = df.yoghurt[df.yoghurt$partitioningtest == 'test',]
pred.end2021 = df.yoghurt[df.yoghurt$partitioningpred == 'prediction',]

dim(train)
dim(test)
dim(pred.end2021)

# Model Evaluation Metrics - Mean Absolute Percentage Error
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

# Random Forest
set.seed(100)
rf = randomForest(sales ~ year + quarter + month + week + workdays + season +
                    article_name + category_1 + category_2 +
                    promo_01 + promo_02 + promo_03 + promo_04 + promo_05 +
                    coronameasures, data = train)

print(rf)

# Checking MAPE => Over-fitting evaluation
predictions = predict(rf, newdata = train)
mape(train$sales, predictions)

predictions = predict(rf, newdata = test)
mape(test$sales, predictions) 

# Assesment of variable importance
varImpPlot(rf)

# Revised Random Forest
set.seed(100)
rf.revised = randomForest(sales ~ article_name + category_2 + category_1 +
                            week + year + month + promo_01, data = train) 
print(rf.revised)

# Rechecking MAPE
predictions <- predict(rf.revised, newdata = train)
mape(train$sales, predictions)

predictions <- predict(rf.revised, newdata = test)
mape(test$sales, predictions) 
## Revised Random Forest II: eliminiation of quarter => MAPE gap increases
## Revised Random Forest III: eliminiation of quarter => MAPE gap increases
 
# Predict H2/2021
predictions <- predict(rf.revised, newdata = pred.end2021)
pred.end2021 <- cbind(pred.end2021, predictions)


# Plot time series incl. prediction
article_color <- c("#F37735", "#E7861B", "#CF9400", "#BB9D00", "#A3A500", "#72B000", "#39B600", "#00BE6C", "#00C19C", "#00BFC4", "#00B8E5", "#00B0F6", "#00A5FF", "#9590FF", "#BF80FF", "#E76BF3", "#FC61D5", "#FF65AE", "#FC717F")


lp <- ggplot() +
  geom_line(data = train, aes(x = yrwk_start, y = sales, colour = article_name)) +
  geom_line(data = test, aes(x = yrwk_start, y = sales, colour = article_name)) +
  geom_line(data = pred.end2021, aes(x = yrwk_start, y = predictions)) +
  scale_color_manual(name = "Articles with Prediction",
                     values = article_color)

#palette_light()[[2]]
#colour = palette_light()[[2]]
#"#E31A1C"

lp

lp + facet_grid(category_1 ~ .)

lp + facet_wrap (~ article_name, scales = "free")




# Prediction Random Forest (with all data)
df.yoghurt[yrwk_start < ymd("2021-06-28"), partitioningtrain := "train"]
df.yoghurt$partitioningtrain = as.factor(df.yoghurt$partitioningtrain)

set.seed(100) 
train = df.yoghurt[df.yoghurt$partitioningtrain == 'train',]
pred.end2021.new = df.yoghurt[df.yoghurt$partitioningpred == 'prediction',]


set.seed(100)
rf.revised = randomForest(sales ~ article_name + category_2 + category_1 +
                            week + year + month + promo_01, data = train) 
print(rf.revised)

# Rechecking MAPE
predictions <- predict(rf.revised, newdata = train)
mape(train$sales, predictions)

# Predict H2/2021
predictions <- predict(rf.revised, newdata = pred.end2021.new)
pred.end2021.new <- cbind(pred.end2021.new, predictions)


# Plot time series incl. prediction
article_color <- c("#F37735", "#E7861B", "#CF9400", "#BB9D00", "#A3A500", "#72B000", "#39B600", "#00BE6C", "#00C19C", "#00BFC4", "#00B8E5", "#00B0F6", "#00A5FF", "#9590FF", "#BF80FF", "#E76BF3", "#FC61D5", "#FF65AE", "#FC717F")


lp <- ggplot() +
  geom_line(data = train, aes(x = yrwk_start, y = sales, colour = article_name)) +
  geom_line(data = pred.end2021.new, aes(x = yrwk_start, y = predictions)) +
  scale_color_manual(name = "Articles with Prediction",
                     values = article_color)

lp

lp + facet_grid(category_1 ~ .)

lp + facet_wrap (~ article_name, scales = "free")


mape(pred.end2021$predictions, pred.end2021.new$predictions)

# Create csv file with prediction

write.csv(pred.end2021.new, "C:/Users/patri/OneDrive - Hochschule Luzern/Desktop/HSLU/CDA/Group project/prediction_alldata.csv", row.names = FALSE)
