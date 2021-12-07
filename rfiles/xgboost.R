# Packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(caret)
library(stringr)
library(xgboost)

# Load data ------------------------------------------------------

data <- purrr::map_dfr(
  list.files("data_hs21", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)
  )



# Encoding ----------------------------------------------------------------

encod <- function(x, var_name) {
  
  # Values
  vals <- unique(x)
  
  # Compare vector with value and add numerized result to df 
  res_enc <- purrr::map_dfc(vals, function(vals, x) as.numeric(x == vals), x = x)
  names(res_enc) <- paste0(var_name, c(1:length(vals)))
  
  return(res_enc)
  
}

data_enc <- data %>% 
  mutate(big = as.numeric(stringr::str_detect(article_name, "500G"))) %>%
  mutate(mclas = as.numeric(stringr::str_detect(article_name, "M-CLAS"))) %>% 
  mutate(bio = as.numeric(stringr::str_detect(article_name, "BIO"))) %>% 
  #select(article, sales, year, month, week, starts_with("promo"), big, mclas, bio) %>%
  bind_cols(encod(data$category_1, "cat1")) %>% 
  bind_cols(encod(data$category_2, "cat2")) %>% 
  mutate(ID = 1:n())


# Create train and test set ---------------------------------------

data_train <- data_enc[data_enc$yrwk_start < "2021-01-01",]
data_test <- data_enc[data_enc$yrwk_start >= "2021-01-01",]


# xgboost ---------------------------------------------------------

# Test set
#rows_test <- which(data_enc$year==2021)

data_xgb_train <- data_train  %>%
  select(-c(article_name, yrwk_start, yrwk_end, sales, category_1,
            category_2, big, mclas, bio, ID))
train_label <- data_train$sales
xgb_train <- xgb.DMatrix(as.matrix(data_xgb_train), label = train_label)

data_xgb_test <- data_test  %>%
  select(-c(article_name, yrwk_start, yrwk_end, sales, category_1,
            category_2, big, mclas, bio, ID))
test_label <- data_test$sales
xgb_test <- xgb.DMatrix(as.matrix(data_xgb_test), label = test_label)

# Model
params <- list(
  booster = "gbtree",
  objective = "count:poisson",
  eta = 0.1, 
  gamma = 0,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Calculating number of rounds
xgbcv <- xgb.cv(
  params = params, 
  data = xgb_train, 
  nrounds = 1000,
  nfold = 5, 
  showsd = T, 
  stratified = T, 
  print_every_n = 10, 
  early_stopping_rounds = 20, 
  maximize = F
)

# Model training
xgb1 <- xgb.train( 
  params = params, 
  data = xgb_train, 
  nrounds = 190, 
  watchlist = list(train = xgb_train, test = xgb_test),
  print_every_n = 10, 
  early_stopping_rounds = 10, 
  maximize = F, 
  eval_metric = "mape"
)

# Model prediction
xgbpred <- predict(xgb1, xgb_test)

data_pred_xgb <- data_pred_aa %>%
  mutate(sales = xgbpred)

# Validate xgboost
residuals <- data_pred_xgb$sales - data_test$sales
ape <- abs(residuals) / data_test$sales
mean(ape)

# Plot real sales data vs. prediction
ggplot(data_test, mapping = aes(yrwk_start, sales)) + 
  geom_line() + 
  geom_line(data = data_pred_xgb, color = "red") +
  facet_wrap(vars(article_name), scales = "free")

