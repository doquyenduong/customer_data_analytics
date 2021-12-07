# Packages -------------------------------------------------------

library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(caret)
library(stringr)

# Load data ------------------------------------------------------
setwd("~/SCHULE/Customer")

data_train <- purrr::map_dfr(
  list.files("data_hs21", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)
  )

data_test <- purrr::map_dfr(
  list.files("test", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)) %>% 
  rename(article_name = artikel_name)

# Encoding ----------------------------------------------------------------

encod <- function(x, var_name) {
  # Values
  vals <- unique(x)
  # Compare vector with value and add numerized result to df 
  res_enc <- purrr::map_dfc(vals, function(vals, x) as.numeric(x == vals), x = x)
  names(res_enc) <- paste0(var_name, c(1:length(vals)))
  return(res_enc)
}

data_train <- data_train %>% 
  mutate(big = as.numeric(stringr::str_detect(article_name, "500G"))) %>%
  mutate(mclas = as.numeric(stringr::str_detect(article_name, "M-CLAS"))) %>% 
  mutate(bio = as.numeric(stringr::str_detect(article_name, "BIO"))) %>% 
  #select(article, sales, year, month, week, starts_with("promo"), big, mclas, bio) %>%
  bind_cols(encod(data_train$category_1, "cat1")) %>% 
  bind_cols(encod(data_train$category_2, "cat2")) %>% 
  mutate(ID = 1:n())

data_test <- data_test %>% 
  mutate(big = as.numeric(stringr::str_detect(article_name, "500G"))) %>%
  mutate(mclas = as.numeric(stringr::str_detect(article_name, "M-CLAS"))) %>% 
  mutate(bio = as.numeric(stringr::str_detect(article_name, "BIO"))) %>% 
  #select(article, sales, year, month, week, starts_with("promo"), big, mclas, bio) %>%
  bind_cols(encod(data_test$category_1, "cat1")) %>% 
  bind_cols(encod(data_test$category_2, "cat2")) %>% 
  mutate(ID = 1:n())

# Auto arima ------------------------------------------------------

# Function to predict

aa_per_article <- function(article, x, from = data_test[1,"yrwk_start"],
                           to = data_test[nrow(data_test),"yrwk_start"]) {
  
  # Feedback
  cat(article, "\n")
  
  # Subset
  sales <- x %>% 
    filter(article_name == article) %>% 
    pull(sales)
  
  # Fit Model
  fit_cc <- auto.arima(sales, stepwise = F, allowdrift = F,
                       method = "ML")
  
  # Predict
  res <- tibble(
    article_name = article,
    year = 2021,
    yrwk_start = seq.Date(as.Date(from), as.Date(to), by = "weeks"),
    yrwk_end = yrwk_start + lubridate::days(6),
    week = lubridate::week(yrwk_start),
    month = lubridate::month(yrwk_start),
    sales = predict(fit_cc, n.ahead = length(yrwk_end))$pred)
  
  return(res)
}

# Go
data_pred_aa <- purrr::map_dfr(unique(data_train$article_name),
                               aa_per_article,
                               x = data_train)

# Plot time series incl. prediction
ggplot(data_train, mapping = aes(yrwk_start, sales, colour = article_name)) + 
  geom_line() + 
  geom_line(data = data_pred_aa, color = "black") +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  facet_wrap(vars(article_name), scales = "free", labeller=label_wrap_gen(width=18, multi_line=TRUE))

# Create csv file
write.csv(data_pred_aa, "sales_pred_aa.csv", row.names = FALSE)
